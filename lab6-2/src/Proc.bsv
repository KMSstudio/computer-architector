import Types::*;
import ProcTypes::*;
import CMemTypes::*;
import MemInit::*;
import RFile::*;
import IMemory::*;
import DMemory::*;
import Decode::*;
import Exec::*;
import CsrFile::*;
import Fifo::*;
import GetPut::*;

typedef struct {
  Instruction inst;
  Addr pc;
  Addr ppc;
  Bool epoch;
} Fetch2Rest deriving(Bits, Eq);

typedef struct {
  DecodedInst dInst;
  Data rVal1;
  Data rVal2;
  Data csrVal;
  Addr pc;
  Addr ppc;
  Bool epoch;
} Decode2Execute deriving(Bits, Eq);

typedef struct {
  RIndx dst;
  Data rVal;
} Forward deriving(Bits, Eq);

(*synthesize*)
module mkProc(Proc);
  Reg#(Addr)    pc[2]  <- mkCRegU(2);
  RFile         rf  <- mkBypassRFile;  // Use the BypassRFile to handle the hazards. (wr < rd, Refer to M10.)
  //RFile         rf  <- mkRFile;
  IMemory     iMem  <- mkIMemory;
  DMemory     dMem  <- mkDMemory;
  CsrFile     csrf <- mkCsrFile;

  // BypassFifo for forwarding
  Fifo#(1, Forward) efw <- mkBypassFifo;
  Fifo#(1, RIndx) ebb <- mkBypassFifo;
  Fifo#(1, Forward) mfw <- mkBypassFifo;
  
  // The control hazard is handled using two Epoch registers and one BypassFifo.
  Reg#(Bool) fEpoch <- mkRegU;
  Reg#(Bool) eEpoch <- mkRegU;
  Fifo#(1, Addr) execRedirect <- mkBypassFifo; 
  
  // PipelineFifo to construct the two-stage pipeline (Fetch stage and Rest stage).
  Fifo#(1, Fetch2Rest)  f2d <- mkPipelineFifo;
  Fifo#(1, Decode2Execute) d2e <- mkPipelineFifo;
  Fifo#(1, ExecInst) e2m <- mkPipelineFifo;
  Fifo#(1, ExecInst) m2w <- mkPipelineFifo;

/* Lab 6-1: TODO) - Implement a 5-stage pipelined processor using the provided scoreboard.
                  - Refer to common-lib/scoreboard.bsv and the PowerPoint slides.
                  - Use the scoreboard interface properly. */
  
  rule doFetch(csrf.started);
   	let inst = iMem.req(pc[1]);
   	let ppc = pc[1] + 4;

    if(execRedirect.notEmpty) begin
      execRedirect.deq;
      pc[1] <= execRedirect.first;
      fEpoch <= !fEpoch;
    end
    else begin
      pc[1] <= ppc;
    end

    f2d.enq(Fetch2Rest{inst:inst, pc:pc[1], ppc:ppc, epoch:fEpoch}); 
    $display("Fetch    : pc = %d  ", pc[1], fshow(inst));
  endrule

  rule doDecode(csrf.started);
    // Decode
    let inst   = f2d.first.inst;
    let pc   = f2d.first.pc;
    let ppc    = f2d.first.ppc;
    let iEpoch = f2d.first.epoch;
    let dInst = decode(inst);
    let csrVal = isValid(dInst.csr) ? csrf.rd(validValue(dInst.csr)) : ?;
    
    let rVal1 = isValid(dInst.src1) ? rf.rd1(validValue(dInst.src1)) : ?;
    let rVal2 = isValid(dInst.src2) ? rf.rd2(validValue(dInst.src2)) : ?;
    if(mfw.notEmpty) begin
      if (mfw.first.dst == fromMaybe(0, dInst.src1)) rVal1 = mfw.first.rVal;
      if (mfw.first.dst == fromMaybe(0, dInst.src2)) rVal2 = mfw.first.rVal;
    end
    if(efw.notEmpty) begin
      if (efw.first.dst == fromMaybe(0, dInst.src1)) rVal1 = efw.first.rVal;
      if (efw.first.dst == fromMaybe(0, dInst.src2)) rVal2 = efw.first.rVal;
    end
    let stall = ebb.notEmpty() ? (ebb.first == fromMaybe(0, dInst.src1)) || (ebb.first == fromMaybe(0, dInst.src2)) : False;

    if (efw.notEmpty()) efw.deq();
    if (ebb.notEmpty()) ebb.deq();
    if (mfw.notEmpty()) mfw.deq();

    if (!stall) begin
        f2d.deq;
        if(iEpoch == fEpoch)
          d2e.enq(Decode2Execute{ dInst: dInst, rVal1: rVal1, rVal2: rVal2, csrVal: csrVal, pc: pc, ppc: ppc, epoch: iEpoch});
    end
    else (stall) begin if(iEpoch!=fEpoch || execRedirect.notEmpty) f2d.deq; end
    $display("Decode   : ");
  endrule

  rule doExecute(csrf.started);
    let dInst = d2e.first.dInst;
    let csrVal = d2e.first.csrVal;
    let pc_E = d2e.first.pc;
    let ppc = d2e.first.ppc;
    let iEpoch = d2e.first.epoch;
    let rVal1 = d2e.first.rVal1;
    let rVal2 = d2e.first.rVal2;

    let eInst = exec(dInst, rVal1, rVal2, pc_E, ppc, csrVal);
    d2e.deq;
    if(iEpoch == eEpoch) begin
      // Execute
      $display("Exec run : src1=$%d src2=$%d imm=%d dst=$%d  data = %d  ", dInst.src1, dInst.src2, dInst.imm, dInst.dst, eInst.dst, eInst.data);
      if(eInst.mispredict) begin
        eEpoch <= !eEpoch;
        pc[0] <= eInst.addr;
        execRedirect.enq(eInst.addr);
        $display("jump! :mispredicted, address %d ", eInst.addr);
      end
      
      if (fromMaybe(0, eInst.dst) != 0) efw.enq(Forward{ dst:fromMaybe(0, eInst.dst), rVal:eInst.data});
      if ((eInst.iType == Ld) && (fromMaybe(0, eInst.dst) != 0)) ebb.enq(fromMaybe(0, eInst.dst));
    end
    else begin
      $display("Exec skip: iEpoch = %d eEpoch = %d ", iEpoch, eEpoch);
      eInst.iType = Alu;
      eInst.dst = tagged Valid 0;
    end
    e2m.enq(eInst);
  endrule

  rule doMemory(csrf.started);
    let eInst = e2m.first;
    e2m.deq;

    //Memory 
    let iType = eInst.iType;
    case(iType)
      Ld :
      begin
        let d <- dMem.req(MemReq{op: Ld, addr: eInst.addr, data: ?});
        eInst.data = d;
      end

      St:
      begin
        let d <- dMem.req(MemReq{op: St, addr: eInst.addr, data: eInst.data});
      end
      Unsupported :
      begin
        $fwrite(stderr, "ERROR: Executing unsupported instruction\n");
        $finish;
      end
    endcase
    
    if ((fromMaybe(0, eInst.dst) != 0)) mfw.enq(Forward{ dst:fromMaybe(?, eInst.dst), rVal:eInst.data});
    m2w.enq(eInst);
  endrule

  rule doWriteBack(csrf.started);
    //WriteBack
    let eInst = m2w.first;
    m2w.deq;
    if (isValid(eInst.dst)) begin
      rf.wr(fromMaybe(?, eInst.dst), eInst.data);
    end
    csrf.wr(eInst.iType == Csrw ? eInst.csr : Invalid, eInst.data);
  endrule

  method ActionValue#(CpuToHostData) cpuToHost;
    let retV <- csrf.cpuToHost;
    return retV;
  endmethod

  method Action hostToCpu(Bit#(32) startpc) if (!csrf.started);
    csrf.start(0);
    eEpoch <= False;
    fEpoch <= False;
    pc[1] <= startpc;
  endmethod

  interface iMemInit = iMem.init;
  interface dMemInit = dMem.init;

endmodule
