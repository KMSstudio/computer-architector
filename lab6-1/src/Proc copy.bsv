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
import Scoreboard::*;
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

(*synthesize*)
module mkProc(Proc);
  Reg#(Addr)    pc[2]  <- mkCRegU(2);
  RFile         rf  <- mkBypassRFile;  // Use the BypassRFile to handle the hazards. (wr < rd, Refer to M10.)
  //RFile         rf  <- mkRFile;
  IMemory     iMem  <- mkIMemory;
  DMemory     dMem  <- mkDMemory;
  CsrFile     csrf <- mkCsrFile;
  
  // The control hazard is handled using two Epoch registers and one BypassFifo.
  Reg#(Bool) fEpoch <- mkRegU;
  Reg#(Bool) eEpoch <- mkRegU;
  Fifo#(1, Addr) execRedirect <- mkBypassFifo; 
  
  // PipelineFifo to construct the two-stage pipeline (Fetch stage and Rest stage).
  Fifo#(1, Fetch2Rest)  f2d <- mkPipelineFifo;
  Fifo#(1, Decode2Execute) d2e <- mkPipelineFifo;
  Fifo#(1, ExecInst) e2m <- mkPipelineFifo;
  Fifo#(1, ExecInst) m2w <- mkPipelineFifo;

  // Scoreboard instantiation. Use this module to address the data hazard. 
  // Refer to scoreboard.bsv in the 'common-lib' directory.
  Scoreboard#(4) sb <- mkPipelineScoreboard;


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
  endrule

  rule doDecode(csrf.started);
    let inst   = f2d.first.inst;
    let pc     = f2d.first.pc;
    let ppc    = f2d.first.ppc;
    let iEpoch = f2d.first.epoch;

    // Decode 
    let dInst = decode(inst);
    let stall = sb.search1(dInst.src1) || sb.search2(dInst.src2);
    if(!stall) begin
        f2d.deq;
        let rVal1 = isValid(dInst.src1) ? rf.rd1(validValue(dInst.src1)) : ?;
        let rVal2 = isValid(dInst.src2) ? rf.rd2(validValue(dInst.src2)) : ?;
        let csrVal = isValid(dInst.csr) ? csrf.rd(validValue(dInst.csr)) : ?;
        d2e.enq(Decode2Execute{
          dInst: dInst, rVal1: rVal1, rVal2: rVal2, csrVal: csrVal, 
          pc: pc, ppc: ppc, epoch: iEpoch});
        if(iEpoch==fEpoch && !execRedirect.notEmpty) sb.insert(dInst.dst);
        else sb.insert(tagged Invalid);
    end
    else begin
      if(iEpoch!=fEpoch || execRedirect.notEmpty) f2d.deq;
    end
    $display("Decode   : iEpoch = %d fEpoch = %d, stall = %d", iEpoch, fEpoch, stall, fshow(dInst));
  endrule

  rule doExecute(csrf.started);
    let dInst = d2e.first.dInst;
    let rVal1 = d2e.first.rVal1;
    let rVal2 = d2e.first.rVal2;
    let csrVal = d2e.first.csrVal;
    let pc_E = d2e.first.pc;
    let ppc = d2e.first.ppc;
    let iEpoch = d2e.first.epoch;
    
    let eInst = exec(dInst, rVal1, rVal2, pc_E, ppc, csrVal);
    d2e.deq;
    if(iEpoch == eEpoch) begin
      // Execute
      $display("Exec run : iEpoch = %d eEpoch = %d ", iEpoch, eEpoch, fshow(eInst));
      if(eInst.mispredict) begin
        eEpoch <= !eEpoch;
        pc[0] <= eInst.addr;
        execRedirect.enq(eInst.addr);
        $display("jump! :mispredicted, address %d ", eInst.addr);
      end
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
    sb.remove;
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
