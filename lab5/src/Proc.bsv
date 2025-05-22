import Types::*;
import ProcTypes::*;
import CMemTypes::*;
import RFile::*;
import IMemory::*;
import DMemory::*;
import Decode::*;
import Exec::*;
import CsrFile::*;
import Vector::*;
import Fifo::*;
import Ehr::*;
import GetPut::*;

typedef struct {
  Instruction inst;
  Addr pc;
  Addr ppc;
  Bool epoch;
} Fetch2Decode deriving(Bits, Eq);

typedef struct {
  DecodedInst dInst;
  Addr pc;
  Addr ppc;
  Bool epoch;
} Decode2Rest deriving(Bits, Eq);

(*synthesize*)
module mkProc(Proc);
  Reg#(Addr)    pc  <- mkRegU;
  RFile         rf  <- mkRFile;
  IMemory     iMem  <- mkIMemory;
  DMemory     dMem  <- mkDMemory;
  CsrFile     csrf <- mkCsrFile;

  Reg#(ProcStatus)   stat		<- mkRegU;
  Fifo#(1,ProcStatus) statRedirect <- mkBypassFifo;

  // Control hazard handling Elements
  Reg#(Bool) fEpoch <- mkRegU;
  Reg#(Bool) eEpoch <- mkRegU;

  Fifo#(1, Addr)         execRedirect <- mkBypassFifo;

  Fifo#(2, Fetch2Decode)  f2d <- mkPipelineFifo;
  Fifo#(2, Decode2Rest)  d2r <- mkPipelineFifo;

  Bool memReady = iMem.init.done() && dMem.init.done();
  rule test (!memReady);
    let e = tagged InitDone;
    iMem.init.request.put(e);
    dMem.init.request.put(e);
  endrule

  // rule doFetch(csrf.started && stat == AOK);
  //  	let inst = iMem.req(pc);
  //  	let ppc = pc + 4;

  //   if(execRedirect.notEmpty) begin
  //     execRedirect.deq;
  //     pc <= execRedirect.first;
  //     fEpoch <= !fEpoch;
  //   end
  //   else begin
  //     pc <= ppc;
  //   end

  //   f2d.enq(Fetch2Decode{inst:inst, pc:pc, ppc:ppc, epoch:fEpoch}); 
  //   $display("Fetch : from Pc %d , \n", pc);
  // endrule

  rule doFetch(csrf.started && stat == AOK);
    Addr addr = execRedirect.notEmpty ? execRedirect.first : pc;
    Bool tEpoch = execRedirect.notEmpty ? !fEpoch : fEpoch;

    if(execRedirect.notEmpty) begin
      execRedirect.deq;
      fEpoch <= !fEpoch;
    end
    
    let inst = iMem.req(addr);
    let ppc = addr + 4;
    pc <= ppc;

    f2d.enq(Fetch2Decode{inst:inst, pc:addr, ppc:ppc, epoch:tEpoch}); 
    $display("Fetch : from Pc %d , \n", pc);
  endrule

  rule doDecode(csrf.started && stat == AOK);
    let inst   = f2d.first.inst;
    let pc   = f2d.first.pc;
    let ppc    = f2d.first.ppc;
    let iEpoch = f2d.first.epoch;
    f2d.deq;

    // Decode 
    let dInst = decode(inst);
    d2r.enq(Decode2Rest{dInst:dInst, pc:pc, ppc:ppc, epoch:iEpoch}); 
    $display(fshow(dInst));
  endrule

  rule doRest(csrf.started && stat == AOK);
    let dInst   = d2r.first.dInst;
    let pc   = d2r.first.pc;
    let ppc    = d2r.first.ppc;
    let iEpoch = d2r.first.epoch;
    let iType = d2r.first.dInst.iType;
    d2r.deq;

    if(iEpoch == eEpoch) begin
      // Logging
      if (iType == J || iType == Jr || iType == Br) begin
        csrf.incInstTypeCnt(Ctr);
        case(iType)
          J  : csrf.incJumpInstTypeCnt(J);
          Jr : csrf.incJumpInstTypeCnt(Jr);
          Br : csrf.incJumpInstTypeCnt(Br);
        endcase
      end
      if (iType == Ld || iType == St)
        csrf.incInstTypeCnt(Mem);

      // Register Read 
      let rVal1 = isValid(dInst.src1) ? rf.rd1(validValue(dInst.src1)) : ?;
      let rVal2 = isValid(dInst.src2) ? rf.rd2(validValue(dInst.src2)) : ?;
      let csrVal = isValid(dInst.csr) ? csrf.rd(validValue(dInst.csr)) : ?;

      // Execute         
      let eInst = exec(dInst, rVal1, rVal2, pc, ppc, csrVal);       
      $display(fshow(eInst));        
      
      if(eInst.mispredict) begin
        eEpoch <= !eEpoch;
        execRedirect.enq(eInst.addr);
        csrf.incBPMissCnt();
        case(iType)
          J  : csrf.incMissInstTypeCnt(J);
          Jr : csrf.incMissInstTypeCnt(Jr);
          Br : csrf.incMissInstTypeCnt(Br);
        endcase
        $display("jump! :mispredicted, address %d ", eInst.addr);
      end

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

      //WriteBack 
      if (isValid(eInst.dst)) begin
          rf.wr(fromMaybe(?, eInst.dst), eInst.data);
      end
      csrf.wr(eInst.iType == Csrw ? eInst.csr : Invalid, eInst.data);
    end
      
	  /* Exercise_3 */
	  /* TODO:
	  1. count the number of each instruciton type
	    Ctr(Control)   : J, Jr, Br
	    Mem(Memory)    : Ld, St 
	        
	  2. count the number of mispredictions */    



	  /* Exercise_4 */
	  /* TODO:
	  1. Implement incInstTypeCnt method in CsrFile.bsv
	        
	  2. count number of mispredictions for each instruction type */    

  endrule

  rule upd_Stat(csrf.started);
	$display("Stat update");
  	statRedirect.deq;
    stat <= statRedirect.first;
  endrule

  method ActionValue#(CpuToHostData) cpuToHost;
    let retV <- csrf.cpuToHost;
    return retV;
  endmethod

  method Action hostToCpu(Bit#(32) startpc) if (!csrf.started && memReady);
    csrf.start(0);
    eEpoch <= False;
    fEpoch <= False;
    pc <= startpc;
    stat <= AOK;
  endmethod

  interface iMemInit = iMem.init;
  interface dMemInit = dMem.init;

endmodule
