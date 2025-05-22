import ProcTypes::*;
import Proc::*;
import Types::*;


typedef enum{Start, Run, Halt} TestState deriving (Bits, Eq);
typedef 300000 MaxCycle;

(*synthesize*)
module mkTestBench();
  Proc proc <- mkProc;

  Reg#(Bit#(32))     cycle  <- mkReg(0);
  Reg#(TestState)    tState <- mkReg(Start);

  //Initialize the PC and make it run
  rule start(tState == Start);
    proc.hostToCpu(32'h00000000); // start address
    tState <= Run;
  endrule

  rule countCycle(tState == Run);
    if (cycle == fromInteger(valueOf((MaxCycle)))) begin
      tState <= Halt;
    end
    else begin
      cycle <= cycle + 1;
    end
  endrule

  rule run(tState == Run);
    CpuToHostData cpuToHostData <- proc.cpuToHost;
    if(cpuToHostData.c2hType == ExitCode)
    begin

      $fwrite(stderr, "==================================\n");
      $fwrite(stderr, "Number of Cycles      : %d\n", cycle);
      $fwrite(stderr, "Executed Instructions : %d\n", cpuToHostData.numInst);

      if(cpuToHostData.data == 0)
        $fwrite(stderr, "Result                :     PASSED\n");
      else
        $fwrite(stderr, "Result                :     FAILED %d\n", cpuToHostData.data);
 
      $fwrite(stderr, "==================================\n");
      $finish;
    end
  endrule


  rule halt(tState == Halt);
     $fwrite(stderr, "Program Exceeded the maximum cycle %d\n", fromInteger(valueOf(MaxCycle)));
     $finish;
  endrule
endmodule
