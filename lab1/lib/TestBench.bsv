import BarrelShifterRight::*;
import Multiplexer::*;
import Rand::*;

(* synthesize *)
module mkTbMultiplexer();
  Reg#(Bit#(32)) cycle <- mkReg(0);
  Rand#(64) randomVal1 <- mkRand('hdeadbeef);
  Rand#(64) randomVal2 <- mkRand('hcafebabe);
  Rand#(1)   randomSel <- mkRand('hf01dab1e);

  rule test;
    if(cycle == 128)
    begin
      $display("PASSED");
      $finish;
    end
    else
    begin
      let val1 <- randomVal1.get;
      let val2 <- randomVal2.get;
      let sel <- randomSel.get;
      Bool isCorrect = True;
      let test = multiplexer64(sel, val1, val2);
      let realAns = sel == 0? val1: val2;
      for (Integer i = 0; i < 64; i = i + 1) begin
        if (test[i] != realAns[i]) begin
          isCorrect = False;
        end
      end
      if(!isCorrect)
      begin
        $display("FAILED Sel %b from %d, %d gave %d instead of %d", sel, val1, val2, test, realAns);
        $finish;
      end
      cycle <= cycle + 1;
    end
  endrule
endmodule

(* synthesize *)
module mkTbRightLogical();
  BarrelShifterRightLogical bsrl <- mkBarrelShifterRightLogical;

  Reg#(Bit#(32)) cycle <- mkReg(0);
  Rand#(64) randomVal <- mkRand('hdeadbeef);
  Rand#(6) randomShift <- mkRand('hcafebabe);

  rule test;
    if(cycle == 128)
    begin
      $display("PASSED");
      $finish;
    end
    else
    begin
      let val <- randomVal.get;
      let shift <- randomShift.get;
      Bool isCorrect = True;
      Bit#(64) implResultL <- bsrl.rightShift(val, shift);
      let trueResultL = val >> shift;
      for (Integer i = 0; i < 64; i = i + 1) begin
        if (implResultL[i] != trueResultL[i]) begin
          isCorrect = False;
        end
      end
      if(!isCorrect)
      begin
        $display("FAILED Logical Shift %b >> %d gave %b instead of %b", val, shift, implResultL, trueResultL);
        $finish;
      end
      cycle <= cycle + 1;
    end
  endrule
endmodule

(* synthesize *)
module mkTbRightArith();
  BarrelShifterRightArithmetic bsra <- mkBarrelShifterRightArithmetic;

  Reg#(Bit#(32)) cycle <- mkReg(0);
  Rand#(64) randomVal <- mkRand('hdeadbeef);
  Rand#(6) randomShift <- mkRand('hcafebabe);

  rule test;
    if(cycle == 128)
    begin
      $display("PASSED");
      $finish;
    end
    else
    begin
      let val <- randomVal.get;
      let shift <- randomShift.get;
      Bool isCorrect = True;
      Bit#(64) implResultA <- bsra.rightShift(val, shift);
      Int#(64) valNew = unpack(val);
      let trueResultA = pack(valNew >> shift);
      for (Integer i = 0; i < 64; i = i + 1) begin
        if (implResultA[i] != trueResultA[i]) begin
          isCorrect = False;
        end
      end
      if(!isCorrect)
      begin
        $display("FAILED Arithmetic Shift %b >> %d gave %b instead of %b", val, shift, implResultA, trueResultA);
        $finish;
      end
      cycle <= cycle + 1;
    end
  endrule
endmodule
