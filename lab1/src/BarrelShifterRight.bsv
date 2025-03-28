import Multiplexer::*;

interface BarrelShifterRight;
  method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt, Bit#(1) shiftValue);
endinterface

module mkBarrelShifterRight(BarrelShifterRight);
  method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt, Bit#(1) shiftValue);
    /* TODO: Implement right barrel shifter using six multiplexers. */
    Bit#(64) res = val;
    Bit#(64) mask = signExtend(shiftValue);
    Bit#(64) temp;

    Integer k=1;
    for (Integer i=0; i<6; i=i+1) begin
      // res = multiplexer64(shiftAmt[i], res, {mask[63:64-k], res[63:k]});
      for (Integer j=0;j<64-k;j=j+1)
        temp[j] = res[j+k];
      for (Integer j=64-k;j<64;j=j+1)
        temp[j] = shiftValue;
      k = k+k;
      res = multiplexer64(shiftAmt[i], res, temp);
    end

    return res;
  endmethod
endmodule

interface BarrelShifterRightLogical;
  method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt);
endinterface

module mkBarrelShifterRightLogical(BarrelShifterRightLogical);
  let bsr <- mkBarrelShifterRight;
  method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt);
    Bit#(64) result <- bsr.rightShift(val, shiftAmt, 1'b0);
    return result;
  endmethod
endmodule

typedef BarrelShifterRightLogical BarrelShifterRightArithmetic;

module mkBarrelShifterRightArithmetic(BarrelShifterRightArithmetic);
  let bsr <- mkBarrelShifterRight;
  method ActionValue#(Bit#(64)) rightShift(Bit#(64) val, Bit#(6) shiftAmt);
    Bit#(64) result <- bsr.rightShift(val, shiftAmt, val[63]);
    return result;
  endmethod
endmodule
