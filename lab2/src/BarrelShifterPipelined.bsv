import Multiplexer::*;
import FIFO::*;
import FIFOF::*;
import Vector::*;
import SpecialFIFOs::*;

/* Interface of the basic right shifter module */
interface BarrelShifterRightPipelined;
	method Action shift_request(Bit#(64) operand, Bit#(6) shamt, Bit#(1) val);
	method ActionValue#(Bit#(64)) shift_response();
endinterface

typedef union tagged {
    Tuple3#(Bit#(64), Bit#(6), Bit#(1)) V;
    void IV;
} SftRg deriving (Bits, Eq);

module mkBarrelShifterRightPipelined(BarrelShifterRightPipelined);
	/* use mkFIFOF for request-response interface.	*/
	let inFifo <- mkFIFOF;
	let outFifo <- mkFIFOF;
	Vector#(7, Reg#(SftRg)) regf <- replicateM(mkReg(IV));

	rule shiftFrt;
		if (inFifo.notEmpty()) begin
			inFifo.deq();
			regf[0] <= V(inFifo.first());
		end
		else regf[0] <= IV;
	endrule

	rule shiftTail;
		case (regf[6]) matches
			tagged V {.orand, .amt, .val}: outFifo.enq(orand);
		endcase
	endrule

	rule shift;
		/* 
			TODO: Implement a pipelined right shift logic. 
			You are allowed to make other rules to pipeline right shifter.
		*/
		/* Cycle 2개 손해보지만 알빠노 */
		for (Integer i=0;i<6;i=i+1) begin
			case (regf[i]) matches
				tagged V {.orand, .amt, .val}: begin
					if (amt[i] == 1'b1) begin
						Bit#(64) nrnd;
						for (Integer j=0;j<64-2**i;j=j+1)
							nrnd[j] = orand[j+2**i];
						for (Integer j=64-2**i;j<64;j=j+1)
							nrnd[j] = val;
						regf[i+1] <= V(tuple3(nrnd, amt, val));
					end
					else regf[i+1] <= regf[i];
				end
				tagged IV: regf[i+1] <= IV;
			endcase
		end
	endrule

	method Action shift_request(Bit#(64) operand, Bit#(6) shamt, Bit#(1) val);
		inFifo.enq(tuple3(operand, shamt, val));
	endmethod

	method ActionValue#(Bit#(64)) shift_response();
		outFifo.deq;
		return outFifo.first;
	endmethod
endmodule


/* Interface of the three shifter modules
 *
 * They have the same interface.
 * So, we just copy it using typedef declarations.
 */
interface BarrelShifterRightLogicalPipelined;
	method Action shift_request(Bit#(64) operand, Bit#(6) shamt);
	method ActionValue#(Bit#(64)) shift_response();
endinterface

typedef BarrelShifterRightLogicalPipelined BarrelShifterRightArithmeticPipelined;
typedef BarrelShifterRightLogicalPipelined BarrelShifterLeftPipelined;

module mkBarrelShifterLeftPipelined(BarrelShifterLeftPipelined);
	/* TODO: Implement left shifter using the pipelined right shifter. */
	let bsrp <- mkBarrelShifterRightPipelined;

	method Action shift_request(Bit#(64) operand, Bit#(6) shamt);
		bsrp.shift_request(reverseBits(operand), shamt, 1'b0);
	endmethod

	method ActionValue#(Bit#(64)) shift_response();
		Bit#(64) result <- bsrp.shift_response();
		return reverseBits(result);
	endmethod
endmodule

module mkBarrelShifterRightLogicalPipelined(BarrelShifterRightLogicalPipelined);
	/* TODO: Implement right logical shifter using the pipelined right shifter. */
	let bsrp <- mkBarrelShifterRightPipelined;

	method Action shift_request(Bit#(64) operand, Bit#(6) shamt);
		bsrp.shift_request(operand, shamt, 1'b0);
	endmethod

	method ActionValue#(Bit#(64)) shift_response();
		Bit#(64) result <- bsrp.shift_response();
		return result;
	endmethod
endmodule

module mkBarrelShifterRightArithmeticPipelined(BarrelShifterRightArithmeticPipelined);
	/* TODO: Implement right arithmetic shifter using the pipelined right shifter. */
	let bsrp <- mkBarrelShifterRightPipelined;

	method Action shift_request(Bit#(64) operand, Bit#(6) shamt);
		bsrp.shift_request(operand, shamt, operand[63]);
	endmethod

	method ActionValue#(Bit#(64)) shift_response();
		Bit#(64) result <- bsrp.shift_response();
		return result;
	endmethod
endmodule
