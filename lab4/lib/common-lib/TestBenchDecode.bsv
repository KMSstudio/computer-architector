import ProcTypes::*;
import CMemTypes::*;
import MemInit::*;
import IMemory::*;
import Decode::*;
import Types::*;

typedef enum {Start, Run} State deriving (Bits, Eq);


(* synthesize *)
module mkTestBenchDecode();
  PrintDecode printD <- mkPrintDecode;

  Reg#(Bit#(32)) cycle <- mkReg(0);
  Reg#(Addr) pc <- mkRegU;
  Reg#(State)    state <- mkReg(Start);
  IMemory  iMem <- mkIMemory;


  Bool memReady = iMem.init.done;

  rule start(state == Start);
    pc <= 32'h0000;
    state <= Run;
  endrule

  rule countCycle(state == Run);
    cycle <= cycle + 1;
    $display("\ncycle %d\n", cycle);
  endrule

  rule getstaus(state == Run);
	  if(cycle == 30)
	  begin
		  $fwrite(stderr, "Decode Test End");
		  $fwrite(stderr, "\n");
		  $finish;
	  end
	  else
	  begin
		  let inst = iMem.req(pc); 
		  DecodedInst dInst = decode(inst);
		  printD.printDecode(dInst);
		  pc <= nextAddr(pc,getICode(inst));
	  end
  endrule
endmodule

interface PrintDecode;
  method Action printDecode( DecodedInst item );
endinterface

module mkPrintDecode(PrintDecode);
	method Action printDecode(DecodedInst dInst);
		let iTypeString = case (dInst.iType)
							  Alu : "Alu";
							  Ld : "Ld";
							  St : "St";
							  J : "J";
							  Jr : "Jr";
							  Br : "Br";
							  Csrr : "Csrr";
							  Csrw : "Csrw";
							  Unsupported : "Unsupported";
							  default : "not set";
						  endcase;
		let brFuncString = case (dInst.brFunc)
							Eq : "equal";
							Neq : "not equal";
							Lt : "less than";
							Ltu : "less than unsigned";
							Ge : "greater than or equal";
							Geu : "greater than or equal unsigned";
							AT : "anything";
							NT : "nothing";
						 endcase;

	    let aluFuncString = case(dInst.aluFunc)
								  Add : "Add";
								  Sub : "Sub";
								  And : "And";
								  Or : "Or";
								  Xor : "Xor";
								  Slt : "set less than";
								  Sltu : "set less than unsigned";
								  Sll : "shift left logical";
								  Sra : "shift right arithmetic";
								  Srl : "shift right logical";
						    endcase;

		$display("===========Instruction Decoding===========");
		$fwrite(stdout, "Instruction Type:\t\t");
    	$fwrite(stdout, iTypeString);
    	if(isValid(dInst.imm) && iTypeString == "Alu")
    	begin
			$fwrite(stdout, "\tImmediate");
    	end
    	$fwrite(stdout, "\nBranch condition:\t\t\t");
    	$fwrite(stdout, brFuncString);
		$fwrite(stdout, "\nALU function:\t\t\t");
   		$fwrite(stdout, aluFuncString);
		$fwrite(stdout, "\nDestination Register:\t");
		if(isValid(dInst.dst))
		begin
			$fwrite(stdout, "dst : %d\n", fromMaybe(?, dInst.dst));
		end
		else
		begin
			$fwrite(stdout, "dst : Invalid\n");
		end
		$fwrite(stdout, "Source Register A:\t\t");
		if(isValid(dInst.src1))
		begin
			$fwrite(stdout, "%d\n", fromMaybe(?, dInst.src1));
		end
		else
		begin
			$fwrite(stdout, "Invalid\n");
		end
		$fwrite(stdout, "Source Register B:\t\t");
		if(isValid(dInst.src2))
		begin
			$fwrite(stdout, "%d\n", fromMaybe(?, dInst.src2));
		end
		else
		begin
			$fwrite(stdout, "Invalid\n");
		end
		$fwrite(stdout, "Immidiate Value:\t\t");
		if(isValid(dInst.imm))
		begin
			$fwrite(stdout, "%d\n", fromMaybe(?, dInst.imm));
		end
		else
		begin
			$fwrite(stdout, "	Invalid\n");
		end
		if(isValid(dInst.csr))
		begin
			$fwrite(stdout, "\t\t\t\t\t\tcsr : %d\n", fromMaybe(?, dInst.csr));
			(fromMaybe(?, dInst.csr) == csrMhartid ? $fwrite(stdout, "end code\n") : $fwrite(stdout, "miss decode\n"));
		end
		else
		begin
			$fwrite(stdout, "\t\t\t\t\t\tcsr : Invalid\n");
		end
		$display("==========================================\n");
	endmethod
endmodule
