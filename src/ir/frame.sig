signature FRAME =
sig 
    type register = string
    val registers: register list
    val temp_regs: register list
    val tempRegNum: int
    val reg_eq: register * register -> bool
    val reg_to_string: register -> string

    val FP: Temp.temp
    val SP: Temp.temp
    val RV: Temp.temp

    val specialregs: Temp.temp list
    val argregs: Temp.temp list
    val retregs: Temp.temp list
    val callersaves: Temp.temp list
    val calleesaves: Temp.temp list

    val calldefs: Temp.temp list 

    val tempMap: register Temp.Table.table
    val get_reg: Temp.temp -> register
    val wordSize: int
    val externalCall: string * Tree.exp list -> Tree.exp
    type frame (* information about formal parameters and local variables allocated in this frame *)
    type access (* formals and locals that may be in the frame or in registers *)
    val newFrame : {name: Temp.label, formals: bool list} -> frame
    val formals : frame -> access list (* locations where the formal parameters are stored *)
    val name : frame -> Temp.label (* allocate a local variable in the frame *)
    val allocLocal : frame -> bool -> access 
    val string: Tree.label * string -> string
    val exp : access -> Tree.exp -> Tree.exp (* access and its stack frame' address *)
    val loc : access -> Tree.exp -> Tree.loc (* access and its stack frame' address *)

    val procEntryExit1 : frame * Tree.exp -> Tree.stm
    val procEntryExit2: frame * Assem.instr list -> Assem.instr list (* tell the register allocator that certain registers are live at procedure exit. *)
    val procEntryExit3 : frame * Assem.instr list -> {prolog: string, body: Assem.instr list, epilog: string}

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string
end