structure MipsFrame : FRAME =
struct
  type register = string

  (* MIPS field *)
  val ZERO = Temp.newtemp()
  val AT = Temp.newtemp()
  val V0 = Temp.newtemp()
  val V1 = Temp.newtemp()
  val A0 = Temp.newtemp()
  val A1 = Temp.newtemp()
  val A2 = Temp.newtemp()
  val A3 = Temp.newtemp()
  val T0 = Temp.newtemp()
  val T1 = Temp.newtemp()
  val T2 = Temp.newtemp()
  val T3 = Temp.newtemp()
  val T4 = Temp.newtemp()
  val T5 = Temp.newtemp()
  val T6 = Temp.newtemp()
  val T7 = Temp.newtemp()
  val S0 = Temp.newtemp()
  val S1 = Temp.newtemp()
  val S2 = Temp.newtemp()
  val S3 = Temp.newtemp()
  val S4 = Temp.newtemp()
  val S5 = Temp.newtemp()
  val S6 = Temp.newtemp()
  val S7 = Temp.newtemp()
  val T8 = Temp.newtemp()
  val T9 = Temp.newtemp()
  val K0 = Temp.newtemp()
  val K1 = Temp.newtemp()
  val GP = Temp.newtemp()
  val SP = Temp.newtemp()
  val FP = Temp.newtemp()
  val RA = Temp.newtemp()

  val RV = V0

  val retregs = [V0, V1]
  val specialregs = [ZERO, AT, V0, V1, K0, K1, GP, SP, FP, RA]
  val argregs = [A0, A1, A2, A3]
  val callersaves = [T0, T1, T2, T3, T4, T5, T6, T7, T8, T9]
  val calleesaves = [S0, S1, S2, S3, S4, S5, S6, S7]

  val calldefs = [RA] @ retregs @ callersaves

  val registers: register list = [
      "$zero", "$at", "$v0", "$v1", "$k0", "$k1",
      "$gp", "$sp", "$fp", "$ra",
      "$a0", "$a1", "$a2", "$a3",
      "$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7", "$t8", "$t9",
      "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7"
    ]
  
  val temp_regs: register list = [
      "$a0", "$a1", "$a2", "$a3",
      "$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7", "$t8", "$t9",
      "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7"
  ]

  val tempRegNum = length temp_regs

  fun reg_eq (r1: register, r2: register) = r1 = r2
  
  fun reg_to_string r = r

  val tempMap: register Temp.Table.table = 
    let 
      fun helper ((regtmp, regname), table) = Temp.Table.enter(table, regtmp, regname)
      val regList = ListPair.zip(specialregs @ argregs @ callersaves @ calleesaves, registers)
    in
      foldl helper Temp.Table.empty regList
    end

  fun get_reg (t: Temp.temp) = 
    case Temp.Table.look(tempMap, t) of
      SOME r => r
      | NONE => raise Fail "get_reg: no such register"
  
  val wordSize = 4
  val numArgReg = List.length(argregs)

  datatype access = InFrame of int (* memory location at offset X from the frame pointer *)
  | InReg of Temp.temp

  type frame = {name: Temp.label, formals : access list, inFrameSize : int ref}

  datatype frag = PROC of {body: Tree.stm, frame: frame}
  | STRING of Temp.label * string

  fun newFrame (arg : {name: Temp.label, formals: bool list}) : frame = let
        val {name, formals} = arg
        val inFrameSize = ref 0
        fun getFormals (b, acc) = 
            let
              fun helper(b, (acc, inFrameNum, inRegNum)) = 
                  if b then (
                      inFrameSize := inFrameNum + 1;
                      (InFrame(~inFrameNum * wordSize)::acc, inFrameNum + 1, inRegNum)
                    ) 
                  else ((InReg(Temp.newtemp()))::acc, inFrameNum, inRegNum + 1)
              val (acc, _, _) = foldl helper ([], 0, 0) formals
            in acc end
      in
        {name = name, formals = getFormals(formals, []), inFrameSize = inFrameSize}
      end

  fun name (frm: frame) : Temp.label = let val {name, ...} = frm in name end

  fun formals (frm: frame) : access list = let val {formals, ...} = frm in formals end

  fun allocLocal (frm: frame) (onFrame : bool) : access = case onFrame of
        true => (
            #inFrameSize frm := !(#inFrameSize frm)+ 1; 
            InFrame(~(!(#inFrameSize frm)) * wordSize)
          )
      | false => InReg(Temp.newtemp()) 

  fun externalCall(s: string, args: Tree.exp list) = (
        Logger.log Logger.DEBUG ("external call: " ^ s); 
        Tree.CALL(Tree.NAME(Temp.namedlabel ("tig_" ^ s)), args)
      )
  (* Tree.CALL(Tree.NAME(Tree.Temp.namedlabel s), args) p165*)

  fun string(label: Tree.label, str: string):string = ".data\n" ^ 
    ".align 4\n" ^ 
    Symbol.name label ^ ":\n" ^ 
    "\t.word " ^ Int.toString (size str) ^ "\n" ^
    "\t.asciiz \"" ^ str ^ "\"\n"

  fun exp (InFrame offset) addr = Tree.READ(Tree.MEM(Tree.BINOP(Tree.PLUS, addr, Tree.CONST offset)))
    | exp (InReg temp) addr = Tree.READ(Tree.TEMP temp)

  fun loc (InFrame offset) addr = Tree.MEM(Tree.BINOP(Tree.PLUS, addr, Tree.CONST offset))
    | loc (InReg temp) addr = Tree.TEMP temp

  (* save/load registers before/after function call *)
  fun procEntryExit1 (frm: frame, body: Tree.exp) = 
      let val {name, formals, inFrameSize} = frm
        (* get memory for registers*) 
        val regs = [RA, FP] @ calleesaves
        val reglocs = map (fn r => loc(allocLocal frm true)(Tree.READ(Tree.TEMP SP))) (regs)


        (* load $a0-$a3 from formals *)
        (* save registers *)
        fun storeReg (r : Temp.temp, l : Tree.loc) = Tree.MOVE(l , Tree.READ (Tree.TEMP r))
        (* load registers *)
        fun loadReg (r : Temp.temp, l : Tree.loc) = Tree.MOVE(Tree.TEMP r, Tree.READ(l))

        val storeRegs = map (fn (r, acc) => storeReg(r, acc)) (ListPair.zip(regs, reglocs))
        val loadRegs = map (fn (r, acc) => loadReg(r, acc)) (ListPair.zip(regs, reglocs))
        (* save fp *)
        (* move sp to fp *)
        val saveFP = Tree.MOVE(loc(allocLocal frm true)(Tree.READ(Tree.TEMP SP)), Tree.READ(Tree.TEMP FP))
        val moveSP = Tree.MOVE(Tree.TEMP FP, Tree.READ(Tree.TEMP SP))
        (*decrement sp by inFrameSize*)
        val decSP = Tree.MOVE(Tree.TEMP SP, Tree.BINOP(Tree.MINUS, Tree.READ(Tree.TEMP SP), Tree.CONST (!inFrameSize * wordSize)))
        (*restore sp*)
        val restoreSP = Tree.MOVE(Tree.TEMP SP, Tree.READ(Tree.TEMP FP))
        (* restore fp *)
        val restoreFP = Tree.MOVE(Tree.TEMP FP, Tree.READ(Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.READ(Tree.TEMP SP), Tree.CONST 0))))
        val prelogue = [saveFP, moveSP, decSP]
        val epilogue = [restoreSP, restoreFP]  

        (* store $a0-$a3 to formals *)
        val _ = Logger.log Logger.DEBUG ("formals in funcEntryExis1: " ^ Int.toString (List.length formals));
        val storeArgs  = map (fn (formalloc, acc) => 
        Tree.MOVE((loc (formalloc) (Tree.READ(Tree.TEMP FP))), Tree.READ(Tree.TEMP (List.nth(argregs, acc))))) 
        (ListPair.zip(formals, List.tabulate(List.length formals, fn x => x)))

        (* seq *)
        fun seq [] = Tree.EXP(Tree.CONST 0)
          | seq (x::xs) = Tree.SEQ(x, seq xs)
      in
        seq([Tree.MOVE(Tree.TEMP RV,
        (Tree.ESEQ(seq ( storeRegs @ prelogue @ storeArgs ), body)))] 
        @  epilogue @ loadRegs)


      end

  (* This function appends a "sink" instruction to the function body to tell the register allocator that certain registers are live at procedure exit. *)
  fun procEntryExit2(frm) = 
  let
      val {name, formals, inFrameSize} = frm
      val decSP = Tree.MOVE(Tree.TEMP SP, Tree.BINOP(Tree.MINUS, Tree.READ(Tree.TEMP SP), Tree.CONST ((!inFrameSize + 1) * wordSize)))
  in
  decSP
  end
  (* Either proc-EntryExit2 should scan the body and record this information in some new component of the frame type, or procEntryExit3 should use the maximum legal value. *)
  fun procEntryExit3({name, formals, inFrameSize}:frame, body) =
      {prolog = "PROCEDURE " ^ Symbol.name name ^ "\n", 
        body = body, 
        epilog = "END " ^ Symbol.name name ^ "\n"}






end