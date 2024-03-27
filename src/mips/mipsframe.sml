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

  val tempMap: register Temp.Table.table = 
    let 
      fun helper ((regtmp, regname), table) = Temp.Table.enter(table, regtmp, regname)
      val regList = ListPair.zip(specialregs @ argregs @ callersaves @ calleesaves, registers)
    in
      foldl helper Temp.Table.empty regList
    end
  
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
                  if b orelse inRegNum = numArgReg then (
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
        Tree.CALL(Tree.NAME(Temp.namedlabel s), args)
      )
  (* Tree.CALL(Tree.NAME(Tree.Temp.namedlabel s), args) p165*)

  fun string(label: Tree.label, str: string):string = ".data\n" ^ ".align 2\n" ^ Symbol.name label ^ ":\n\t.asciiz \"" ^ str ^ "\"\n" (*TODO: GPT work, need consider escapes *)

  fun exp (access: access) (addr: Tree.exp) : Tree.exp = case access of
        InFrame(offset) => Tree.READ(Tree.MEM(Tree.BINOP(Tree.PLUS, addr, Tree.CONST offset)))
      | InReg(temp) => Tree.READ(Tree.TEMP temp)

  fun loc (access: access) (addr : Tree.exp) : Tree.loc = case access of
        InFrame(offset) => Tree.MEM(Tree.BINOP(Tree.PLUS, addr, Tree.CONST offset))
      | InReg(temp) => Tree.TEMP temp

  (* raise ErrorMsg.impossible "String Not Implemented" *)
  (* Tree.DATASTRING(label, str) *)

  (* save/load registers before/after function call *)
  fun procEntryExit1 (frm: frame, body: Tree.exp) = 
      let val {name, formals, inFrameSize} = frm
      (* get memory for registers*) 
      val reglocs = map (fn r => loc(allocLocal frm true)(Tree.CONST 0)) ([RA, SP, FP] @ calleesaves)
      (* save registers *)
      fun storeReg (r : Temp.temp, l : Tree.loc) = Tree.MOVE(l , Tree.READ (Tree.TEMP r))
      fun loadReg (r : Temp.temp, l : Tree.loc) = Tree.MOVE(Tree.TEMP r, Tree.READ(l))

      val storeRegs = map (fn (r, acc) => storeReg(r, acc)) (ListPair.zip([RA, SP, FP] @ calleesaves, reglocs))
      val loadRegs = map (fn (r, acc) => loadReg(r, acc)) (ListPair.zip([RA, SP, FP] @ calleesaves, reglocs))
      (* seq *)
      fun seq [] = Tree.EXP(Tree.CONST 0)
        | seq (x::xs) = Tree.SEQ(x, seq xs)
      in
        seq (storeRegs @ [Tree.EXP body] @ loadRegs)
      end

  fun procEntryExit2(frame, body) = body @
      [Assem.OPER{
          assem="",
          src =[ZERO, RA, SP] @ calleesaves,
          dst=[], jump=SOME[]
        }]

  fun procEntryExit3({name, formals, inFrameSize}:frame, body) =
      {prolog = "PROCEDURE " ^ Symbol.name name ^ "\n", 
        body = body, 
        epilog = "END " ^ Symbol.name name ^ "\n"}






end