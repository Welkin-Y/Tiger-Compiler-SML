

structure MipsGen: CODEGEN = 
struct
  structure Frame = MipsFrame
  structure F = Frame
  structure T = Tree
  structure A = Assem

  fun codegen (frame: MipsFrame.frame) (irTree: T.stm): Assem.instr list = 
      (* Following is book example, we need to write our own based on MIPS grm *)
      let 
        val calldefs = F.calldefs
        val ilist = ref (nil: A.instr list)

        fun emit x= ilist := x :: !ilist
        and result(gen) = let val t = Temp.newtemp() in gen t; t end
        and munchStm(T.SEQ(a,b)) = (munchStm a; munchStm b)
          | munchStm(T.LABEL lab) =
            emit(A.LABEL{assem=(Symbol.name lab) ^ ":\n", lab=lab})
          | munchStm(T.JUMP(T.NAME lab, labs)) =
            emit(A.OPER{assem="\t" ^ "j\t" ^ (Symbol.name lab) ^ "\n",
                src=[], dst=[], jump=SOME(labs)})
          | munchStm(T.JUMP(e, _)) =
            emit(A.OPER{assem="\t" ^ "jr\t" ^ "`s0\n",
                src=[munchExp e], dst=[], jump=SOME[]})
          (* conditional branch *)
          | munchStm(T.CJUMP(T.EQ, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "beq\t" ^ "`s0, `s1, `j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.NE, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "bne\t" ^ "`s0, `s1, `j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.LT, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "blt\t" ^ "`s0, `s1, `j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.GT, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "bgt\t" ^ "`s0, `s1, `j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.LE, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "ble\t" ^ "`s0, `s1, `j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          
          | munchStm(T.CJUMP(T.GE, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "bge\t" ^ "`s0, `s1, `j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.ULT, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "bltu\t" ^ "`s0, `s1, `j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.UGT, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "bgtu\t" ^ "`s0, `s1, `j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.ULE, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "bleu\t" ^ "`s0, `s1, `j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.UGE, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "bgeu\t" ^ "`s0, `s1, `j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          (* store exp to loc *)
          (* save to mem *)
          | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) = 
            emit(A.OPER{assem="\t" ^ "sw\t" ^ "`s1, " ^ Int.toString(i) ^ "(`s0)\n",
                src=[munchExp e1, munchExp e2], 
                dst=[],jump=NONE})
          | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
            emit(A.OPER{assem="\t" ^ "sw\t" ^ "`s1, " ^ Int.toString(i) ^ "(`s0)\n",
                src=[munchExp e1, munchExp e2],
                dst=[],jump=NONE})
          | munchStm(T.MOVE(T.MEM(e1), T.READ(T.MEM(e2)))) =
            emit(A.OPER{assem="\t" ^ "lw\t" ^ "t0, " ^ "0(`s1)\n" ^ 
                "\t" ^ "sw\t" ^  "t0,\t" ^ "0(`s0)\n",
                src=[munchExp e1, munchExp e2],
                dst=[],jump=NONE})
          | munchStm(T.MOVE(T.MEM(T.CONST i), e2)) =
            emit(A.OPER{assem="\t" ^ "sw\t" ^ "`s1, " ^ Int.toString(i) ^ "(r0)\n",
                src=[munchExp e2], dst=[],jump=NONE})
          | munchStm(T.MOVE(T.MEM(e1), e2)) =
            emit(A.OPER{assem="\t" ^ "sw\t" ^ "`s1, " ^ "0(`s0)\n",
                src=[munchExp e1, munchExp e2],
                dst= [] ,jump=NONE})
          | munchStm(T.MOVE(T.TEMP i, e2) ) =
            emit(A.OPER{assem="\t" ^ "move\t" ^ "`d0, " ^ "`s0\n",
                src=[munchExp e2],
                dst=[i],jump=NONE})
          | munchStm (T.EXP e) = (munchExp e; ())
          (* | munchStm s = (
              print "FATAL: Cannot convert:\n ";
              Printtree.printtree(TextIO.stdOut, s);raise Fail "MipsGen: munchStm"
            ) *)

        and munchExp(T.READ(T.TEMP t)) = t
          (* load from mem *)
          | munchExp(T.READ(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)))) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "lw\t" ^  "`d0, " ^ Int.toString(i) ^ "(`s0)\n",
                    src =[munchExp e1], dst=[r], jump=NONE}))
          | munchExp(T.READ(T.MEM(T.BINOP(T.PLUS,T.CONST i,e1)))) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "lw\t" ^  "`d0, " ^ Int.toString(i) ^ "(`s0)\n",
                    src=[munchExp e1], dst=[r], jump=NONE}))
          | munchExp(T.READ(T.MEM(T.CONST i))) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "lw\t" ^  "`d0, " ^Int.toString(i) ^ "(r0)\n",
                    src=[], dst=[r], jump=NONE}))
          | munchExp(T.READ(T.MEM(e1))) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "lw\t" ^  "`d0, " ^ "0(`s0)\n",
                    src=[munchExp e1], dst=[r], jump=NONE}))
          (* binary ops *)
          | munchExp(T.BINOP(T.PLUS, e1, T.CONST i)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "addi\t" ^ "`d0, " ^ "`s0, " ^ Int.toString(i) ^ "\n",
                    src=[munchExp e1], dst=[r], jump=NONE}))
          | munchExp(T.BINOP(T.PLUS, T.CONST i, e1)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "addi\t" ^ "`d0, " ^ "`s0, " ^ Int.toString(i) ^ "\n",
                    src=[munchExp e1], dst=[r], jump=NONE}))
          | munchExp(T.BINOP(T.PLUS, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "add\t" ^ "`d0, " ^ "`s0, " ^ "`s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.MINUS, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "sub\t" ^ "`d0, " ^ "`s0, " ^ "`s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.MUL, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "mul\t" ^ "`d0, " ^ "`s0, " ^ "`s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          (* div $t1, $t2  #result in LO (quotient) and HI (remainder) *)
          (* mflo $t3      # Move quotient into $t3 *)
          (* mfhi $t4      # Move remainder into $t4 *)
          | munchExp(T.BINOP(T.DIV, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "div\t" ^ "`s0, " ^ "`s1\n" ^ 
                    "\t" ^ "mflo\t" ^ "`d0\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.AND, e1, T.CONST i)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^"andi\t" ^ "`d0, " ^ "`s0, " ^ Int.toString i ^ "\n",
                    src=[munchExp e1], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.AND, T.CONST i, e1)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^"andi\t" ^ "`d0, " ^ "`s0, " ^ Int.toString i ^ "\n",
                    src=[munchExp e1], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.AND, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^"and\t" ^ "`d0, " ^ "`s0, " ^ "`s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.OR, e1, T.CONST i)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "ori\t" ^ "`d0, " ^ "`s0, " ^ Int.toString i ^ "\n",
                    src=[munchExp e1], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.OR, T.CONST i, e1)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "ori\t" ^ "`d0, " ^ "`s0, " ^ Int.toString i ^ "\n",
                    src=[munchExp e1], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.OR, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "or\t" ^ "`d0, " ^ "`s0, " ^ "`s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.XOR, e1, T.CONST i)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "xori\t" ^ "`d0, " ^ "`s0, " ^ Int.toString i ^ "\n",
                    src=[munchExp e1], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.XOR, T.CONST i, e1)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "xori\t" ^ "`d0, " ^ "`s0, " ^ Int.toString i ^ "\n",
                    src=[munchExp e1], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.XOR, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "xor\t" ^ "`d0, " ^ "`s0, " ^ "`s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.LSHIFT, e1, T.CONST i)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "sll\t" ^ "`d0, " ^ "`s0, " ^ Int.toString i ^ "\n",
                    src=[munchExp e1], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.LSHIFT, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "sllv\t" ^ "`d0, " ^ "`s0, " ^ "`s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.RSHIFT, e1, T.CONST i)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "srl\t" ^ "`d0, " ^ "`s0, " ^ Int.toString i ^ "\n",
                    src=[munchExp e1], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.RSHIFT, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "srlv\t" ^ "`d0, " ^ "`s0, " ^ "s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.ARSHIFT, e1, T.CONST i)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "sra\t" ^ "`d0, " ^ "`s0, " ^ Int.toString i ^ "\n",
                    src=[munchExp e1], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.ARSHIFT, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "srav\t" ^ "`d0, " ^ "`s0, " ^ "`s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          
          (* func call *)
          | munchExp(T.CALL(T.NAME l, args)) = 
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "jal\t" ^ (Symbol.name l) ^ "\n",
                    src=munchArgs(0, args), 
                    dst=calldefs,
                    jump=NONE}))
          | munchExp(T.CALL(e, args)) = 
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "jalr\t" ^ "`s0\n",
                    src=munchExp e::munchArgs(0, args),
                    dst=calldefs,
                    jump=NONE}))
          (* load addr *)
          | munchExp(T.NAME lab) = result(fn r => emit(A.OPER
                  {assem="\t" ^ "la\t" ^ "`d0, " ^ (Symbol.name lab) ^ "\n",
                    src=[], dst=[r], jump=NONE}))
          (* load const *)
          | munchExp(T.CONST i) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "li\t" ^ "`d0, " ^ Int.toString(i) ^ "\n",
                    src=[], dst=[r], jump=NONE}))
          | munchExp e = (
              print "FATAL: Cannot convert:\n ";
              Printtree.printtree(TextIO.stdOut, T.EXP e);raise Fail "MipsGen: munchExp"
            )

          (* munchArgs generates code to move all the arguments to
          their correct positions, in outgoing parameter registers 
          and/or in memory. The integer parameter to munchArgs is / 
          for the ith argument; munchArgs will recur with / + 1 for 
          the next argument, and so on.
          What munchArgs returns is a list of all the temporaries 
          that are to be passed to the machine`s CALL instruction. 
          Even though these temps are never written explicitly in 
          assembly language, they should be listed as "sources" of
          the instruction, so that liveness analysis (Chapter 10) 
          can see that their values need to be kept up to the point 
          of call. *)
          (* TODO need check *)
        and munchArgs(i, args) =
            let
              fun handleArg(arg, idx: int) =
                  if idx < List.length(F.argregs) then (* move first four arguments to $a0-$a3. *)
                    let
                      val dstReg = case Temp.Table.look(F.tempMap, List.nth(F.argregs, idx)) of SOME reg => reg 
                        | NONE => raise Fail "arg reg not found"
                      val srcExp = munchExp arg
                    in
                      emit(A.OPER{assem = "\tmove\t" ^ dstReg ^ ", `s0\n", src = [srcExp], dst = [], jump = NONE})
                    end
                  else (* push others onto the stack. *)
                    let
                      val srcExp = munchExp arg
                      val offset = (idx - 4) * F.wordSize 
                    in
                      emit(A.OPER{assem = "\tsw\t" ^ "`s0, " ^ Int.toString(offset) ^ "(`s1)\n", src = [srcExp, F.SP], dst = [], jump = NONE})
                    end
            in
              ListPair.map handleArg (args,List.tabulate(List.length(args), fn x => x));
              F.argregs
            end
      in 
        munchStm irTree;
        rev(!ilist)
      end

end