

structure MipsGen: CODEGEN = 
struct
  structure Frame = MipsFrame
  structure F = Frame
  structure T = Tree
  structure A = Assem

  fun codegen (frame: MipsFrame.frame) (irTree: T.stm): Assem.instr list = 
      (* Following is book example, we need to write our own based on MIPS grm *)
      let 
        val calldefs = F.retregs @ F.callersaves
        val ilist = ref (nil: A.instr list)

        fun emit x= ilist := x :: !ilist
        and result(gen) = let val t = Temp.newtemp() in gen t; t end
        and munchStm(T.SEQ(a,b)) = (munchStm a; munchStm b)
          | munchStm(T.LABEL lab) =
            emit(A.LABEL{assem=(Symbol.name lab) ^ ":\n", lab=lab})
          | munchStm(T.JUMP(T.NAME lab, labs)) =
            emit(A.OPER{assem="\t" ^ "j\t" ^ (Symbol.name lab) ^ "\n",
                src=[], dst=[], jump=SOME(labs)})
          (* conditional branch *)
          | munchStm(T.CJUMP(T.EQ, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "beq\t" ^ "'s0, 's1, t\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.NE, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "bne\t" ^ "'s0, 's1, t\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.LT, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "blt\t" ^ "'s0, 's1, t\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.GT, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "bgt\t" ^ "'s0, 's1, t\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.LE, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "ble\t" ^ "'s0, 's1, t\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          
          | munchStm(T.CJUMP(T.GE, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "bge\t" ^ "'s0, 's1, t\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.ULT, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "bltu\t" ^ "'s0, 's1, t\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.UGT, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "bgtu\t" ^ "'s0, 's1, t\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.ULE, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "bleu\t" ^ "'s0, 's1, t\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.UGE, e1, e2, t, f)) =
            emit(A.OPER{assem="\t" ^ "bgeu\t" ^ "'s0, 's1, t\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
                    (* store exp to loc *)
          (* save to mem *)
          | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) = 
            emit(A.OPER{assem="\t" ^ "sw\t" ^ "'s1, " ^ Int.toString(i) ^ "('s0)\n",
                src=[munchExp e1, munchExp e2], 
                dst=[],jump=NONE})
          | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
            emit(A.OPER{assem="\t" ^ "sw\t" ^ "'s1, " ^ Int.toString(i) ^ "('s0)\n",
                src=[munchExp e1, munchExp e2],
                dst=[],jump=NONE})
          | munchStm(T.MOVE(T.MEM(e1), T.READ(T.MEM(e2)))) =
            emit(A.OPER{assem="\t" ^ "lw\t" ^ "t0, " ^ "0('s1)\n" ^ 
                "\t" ^ "sw\t" ^  "t0,\t" ^ "0('s0)\n",
                src=[munchExp e1, munchExp e2],
                dst=[],jump=NONE})
          | munchStm(T.MOVE(T.MEM(T.CONST i), e2)) =
            emit(A.OPER{assem="\t" ^ "sw\t" ^ "'s1, " ^ Int.toString(i) ^ "(r0)\n",
                src=[munchExp e2], dst=[],jump=NONE})
          | munchStm(T.MOVE(T.MEM(e1), e2)) =
            emit(A.OPER{assem="\t" ^ "sw\t" ^ "'s1, " ^ "0('s0)\n",
                src=[munchExp e1, munchExp e2],
                dst= [] ,jump=NONE})
          | munchStm(T.MOVE(T.TEMP i, e2) ) =
            emit(A.OPER{assem="\t" ^ "move\t" ^ "'d0, " ^ "'s0\n",
                src=[munchExp e2],
                dst=[i],jump=NONE})
          | munchStm (T.EXP e) = (munchExp e; ())
          | munchStm s = (
              print "FATAL: Cannot convert:\n ";
              Printtree.printtree(TextIO.stdOut, s);raise Fail "MipsGen: munchStm"
            )

        and munchExp(T.READ(T.TEMP t)) = t
          (* load from mem *)
          | munchExp(T.READ(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)))) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "lw\t" ^  "'d0, " ^ Int.toString(i) ^ "('s0)\n",
                    src =[munchExp e1], dst=[r], jump=NONE}))
          | munchExp(T.READ(T.MEM(T.BINOP(T.PLUS,T.CONST i,e1)))) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "lw\t" ^  "'d0, " ^ Int.toString(i) ^ "('s0)\n",
                    src=[munchExp e1], dst=[r], jump=NONE}))
          | munchExp(T.READ(T.MEM(T.CONST i))) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "lw\t" ^  "'d0, " ^Int.toString(i) ^ "(r0)\n",
                    src=[], dst=[r], jump=NONE}))
          | munchExp(T.READ(T.MEM(e1))) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "lw\t" ^  "'d0, " ^ "0('s0)\n",
                    src=[munchExp e1], dst=[r], jump=NONE}))
          (* binary ops *)
          | munchExp(T.BINOP(T.PLUS, e1, T.CONST i)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "addi\t" ^ "'d0, " ^ "'s0, " ^ Int.toString(i) ^ "\n",
                    src=[munchExp e1], dst=[r], jump=NONE}))
          | munchExp(T.BINOP(T.PLUS, T.CONST i, e1)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "addi\t" ^ "'d0, " ^ "'s0, " ^ Int.toString(i) ^ "\n",
                    src=[munchExp e1], dst=[r], jump=NONE}))
          | munchExp(T.BINOP(T.PLUS, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "add\t" ^ "'d0, " ^ "'s0, " ^ "'s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.MINUS, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "sub\t" ^ "'d0, " ^ "'s0, " ^ "'s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.MUL, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "mul\t" ^ "'d0, " ^ "'s0, " ^ "'s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          (* div $t1, $t2  #result in LO (quotient) and HI (remainder) *)
          (* mflo $t3      # Move quotient into $t3 *)
          (* mfhi $t4      # Move remainder into $t4 *)
          | munchExp(T.BINOP(T.DIV, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "div\t" ^ "'s0, " ^ "'s1\n" ^ 
                  "\t" ^ "mflo\t" ^ "'d0\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.AND, e1, T.CONST i)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^"andi\t" ^ "'d0, " ^ "'s0, " ^ Int.toString i ^ "\n",
                    src=[munchExp e1], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.AND, T.CONST i, e1)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^"andi\t" ^ "'d0, " ^ "'s0, " ^ Int.toString i ^ "\n",
                    src=[munchExp e1], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.AND, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^"and\t" ^ "'d0, " ^ "'s0, " ^ "'s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.OR, e1, T.CONST i)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "ori\t" ^ "'d0, " ^ "'s0, " ^ Int.toString i ^ "\n",
                    src=[munchExp e1], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.OR, T.CONST i, e1)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "ori\t" ^ "'d0, " ^ "'s0, " ^ Int.toString i ^ "\n",
                    src=[munchExp e1], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.OR, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "or\t" ^ "'d0, " ^ "'s0, " ^ "'s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.XOR, e1, T.CONST i)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "xori\t" ^ "'d0, " ^ "'s0, " ^ Int.toString i ^ "\n",
                    src=[munchExp e1], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.XOR, T.CONST i, e1)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "xori\t" ^ "'d0, " ^ "'s0, " ^ Int.toString i ^ "\n",
                    src=[munchExp e1], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.XOR, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "xor\t" ^ "'d0, " ^ "'s0, " ^ "'s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.LSHIFT, e1, T.CONST i)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "sll\t" ^ "'d0, " ^ "'s0, " ^ Int.toString i ^ "\n",
                    src=[munchExp e1], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.LSHIFT, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "sllv\t" ^ "'d0, " ^ "'s0, " ^ "'s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.RSHIFT, e1, T.CONST i)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "srl\t" ^ "'d0, " ^ "'s0, " ^ Int.toString i ^ "\n",
                    src=[munchExp e1], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.RSHIFT, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "srlv\t" ^ "'d0, " ^ "'s0, " ^ "s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.ARSHIFT, e1, T.CONST i)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "sra\t" ^ "'d0, " ^ "'s0, " ^ Int.toString i ^ "\n",
                    src=[munchExp e1], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.ARSHIFT, e1, e2)) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "srav\t" ^ "'d0, " ^ "'s0, " ^ "'s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          
          (* func call *)
          | munchExp(T.CALL(T.NAME l, args)) = (* TODO: save load staffs here *)
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "jal\t" ^ (Symbol.name l) ^ "\n",
                    src=munchArgs(0, args), 
                    dst=calldefs,
                    jump=NONE}))
          | munchExp(T.CALL(e, args)) = (* TODO: save load staffs here *)
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "jalr\t" ^ "'s0\n",
                    src=munchExp e::munchArgs(0, args),
                    dst=calldefs,
                    jump=NONE}))
          (* load addr *)
          | munchExp(T.NAME lab) = result(fn r => emit(A.OPER
                  {assem="\t" ^ "la\t" ^ "'d0, " ^ (Symbol.name lab) ^ "\n",
                    src=[], dst=[r], jump=NONE}))
          (* load const *)
          | munchExp(T.CONST i) =
            result(fn r => emit(A.OPER
                  {assem="\t" ^ "li\t" ^ "'d0, " ^ Int.toString(i) ^ "\n",
                    src=[], dst=[r], jump=NONE}))
          | munchExp e = (
              print "FATAL: Cannot convert:\n ";
              Printtree.printtree(TextIO.stdOut, T.EXP e);raise Fail "MipsGen: munchExp"
            )

        and munchArgs(i,[]) = []
          | munchArgs(i,e::es) = munchExp(e)::munchArgs(i+1,es)
      in 
        munchStm irTree;
        rev(!ilist)
      end

end