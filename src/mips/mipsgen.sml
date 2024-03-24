

structure MipsGen: CODEGEN = 
struct
  structure Frame = MipsFrame
  structure F = Frame
  structure T = Tree
  structure A = Assem

  fun codegen (frame: MipsFrame.frame) (irTree: T.stm): Assem.instr list = 
      (* Following is book example, we need to write our own based on MIPS grm *)
      let 
        val calldefs = []
        val ilist = ref (nil: A.instr list)

        fun emit x= ilist := x :: !ilist
        and result(gen) = let val t = Temp.newtemp() in gen t; t end
        and munchStm(T.SEQ(a,b)) = (munchStm a; munchStm b)
          (* store exp to loc *)
          | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) = 
            emit(A.OPER{assem="sw\t" ^ "'s1,\t" ^ Int.toString(i) ^ "('s0)\n",
                src=[munchExp e1, munchExp e2], 
                dst=[],jump=NONE})
          | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)),e2)) =
            emit(A.OPER{assem="sw\t" ^ "'s1,\t" ^ Int.toString(i) ^ "('s0)\n",
                src=[munchExp e1, munchExp e2],
                dst=[],jump=NONE})
          | munchStm(T.MOVE(T.MEM(e1), T.READ(T.MEM(e2)))) =
            emit(A.OPER{assem="lw\t" ^ "t0,\t" ^ "0('s1)\n" ^ 
                "sw\t" ^  "t0,\t" ^ "0('s0)\n",
                src=[munchExp e1, munchExp e2],
                dst=[],jump=NONE})
          | munchStm(T.MOVE(T.MEM(T.CONST i), e2)) =
            emit(A.OPER{assem="sw\t" ^ "'s1,\t" ^ Int.toString(i) ^ "(r0)\n",
                src=[munchExp e2], dst=[],jump=NONE})
          | munchStm(T.MOVE(T.MEM(e1), e2)) =
            emit(A.OPER{assem="sw\t" ^ "'s1,\t" ^ "0('s0)\n",
                src=[munchExp e1, munchExp e2],
                dst= [] ,jump=NONE})
          | munchStm(T.MOVE(T.TEMP i, e2) ) =
            emit(A.OPER{assem="li\t" ^ "'d0,\t" ^ "'s0\n",
                src=[munchExp e2],
                dst=[i],jump=NONE})
          | munchStm(T.LABEL lab) =
            emit(A.LABEL{assem=(Symbol.name lab) ^ ":\n", lab=lab})
          | munchStm(T.JUMP(T.NAME lab, labs)) =
            emit(A.OPER{assem="JUMP " ^ (Symbol.name lab) ^ "\n",
                src=[], dst=[], jump=SOME(labs)})
          | munchStm(T.CJUMP(T.EQ,e1,e2,t,f)) =
            emit(A.OPER{assem="IF 's0 == 's1 JUMP 'j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.NE,e1,e2,t,f)) =
            emit(A.OPER{assem="IF 's0 != 's1 JUMP 'j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.LT,e1,e2,t,f)) =
            emit(A.OPER{assem="IF 's0 < 's1 JUMP 'j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.GT,e1,e2,t,f)) =
            emit(A.OPER{assem="IF 's0 > 's1 JUMP 'j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.LE,e1,e2,t,f)) =
            emit(A.OPER{assem="IF 's0 <= 's1 JUMP 'j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          
          | munchStm(T.CJUMP(T.GE,e1,e2,t,f)) =
            emit(A.OPER{assem="IF 's0 >= 's1 JUMP 'j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.ULT,e1,e2,t,f)) =
            emit(A.OPER{assem="IF 's0 < 's1 JUMP 'j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.UGT,e1,e2,t,f)) =
            emit(A.OPER{assem="IF 's0 > 's1 JUMP 'j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.ULE,e1,e2,t,f)) =
            emit(A.OPER{assem="IF 's0 <= 's1 JUMP 'j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm(T.CJUMP(T.UGE,e1,e2,t,f)) =
            emit(A.OPER{assem="IF 's0 >= 's1 JUMP 'j0\n",
                src=[munchExp e1, munchExp e2],
                dst=[], jump=SOME([t,f])})
          | munchStm (T.EXP e) = (munchExp e; ())
          | munchStm s = (
              print "FATAL: Cannot convert:\n ";
              Printtree.printtree(TextIO.stdOut, s);raise Fail "MipsGen: munchStm"
            )

        and  munchExp(T.READ(T.TEMP t)) = t
          | munchExp(T.READ(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)))) =
            result(fn r => emit(A.OPER
                  {assem="lw\t" ^  "'d0,\t" ^ Int.toString(i) ^ "('s0)\n",
                    src =[munchExp e1], dst=[r], jump=NONE}))
          | munchExp(T.READ(T.MEM(T.BINOP(T.PLUS,T.CONST i,e1)))) =
            result(fn r => emit(A.OPER
                  {assem="lw\t" ^  "'d0,\t" ^ Int.toString(i) ^ "('s0)\n",
                    src=[munchExp e1], dst=[r], jump=NONE}))
          | munchExp(T.READ(T.MEM(T.CONST i))) =
            result(fn r => emit(A.OPER
                  {assem="lw\t" ^  "'d0,\t" ^Int.toString(i) ^ "(r0)\n",
                    src=[], dst=[r], jump=NONE}))
          | munchExp(T.READ(T.MEM(e1))) =
            result(fn r => emit(A.OPER
                  {assem="lw\t" ^  "'d0,\t" ^ "0('s0)\n",
                    src=[munchExp e1], dst=[r], jump=NONE}))
          | munchExp(T.BINOP(T.PLUS,e1,T.CONST i)) =
            result(fn r => emit(A.OPER
                  {assem="ADDI 'd0 <- 's0+" ^ Int.toString(i) ^ "\n",
                    src=[munchExp e1], dst=[r], jump=NONE}))
          | munchExp(T.BINOP(T.PLUS,T.CONST i,e1)) =
            result(fn r => emit(A.OPER
                  {assem="ADDI 'd0 <- 's0+" ^ Int.toString(i) ^ "\n",
                    src=[munchExp e1], dst=[r], jump=NONE}))
          | munchExp(T.CONST i) =
            result(fn r => emit(A.OPER
                  {assem="ADDI 'd0 <- r0+"  ^ Int.toString(i) ^ "\n",
                    src=[], dst=[r], jump=NONE}))
          | munchExp(T.BINOP(T.PLUS,e1,e2)) =
            result(fn r => emit(A.OPER
                  {assem="ADD 'd0 <- 's0+'s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.MINUS,e1,e2)) =
            result(fn r => emit(A.OPER
                  {assem="ADD 'd0 <- 's0-'s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.MUL,e1,e2)) =
            result(fn r => emit(A.OPER
                  {assem="ADD 'd0 <- 's0*'s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.DIV,e1,e2)) =
            result(fn r => emit(A.OPER
                  {assem="ADD 'd0 <- 's0/'s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.AND,e1,e2)) =
            result(fn r => emit(A.OPER
                  {assem="ADD 'd0 <- 's0+'s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.OR,e1,e2)) =
            result(fn r => emit(A.OPER
                  {assem="ADD 'd0 <- 's0|'s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.XOR,e1,e2)) =
            result(fn r => emit(A.OPER
                  {assem="ADD 'd0 <- 's0^'s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.RSHIFT,e1,e2)) =
            result(fn r => emit(A.OPER
                  {assem="ADD 'd0 <- 's0>>'s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.LSHIFT,e1,e2)) =
            result(fn r => emit(A.OPER
                  {assem="ADD 'd0 <- 's0<<'s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          | munchExp(T.BINOP(T.ARSHIFT,e1,e2)) =
            result(fn r => emit(A.OPER
                  {assem="ADD 'd0 <- 's0>>'s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r],
                    jump=NONE}))
          
          
          | munchExp(T.CALL(e,args)) = (* TODO: save load staffs here *)
            result(fn r => emit(A.OPER
                  {assem="jal\t" ^ "'s0\n",
                    src=munchExp e::munchArgs(0,args),
                    dst=calldefs,
                    jump=NONE}))
          (* load addr *)
          | munchExp(T.NAME lab) = result(fn r => emit(A.OPER
                  {assem="la\t" ^ "'d0\t" ^ (Symbol.name lab) ^ "\n",
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