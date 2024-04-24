(* Called by Semant, Generate Tree represent IR*)
functor Translate (F: FRAME) : TRANSLATE =
struct
    structure Tr = Tree
    structure L = Logger
    
    datatype level = ROOT 
    | LEVEL of {parent : level, frame : F.frame, id: unit ref}

    type access = level * F.access
    type frag = F.frag
    val fragments : frag list ref = ref []

    val outermost = ROOT

    fun newLevel ({parent: level, name: Temp.label, formals: bool list}) = let 
                val () = L.log L.DEBUG ("create new level with formal length: " ^ Int.toString ((length formals) + 1))
            in LEVEL{parent = parent, frame = F.newFrame({name = name, formals = true::formals}), id = ref () } end

    fun formals(ROOT) = []
        | formals(LEVEL{frame, parent, id}) = map (fn acc => (LEVEL{frame=frame, parent=parent, id=id}, acc)) (F.formals frame) 

    fun allocLocal (LEVEL{frame, parent, id}) (escape) = (LEVEL{frame=frame, parent=parent, id=id}, F.allocLocal frame escape) (*TODO *)
        | allocLocal (ROOT) _ = raise ErrorMsg.impossible "allocLocal: no frame, cannot alloc at ROOT level"

    datatype exp = Ex of Tr.exp 
    | Nx of Tr.stm 
    | Cx of (Temp.label * Temp.label -> Tr.stm)
    | Lx of Tr.loc
    | NOT_IMPLEMENTED (* Placeholder for not implemented translations *)

    (* helper function for seq of exps *)
    fun seq [] = Tr.EXP(Tr.CONST 0)
        | seq [s] = s
        | seq (s::ss) = Tr.SEQ(s, seq ss)

    (* helper function for read exp from loc *)
    fun rdTmp x = Tr.READ(Tr.TEMP x)
    fun rdMem x = Tr.READ(Tr.MEM x)
    (* credit to: Dr. Drew Hilton *)

    fun unEx (Ex e) = e
        | unEx (Cx genstm) = 
            let val r = Temp.newtemp()
                val t = Temp.newlabel() and f = Temp.newlabel()
            in Tr.ESEQ(seq[
                        Tr.MOVE(Tr.TEMP r, Tr.CONST 1), 
                        genstm(t,f), 
                        Tr.LABEL f, 
                        Tr.MOVE(Tr.TEMP r, Tr.CONST 0), 
                        Tr.LABEL t],
                    rdTmp r) end
        | unEx (Nx s) = Tr.ESEQ(s, Tr.CONST 0) 
        | unEx (Lx l) = ( case l of Tr.MEM e => e
                                    | Tr.TEMP t => Tr.READ(Tr.TEMP t))
        | unEx NOT_IMPLEMENTED = raise ErrorMsg.impossible "unEx with NOT_IMPLEMENTED"

    fun unNx (Ex e) = Tr.EXP e
        | unNx (Cx genstm) = Tr.EXP (unEx (Cx genstm))
        | unNx (Nx s) = s
        | unNx (Lx l) = Tr.EXP (Tr.READ l)
        | unNx NOT_IMPLEMENTED = raise ErrorMsg.impossible "unNx with NOT_IMPLEMENTED"

    fun unCx (Ex(Tr.CONST 0)) = (fn (t,f) => Tr.JUMP(Tr.NAME f, [f]))
        | unCx (Ex(Tr.CONST 1)) = (fn (t,f) => Tr.JUMP(Tr.NAME t, [t]))
        | unCx (Ex e) = (fn (t,f) => Tr.CJUMP(Tr.NE, e, Tr.CONST 0, t, f))
        | unCx (Cx genstm) = genstm
        (* unCx(Nx _) need not be translated *)
        | unCx (Nx _) = (ErrorMsg.impossible "Cannot contruct conditional from Nx"; (fn _ => Tr.EXP(Tr.CONST 0)))
        | unCx (Lx l) = unCx(Ex(Tr.READ l))
        | unCx NOT_IMPLEMENTED = raise ErrorMsg.impossible "unCx with NOT_IMPLEMENTED"

    fun unLx (Lx l) = l
        | unLx (Ex e) = Tr.MEM e
        | unLx NOT_IMPLEMENTED = raise ErrorMsg.impossible "unLx with NOT_IMPLEMENTED"
        | unLx _ = raise ErrorMsg.impossible "unLx not supported type"

    fun procEntryExit({level: level, body: exp}) = 
            case level of
                ROOT => ErrorMsg.impossible "procEntryExit: no frame, cannot exit at ROOT level"
            | LEVEL{frame, parent, id} => 
                let
                    val body' = unEx(body)
                    val funcProc = F.PROC{body=Tr.EXP(body'), frame=frame}
                in
                    fragments := (!fragments)@[funcProc]
                end

    (* MEM(+(CONST kn, MEM(+(CONST kn-1, ... MEM(+(CONST k1, TEMP FP)) ...)))) *)
    fun followStaticLink (LEVEL{id=def_id, parent=def_prt, frame=def_frm}, LEVEL{id=use_id, parent=use_prt, frame=use_frm}): Tree.exp = 
            if def_id = use_id then rdTmp F.FP
            else (
                    L.log L.DEBUG "Follow use level";
                    F.exp (List.hd(F.formals use_frm))(followStaticLink(LEVEL{id=def_id, parent=def_prt, frame=def_frm}, use_prt))
                )
        | followStaticLink(ROOT, _) = let 
                val errmsg = "followStaticLink: define level is ROOT" 
            in L.log L.FATAL errmsg; ErrorMsg.impossible errmsg end
        | followStaticLink(_, ROOT) = let
                val errmsg = "followStaticLink: use level is ROOT" 
            in L.log L.FATAL errmsg; ErrorMsg.impossible errmsg end
    
    fun transNil () = Ex(Tr.CONST 0)
    
    fun simpleVar(access, useLevel) =
            let val (defLevel, defAccess) = access
                val _ = L.log L.DEBUG "Translate.simpleVar";
            in Lx(F.loc defAccess (followStaticLink(defLevel, useLevel))) end
       

    fun fieldVar (var, index) = 
    let 
        val indexReg = Temp.newtemp()
        val baseReg = Temp.newtemp()

    in
            Lx(Tr.MEM( Tr.ESEQ(
                seq([
                    Tr.MOVE(Tr.TEMP indexReg, Tr.CONST index),
                    Tr.MOVE(Tr.TEMP baseReg, unEx var)
                ]),
                Tr.BINOP(
                Tr.PLUS, 
                    rdTmp baseReg, 
                Tr.BINOP(
                    Tr.MUL, 
                    rdTmp indexReg, 
                    Tr.CONST F.wordSize)
                )
            )))
    end
            
    fun subscriptVar (arr, index) = 
            let 
                val indexReg = Temp.newtemp()
                val baseReg = Temp.newtemp()
                val errorLabel = Temp.newlabel()
                val succeeLabel = Temp.newlabel()
                val nextCheckLabel = Temp.newlabel()
            in 
                
                Lx(Tr.MEM(Tr.ESEQ(
                        seq[
                            Tr.MOVE(Tr.TEMP indexReg, unEx index),
                            Tr.MOVE(Tr.TEMP baseReg, unEx arr),
                            (*check bound*)
                            Tr.CJUMP(Tr.GT, Tr.CONST 0, 
                                rdTmp indexReg,
                                errorLabel,
                                nextCheckLabel
                                ),
                            Tr.LABEL nextCheckLabel,
                            Tr.CJUMP(Tr.GT, rdTmp indexReg, 
                                Tr.READ(Tr.MEM(
                                        Tr.BINOP(
                                            Tr.MINUS,
                                                rdTmp baseReg,
                                            Tr.CONST F.wordSize
                                            )
                                    )),
                                errorLabel,
                                succeeLabel
                                ),
                        
                            Tr.LABEL errorLabel,
                            Tr.EXP(F.externalCall("exit", [Tr.CONST 1])),
                            Tr.LABEL succeeLabel
                            (*indexing the array *)
                            
                        ], 
                            Tr.BINOP(
                                Tr.PLUS, 
                                    rdTmp baseReg, 
                                Tr.BINOP(
                                    Tr.MUL, 
                                    rdTmp indexReg, 
                                    Tr.CONST F.wordSize)
                                )
                )))
            end
    fun transVarDec (access, init) = 
            let 
                val (level, acc) = access
                val init = unEx init

            in
                Nx(Tr.MOVE(F.loc (acc) (rdTmp F.FP), init))
            end


    fun transLet (decs, body) = 
            let 
                val len = List.length decs
                val decs = map unNx decs
                val body = unEx body
            in
                if len = 0 then Ex(body)
                else Ex(Tr.ESEQ(seq decs, body))
            end
    fun transInt (num : int) = Ex(Tr.CONST num)

    fun transIf(cond, thenexp, elseexp) = 
            let 
                fun transIfThenElse(cond, thenexp, elseexp) = 
                        let 
                            val test = unCx(cond)
                            val thenexp = unEx(thenexp)
                            val elseexp = unEx(elseexp)
                            val res = Temp.newtemp()
                            val labelthen = Temp.newlabel()
                            val labelelse = Temp.newlabel()
                            val labelend = Temp.newlabel()
                        in
                            Ex(Tr.ESEQ(seq[
                                        test(labelthen, labelelse),
                                        Tr.LABEL labelthen,
                                        Tr.MOVE(Tr.TEMP res, thenexp),
                                        Tr.JUMP(Tr.NAME labelend, [labelend]),
                                        Tr.LABEL labelelse,
                                        Tr.MOVE(Tr.TEMP res, elseexp),
                                        Tr.LABEL labelend],
                                    rdTmp res))
                        end

                fun transIfThen(cond, thenexp) = 
                        let
                            val res = Temp.newtemp()
                            val t = Temp.newlabel()
                            val done = Temp.newlabel()
                        in
                            Nx (seq[unCx cond (t, done),
                                    Tr.LABEL t,
                                    unNx thenexp,
                                    Tr.LABEL done])
                        end
            in
                case elseexp of SOME elseexp => transIfThenElse(cond, thenexp, elseexp)
                | NONE => transIfThen(cond, thenexp)
            end

    fun transString(lit) = 
            let 
                val label = Temp.newlabel()
            in
                (* insert string into frags *)
                (* potential optimization: reuse the inserted string*)
                fragments := F.STRING(label, lit)::(!fragments);
                Ex(Tr.NAME label)
            end

    fun transBinop(oper, e1, e2) = Ex(Tr.BINOP(Tr.getBinop oper, unEx e1, unEx e2))

    fun transRelop(oper, e1, e2) = Cx(fn (t, f) => Tr.CJUMP(Tr.getRelop oper, unEx e1, unEx e2, t, f))

    fun transBreak(SOME(label)) = Nx(Tr.JUMP(Tr.NAME label, [label]))
        | transBreak(NONE) = Nx(Tr.EXP(Tr.CONST 0)) (*placeholder break label. even if there are no break in exp, we still need to pass a label*)

    fun transAssign(var, exp) = Nx(Tr.MOVE(unLx var, unEx exp)) 

    fun transCall (label, defLevel, callLevel, args) = case defLevel of 
                ROOT => Ex(F.externalCall(Symbol.name label, map unEx args))
            | LEVEL{frame, parent, id} => let
                    val static_link = followStaticLink(defLevel, callLevel)
                in
                    Ex(Tr.CALL(Tr.NAME label, static_link::(map unEx args)))
                end

    fun transLoop(cond, body, breakLabel) = 
            let
                val pretest = unCx(cond)
                val posttest = unCx(cond)
                val body = unNx(body)
                val bodylabel = Temp.newlabel()
                val endlabel = Temp.newlabel()
            in
                Nx(seq[
                        pretest(bodylabel, endlabel),
                        Tr.LABEL bodylabel,
                        body,
                        posttest(bodylabel, endlabel),
                        Tr.LABEL endlabel,
                        Tr.LABEL breakLabel]
                )
            end
    
    fun transWhile(cond, body, breakLabel) = transLoop(cond, body, breakLabel)

    fun transFor(var, lo, hi, body, breakLabel) = 
            let
                val assignVar = transAssign(var, lo)
                val test = transRelop(A.LeOp, var, hi)
                val plusone = unNx(transAssign(var, Ex(Tr.BINOP(Tr.PLUS, Tr.READ(unLx var), Tr.CONST 1))))
                val newbody = Nx(Tr.SEQ(unNx body, plusone))
            in
                Nx(Tr.SEQ(unNx assignVar, unNx(transLoop(test, newbody, breakLabel))))
            end
    
    fun transSeq [] = Ex(Tr.CONST 0)
        | transSeq [e] = Ex(unEx e)
        | transSeq (e::lst) = Ex(Tr.ESEQ(seq(map unNx (rev lst)), unEx e)) 

    fun transRecord(explist) =
            let
                val len = length explist
                val res = Temp.newtemp()
                val (expseq, _) = foldr (fn (exp, (expseq, index)) => 
                            (transAssign(fieldVar(Ex (rdTmp res), (len - index - 1)), exp)::expseq, index + 1)
                    ) ([], 0) explist
                val malloc = Tr.MOVE(Tr.TEMP res, F.externalCall("allocRecord", [Tr.CONST (len * F.wordSize)]))
            in
                Lx(Tr.MEM(Tr.ESEQ(Tr.SEQ(malloc, seq (map unNx expseq)), rdTmp res)))
            end
    
    fun transArray(size, init) = 
            let
                val size = unEx size
                val init = unEx init
                (* val init = case init of Ex e => e 
                | Lx l => (case l of Tr.MEM exp => exp 
                            | Tr.TEMP t => Tr.READ(Tr.TEMP t)) *)
      
                val res = Temp.newtemp()
                val initArr = Tr.MOVE(Tr.TEMP res, F.externalCall("initArray", [size, init]))
            in
                Lx(Tr.MEM(Tr.ESEQ(seq[initArr, Tr.MOVE(Tr.MEM(Tr.BINOP(Tr.MINUS, rdTmp res, Tr.CONST F.wordSize)), size)], rdTmp res)))
            end
    
    fun transFunDec (level, label: Temp.label , body : exp) = 
            let
                val {frame, ...} = case level of LEVEL level => level | ROOT => raise ErrorMsg.impossible "transFunDec: no frame"
                val funlabel = label
                val endlabel = Temp.newlabel()
                (*TODO 
                Maybe change transFunDec function params for needed info
                
                1. ( ) adjust the stack pointer (to allocate a new frame);
                2. ( ) save "escaping" arguments - including the static link - into the frame, and to move nonescaping arguments into fresh temporary registers;
                3. (✔) store instructions to save any callee-save registers - including the return address register - used within the function.
                *)
                val prologue = seq[Tr.JUMP(Tr.NAME endlabel, [endlabel]), 
                        Tr.LABEL funlabel] 
                
                val body = Nx(F.procEntryExit1(frame, unEx body))
                val body = unEx body

                (*TODO 
                1. (✔) move return value to RV 
                2. (✔ ) restore the callee-save registers
                3. ( ) an instruction to reset the stack pointer (to deallocate the frame);
                4. (✔) jr $ra 
                *)
                val epilogue = seq[Tr.EXP body, 
                        Tr.JUMP(rdTmp (List.nth(F.specialregs,9)), []), 
                        Tr.LABEL endlabel] 

                (* prologue and epilogue are ISA dependent, can we move them to MipsFrame? *)
            in
                (
                    (* Printtree.printtree(TextIO.stdOut, prologue); print "\n";
                    Printtree.printtree(TextIO.stdOut, epilogue); print "\n"; *)
                    Nx(Tr.SEQ(prologue, epilogue)))
            end

    fun transFunDecs (expfuncs) = 
            Nx(seq(map unNx expfuncs))
    

    fun getResult() = !fragments

end