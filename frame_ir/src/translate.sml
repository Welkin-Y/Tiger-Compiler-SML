(* Called by Semant, Generate Tree represent IR*)
structure Translate : TRANSLATE =
struct
    structure Tr = Tree
    structure Frame = MipsFrame
    structure F = Frame
    
    datatype level = ROOT 
    | LEVEL of {parent : level, frame : F.frame, id: unit ref}

    type access = level * F.access
    type frag = F.frag

    val outermost = ROOT

    fun newLevel ({parent: level, name: Temp.label, formals: bool list}) = 
            let val arg : {name: Temp.label, formals: bool list} = {name = name, formals = true::formals}
                val f = F.newFrame(arg)
            in LEVEL{parent = parent, frame = f, id = ref () } end

    fun formals(ROOT) = []
        | formals(LEVEL{frame, parent, id}) = map (fn acc => (LEVEL{frame=frame, parent=parent, id=id}, acc)) (F.formals frame) 

    fun allocLocal (LEVEL{frame, parent, id}) (escape) = (LEVEL{frame=frame, parent=parent, id=id}, F.allocLocal frame escape) (*TODO *)
        | allocLocal (ROOT) _ = raise ErrorMsg.impossible "allocLocal: no frame"

    datatype exp = Ex of Tr.exp 
    | Nx of Tr.stm 
    | Cx of (Temp.label * Temp.label -> Tr.stm)
    | Lx of Tr.loc
    | NOT_IMPLEMENTED



    fun procEntryExit({level: level, body: exp}) = raise Fail "TODO: procEntryExit"

    fun getResult() = raise Fail "TODO: getResult"

    (* helper function for seq of exps *)
    fun seq [] = Tr.EXP(Tr.CONST 0)
        | seq [s] = s
        | seq (s::ss) = Tr.SEQ(s, seq ss)

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
                    Tr.LOC(Tr.TEMP r)) end
        | unEx (Nx s) = Tr.ESEQ(s, Tr.CONST 0) 
        | unEx (Lx l) = Tr.LOC l

    fun unNx (Ex e) = Tr.EXP e
        | unNx (Cx genstm) = Tr.EXP (unEx (Cx genstm))
        | unNx (Nx s) = s
        | unNx (Lx l) = Tr.EXP (Tr.LOC l)

    fun unCx (Ex(Tr.CONST 0)) = (fn (t,f) => Tr.JUMP(Tr.NAME f, [f]))
        | unCx (Ex(Tr.CONST 1)) = (fn (t,f) => Tr.JUMP(Tr.NAME t, [t]))
        | unCx (Ex e) = (fn (t,f) => Tr.CJUMP(Tr.NE, e, Tr.CONST 0, t, f))
        | unCx (Cx genstm) = genstm
        (* unCx(Nx _) need not be translated *)
        | unCx (Nx _) = (ErrorMsg.impossible "Cannot contruct conditional from no results"; (fn _ => Tr.EXP(Tr.CONST 0)))
        | unCx (Lx _) = (ErrorMsg.impossible "Cannot contruct conditional from no results"; (fn _ => Tr.EXP(Tr.CONST 0)))

    (* MEM(+(CONST kn, MEM(+(CONST kn-1, ... MEM(+(CONST k1, TEMP FP)) ...)))) *)
    fun followStaticLink (LEVEL{id=def_id, parent=def_prt, frame=def_frm}, LEVEL{id=use_id, parent=use_prt, frame=use_frm}): Tree.exp =
            if def_id = use_id then Tr.LOC(Tr.MEM(Tr.BINOP(Tr.PLUS, Tr.CONST 0, Tr.LOC(Tr.TEMP F.FP))))
            else Tr.LOC(Tr.MEM(Tr.BINOP(Tr.PLUS, Tr.CONST 0, followStaticLink(LEVEL{id=def_id, parent=def_prt, frame=def_frm}, use_prt))))
        | followStaticLink(ROOT, _) = ErrorMsg.impossible "followStaticLink: no static link"
        | followStaticLink(_, ROOT) = ErrorMsg.impossible "followStaticLink: no static link"
    



    fun transNil () = Ex(Tr.CONST 0)
    fun simpleVar ((ROOT, _), _) = raise ErrorMsg.impossible "simpleVar: access variable in root level"
        | simpleVar((_, _), ROOT) = raise ErrorMsg.impossible "simpleVar: access root level"
        | simpleVar (access, lf) =
            let val (LEVEL{parent, frame, id}, access_g) = access
            val ref_g = id
            fun follow (LEVEL({parent, frame, id}) : level, cur_acc : Tr.loc) =
            let 
                val cur_ref = id
                in
                if ref_g = cur_ref
                then F.exp(access_g)(Tr.LOC cur_acc)
                else let val next_link = List.hd (F.formals frame)
                in follow (parent, F.exp(next_link)(Tr.LOC cur_acc))
                end
                end
            in Ex(Tr.LOC (follow(lf, Tr.TEMP(F.FP))))
            end
       

    fun fieldVar (var, index) = Ex(Tr.LOC (Tr.MEM(Tr.BINOP(Tr.PLUS, unEx var, Tr.CONST index))))
    fun subscriptVar (arr, index) = 
        let 
            val indexReg = Temp.newtemp()
            val baseReg = Temp.newtemp()
        in 
            Ex(Tr.ESEQ(seq[
                        Tr.MOVE(Tr.TEMP indexReg, unEx index),
                        Tr.MOVE(Tr.TEMP baseReg, unEx arr)],
                        Tr.LOC(Tr.MEM(Tr.BINOP(Tr.PLUS, Tr.LOC(Tr.MEM(Tr.LOC(Tr.TEMP baseReg))), Tr.BINOP(Tr.MUL, Tr.LOC(Tr.TEMP indexReg), Tr.CONST F.wordSize))))
            ))
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
                                    Tr.LOC(Tr.TEMP res)))
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
                (* TODO *)
                Ex(Tr.NAME label)
            end

    fun transBinop(oper, e1, e2) = Ex(Tr.BINOP(Tr.getBinop oper, unEx e1, unEx e2))

    fun transRelop(oper, e1, e2) = Cx(fn (t, f) => Tr.CJUMP(Tr.getRelop oper, unEx e1, unEx e2, t, f))

    fun transAssign(var, exp) = let
                fun unLx (Lx l) = l
                    | unLx _ = raise Fail "unLx"
            in Nx(Tr.MOVE(unLx var, unEx exp)) end

    fun transCall (label, defLevel, callLevel, args) = case defLevel of 
                ROOT => Ex(F.externalCall(Symbol.name label, map unEx args))
            | LEVEL{frame, parent, id} => let
                    val static_link = followStaticLink(defLevel, callLevel)
                in
                    Ex(Tr.CALL(Tr.NAME label, static_link::(map unEx args)))
                end

    fun transLet(d, body) = raise Fail "TODO: transLet"

    fun transLoop(cond, body) = 
        let
            val pretest = unCx(cond)
            val posttest = unCx(cond)
            val body = unNx(body)
            val bodylabel = Temp.newlabel()
            val endlabel = Temp.newlabel()
        in
            Nx(Tr.SEQ(seq[
                pretest(bodylabel, endlabel),
                Tr.LABEL bodylabel,
                unNx body,
                posttest(bodylabel, endlabel),
                Tr.LABEL endlabel
            ]))
        end
    
    fun transWhile(cond, body) = transLoop(cond, body)

    fun transFor(var, lo, hi, body) = 
        let
            val assignVar = transAssign(var, lo)
            val test = transRelop(Tr.LE, var, hi)
            val newbody = Nx(Tr.SEQ(unNx body, Tr.EXP(Tr.BINOP(Tr.PLUS, unLx var, Tr.CONST 1))))
        in
            Nx(Tr.SEQ(unNx assignVar, unNx(transLoop(test, newbody))))
        end

end