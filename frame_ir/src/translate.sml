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

    (* Copilot work *)
    fun unNx (Ex e) = Tr.EXP e
        | unNx (Cx genstm) = raise Fail "TODO: unNx(Cx genstm)"
        | unNx (Nx s) = s

    fun unCx (Ex(Tr.CONST 0)) = (fn (t,f) => Tr.JUMP(Tr.NAME f, [f]))
        | unCx (Ex(Tr.CONST 1)) = (fn (t,f) => Tr.JUMP(Tr.NAME t, [t]))
        | unCx (Ex e) = (fn (t,f) => Tr.CJUMP(Tr.NE, e, Tr.CONST 0, t, f))
        | unCx (Cx genstm) = genstm
        (* unCx(Nx _) need not be translated *)
        | unCx (Nx _) = (ErrorMsg.impossible "Cannot contruct conditional from no results"; (fn _ => Tr.EXP(Tr.CONST 0)))

    fun transNil () = Ex(Tr.CONST 0)

    fun transInt (num : int) = Ex(Tr.CONST num)

    fun transIf(cond, thenexp, elseexp) = 
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

    fun simpleVar (access, level) = raise Fail "TODO: simpleVar"

    fun transString(lit) = 
            let 
                val label = Temp.newlabel()
            in
                (* TODO *)
                Ex(Tr.NAME label)
            end

    fun transBinop(oper, e1, e2) = Ex(Tr.BINOP(Tr.getBinop oper, unEx e1, unEx e2))

    fun transRelop(oper, e1, e2) = Cx(fn (t, f) => Tr.CJUMP(Tr.getRelop oper, unEx e1, unEx e2, t, f))

end