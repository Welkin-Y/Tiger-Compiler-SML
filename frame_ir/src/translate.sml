(* Called by Semant, Generate Tree represent IR*)
structure Translate : TRANSLATE =
struct
    structure Tr = Tree
    structure F = MipsFrame
    
datatype level = ROOT 
| LEVEL of {parent : level, frame : F.frame, id: unit ref  }

type access = level * F.access
type frag = F.frag

val outermost = ROOT

fun newLevel (parent, name, formals: bool list) = 
    let val f = F.newFrame(name, true::formals)
    in LEVEL{parent = parent, frame = f} end

fun formals (LEVEL{frame, ...}) = F.formals frame
| formals(ROOT) = []
fun allocLocal (LEVEL{frame, ...}) (escape) = F.allocLocal(frame, escape)
| allocLocal (ROOT) _ = raise ErrorMsg.Error "allocLocal: no frame"

datatype exp = Ex of Tr.exp 
| Nx of Tr.stm 
| Cx of (Temp.label * Temp.label -> Tr.stm)

fun procEntryExit(level, body) = ()


(* fun getResult()  *)





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
        r.TEMP r) end
| unEx (Nx s) = Tr.ESEQ(s Tr.CONST 0) 

(* Copilot work *)
fun unNx (Ex e) = Tr.EXP e
| unNx (Cx genstm) = raise Fail "TODO: unNx(Cx genstm)"
| unNx (Nx s) = s

(* treat the cases of CONST 0 *)
fun unCx (Ex(Tr.CONST 0)) = (fn (t,f) => Tr.JUMP(Tr.NAME f, [f]))
(* treat the cases of CONST 1 *)
| unCx (Ex(Tr.CONST 1)) = (fn (t,f) => Tr.JUMP(Tr.NAME t, [t]))
| unCx (Ex e) = (fn (t,f) => Tr.CJUMP(Tr.NE, e, Tr.CONST 0, t, f))
| unCx (Cx genstm) = genstm
(* unCx(Nx _) need not be translated *)
| unCx (Nx _) = (ErrorMsg.impossible "Cannot contruct conditional from no results", fn _ => Tr.EXP(Tr.CONST 0))

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
    Ex(T.ESEQ(seq[
        test(labelthen, labelelse),
        T.LABEL labelthen,
        T.MOVE(T.TEMP res, thenexp),
        T.JUMP(T.NAME labelend, [labelend]),
        T.LABEL labelelse,
        T.MOVE(T.TEMP res, elseexp),
        T.LABEL labelend],
        T.TEMP res))
end