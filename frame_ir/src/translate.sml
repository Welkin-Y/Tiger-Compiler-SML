structure Translate : TRANSLATE =
struct
    structure Tr = Tree
    structure Frame = MipsFrame
    
datatype level = ROOT | LEVEL of {parent : level, frame : F.frame}
type access = level * F.access


val outermost = ROOT

fun newLevel (parent, name, formals) = 
    let val f = F.newFrame(name, formals)
    in LEVEL{parent = parent, frame = f} end

fun formals (LEVEL{frame, ...}) = F.formals frame
fun allocLocal (LEVEL{frame, ...}, escape) = F.allocLocal(frame, escape)

datatype exp = Ex of Tr.exp 
| Nx of Tr.stm 
| Cx of (Temp.label * Temp.label -> Tr.stm)

fun procEntryExit(level, body) = ()


(* fun getResult()  *)

(* Copilot work *)


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
    Tr.TEMP r) end
| unEx (Nx s) = Tr.ESEQ(s Tr.CONST 0) 

(* Copilot work *)
fun unNx (Ex e) = Tr.EXP e
| unNx (Cx genstm) = genstm(Temp.newlabel(), Temp.newlabel())
| unNx (Nx s) = s

(* Copilot work *)
fun unCx (Ex(Tr.CONST 0)) = (fn (t,f) => Tr.JUMP(Tr.NAME f, [f]))
| unCx (Ex(Tr.CONST 1)) = (fn (t,f) => Tr.JUMP(Tr.NAME t, [t]))
| unCx (Ex e) = (fn (t,f) => Tr.CJUMP(Tr.NE, e, Tr.CONST 0, t, f))
| unCx (Nx s) = (fn (t,f) => s; Tr.JUMP(Tr.NAME t, [t]))
| unCx (Cx genstm) = genstm


end