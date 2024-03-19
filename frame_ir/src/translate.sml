structure Translate : TRANSLATE =
struct
    structure Tr = Tree

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