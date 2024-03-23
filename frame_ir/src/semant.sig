signature SEMANT = 
sig
    type tyvenv
    type tytenv
    type expty
    val transVar: tyvenv * tytenv * Absyn.var * int *  Translate.level * Temp.label option -> expty
    val transExp: tyvenv * tytenv * Absyn.exp * int *  Translate.level * Temp.label option -> expty
    val transDec: tyvenv * tytenv * Absyn.dec * int *  Translate.level * Temp.label option -> {venv: tyvenv , tenv: tytenv, exp: Translate.exp option}
    val transProg: Absyn.exp -> MipsFrame.frag list
end

(* type expty = {exp: Translate.exp, ty: Types.ty} *)