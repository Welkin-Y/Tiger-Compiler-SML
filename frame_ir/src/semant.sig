signature SEMANT = 
sig
    type tyvenv
    type tytenv
    type expty
    val transVar: tyvenv * tytenv * Absyn.var * int *  Translate.level -> expty
    val transExp: tyvenv * tytenv * Absyn.exp * int *  Translate.level -> expty
    val transDec: tyvenv * tytenv * Absyn.dec * int *  Translate.level -> {venv: tyvenv , tenv: tytenv, exp: Translate.exp option}
    val transProg: Absyn.exp -> unit
end

(* type expty = {exp: Translate.exp, ty: Types.ty} *)