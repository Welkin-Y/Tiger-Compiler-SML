signature SEMANT = 
sig
    type tyvenv
    type tytenv
    type expty
    val transVar: tyvenv * tytenv * Absyn.var * int -> expty
    val transExp: tyvenv * tytenv * Absyn.exp * int -> expty
    val transDec: tyvenv * tytenv * Absyn.dec * int -> {venv: tyvenv , tenv: tytenv}
    val transProg: Absyn.exp -> unit
end

(* type expty = {exp: Translate.exp, ty: Types.ty} *)