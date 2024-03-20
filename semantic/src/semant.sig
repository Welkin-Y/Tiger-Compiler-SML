signature SEMANT = 
sig
    type tyvenv
    type tytenv
    type expty
    val transVar: tyvenv * tytenv * Absyn.var * int * (Symbol.symbol list) * Translate.level -> expty
    val transExp: tyvenv * tytenv * Absyn.exp * int * (Symbol.symbol list) * Translate.level -> expty
    val transDec: tyvenv * tytenv * Absyn.dec * int * (Symbol.symbol list) * Translate.level -> {venv: tyvenv , tenv: tytenv}
    val transProg: Absyn.exp -> unit
end

(* type expty = {exp: Translate.exp, ty: Types.ty} *)