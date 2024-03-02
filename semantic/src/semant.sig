signature SEMANT = 
sig
    type tyvenv
    type tytenv
    type expty
    val transVar: tyvenv * tytenv * Absyn.var * int * (Symbol.symbol list)-> expty
    val transExp: tyvenv * tytenv * Absyn.exp * int * (Symbol.symbol list)-> expty
    val transDec: tyvenv * tytenv * Absyn.dec * int * (Symbol.symbol list)-> {venv: tyvenv , tenv: tytenv}
    val transProg: Absyn.exp -> unit
end

(* type expty = {exp: Translate.exp, ty: Types.ty} *)