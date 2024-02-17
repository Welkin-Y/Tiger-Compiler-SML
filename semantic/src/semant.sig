signature SEMANT = 
sig
    type tyvenv
    type tytenv
    type expty
    val transVar: tyvenv * tytenv * Absyn.var -> expty
    val transExp: tyvenv * tytenv * Absyn.exp -> expty
    val transDec: tyvenv * tytenv * Absyn.dec -> {venv: tyvenv, tenv: tytenv}
    val transTy: tytenv * Absyn.ty -> Types.ty
    val transProg: Absyn.exp -> unit
end

(* type expty = {exp: Translate.exp, ty: Types.ty} *)