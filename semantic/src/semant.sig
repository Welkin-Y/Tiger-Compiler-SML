signature SEMANT = 
sig
    type tyvenv
    type tytenv
    type expty
    val transVar: tyvenv ref * tytenv ref* Absyn.var -> expty
    val transExp: tyvenv ref * tytenv ref * Absyn.exp -> expty
    val transDec: tyvenv ref * tytenv ref * Absyn.dec -> {venv: tyvenv ref, tenv: tytenv ref}
    val transTy: tytenv ref * Absyn.ty -> Types.ty
    val transProg: Absyn.exp -> unit
    val prTransProg: Absyn.exp -> unit
end

(* type expty = {exp: Translate.exp, ty: Types.ty} *)