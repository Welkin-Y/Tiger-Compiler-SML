
(* dummy Translate *)
structure Translate = struct type exp = unit end

structure Semant :> SEMANT =
struct
    type venv = Env.enventry Symbol.table
    type tenv = ty Symbol.table
    type expty = {exp: Translate.exp, ty: Types.ty}


    fun transVar (venv, tenv, Absyn.var) = {exp = (), ty = Types.INT}
    fun transExp (venv, tenv, Absyn.exp) = {exp = (), ty = Types.INT}
    fun transTy (tenv, Absyn.ty) = Types.INT
end