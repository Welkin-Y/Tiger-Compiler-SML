
(* dummy Translate *)
structure Translate = struct type exp = unit end
structure T = Types
structure Semant :> SEMANT =
struct
    type venv = Env.enventry Symbol.table
    type tenv = T.ty Symbol.table
    type expty = {exp: Translate.exp, ty: T.ty}


    fun transVar (venv: venv, tenv: tenv, var: A.var) = {exp = (), ty = T.INT}
    fun transExp (venv: venv, tenv: tenv, var: A.exp) = {exp = (), ty = T.INT}
    fun transDec (venv: venv, tenv: tenv, dec: A.dec) = {venv = venv, tenv = tenv}
    fun transTy (tenv: tenv, ty: A.ty) = T.INT
end