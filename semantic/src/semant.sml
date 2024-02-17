
(* dummy Translate *)
structure Translate = struct type exp = unit end
structure T = Types
structure Semant :> SEMANT =
struct
    type venv = Env.enventry Symbol.table
    type tenv = T.ty Symbol.table
    type expty = {exp: Translate.exp, ty: T.ty}


    fun transVar (venv, tenv, var) = {exp = (), ty = T.INT}
    fun transExp (venv, tenv, exp) = {exp = (), ty = T.INT}
    fun transDec (venv, tenv, dec) = {venv = venv, tenv = tenv}
    fun transTy (tenv, ty) = T.INT
    fun transProg (exp: A.exp) = (transExp (Env.base_venv, Env.base_tenv, exp); print "yeah";())
end