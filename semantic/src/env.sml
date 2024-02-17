structure Env :> ENV = 
struct
type access = unit
    (* to be implemented *)
type ty = Types.ty
datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {formals: ty list, result: ty}
val base_tenv = let 
                val envmap : (ty Symbol.table) = Symbol.empty
                in 
                envmap
                end
val base_venv = let
                val envmap : (enventry Symbol.table) = Symbol.empty
                in
                envmap
                end
end