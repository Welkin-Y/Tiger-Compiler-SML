structure Env :> ENV = 
struct
type access = unit
    (* to be implemented *)

datatype enventry = VarEntry of {ty: T.ty}
                    | FunEntry of {formals: T.ty list, result: T.ty}
val base_tenv = let 
                val envmap : (T.ty Symbol.table) = Symbol.empty
                val initlist = [(Symbol.symbol "int", Types.INT), 
                    (Symbol.symbol "string", Types.STRING), 
                    (Symbol.symbol "nil", Types.NIL),
                    (Symbol.symbol "unit", Types.UNIT)]
                in 
                foldl (fn ((key, value), tab) => Symbol.enter (tab, key, value)) envmap initlist
                end
val base_venv = let
                val envmap : (enventry Symbol.table) = Symbol.empty
                in
                envmap
                end
end