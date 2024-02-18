structure Env : ENV = 
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
                val initlist = [(Symbol.symbol "print", FunEntry {formals = [Types.STRING], result = Types.UNIT}),
                    (Symbol.symbol "break", FunEntry {formals = [], result = Types.UNIT}),
                    (Symbol.symbol "exit", FunEntry {formals = [], result = Types.UNIT}),
                    (Symbol.symbol "add", FunEntry {formals = [Types.INT, Types.INT], result = Types.INT}),
                    (Symbol.symbol "a", VarEntry {ty = Types.INT})]
                in
                foldl (fn ((key, value), tab) => Symbol.enter (tab, key, value)) envmap initlist
                end

fun enventryToString (VarEntry {ty}) = T.toString ty
  | enventryToString (FunEntry {formals, result}) = 
    let
      fun tyListToString [] = ""
        | tyListToString [ty] = T.toString ty
        | tyListToString (ty::tys) = T.toString ty ^ " * " ^ tyListToString tys
    in
      "(" ^ tyListToString formals ^ ")" ^ " -> " ^ T.toString result
    end
end