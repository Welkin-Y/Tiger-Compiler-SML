structure Env : ENV = 
struct
type access = unit
    (* to be implemented *)

datatype enventry = VarEntry of {access: Translate.access, ty: T.ty}
                    | FunEntry of {level: Translate.level, label: Temp.label, formals: T.ty list, result: T.ty}
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
                    (Symbol.symbol "flush", FunEntry {formals = [], result = Types.UNIT}),
                    (Symbol.symbol "getchar", FunEntry {formals = [], result = Types.STRING}),
                    (Symbol.symbol "ord", FunEntry {formals = [Types.STRING], result = Types.INT}),
                    (Symbol.symbol "chr", FunEntry {formals = [Types.INT], result = Types.STRING}),
                    (Symbol.symbol "size", FunEntry {formals = [Types.STRING], result = Types.INT}),
                    (Symbol.symbol "substring", FunEntry {formals = [Types.STRING, Types.INT, Types.INT], result = Types.STRING}),
                    (Symbol.symbol "concat", FunEntry {formals = [Types.STRING, Types.STRING], result = Types.STRING}),
                    (Symbol.symbol "not", FunEntry {formals = [Types.INT], result = Types.INT}),
                    (Symbol.symbol "exit", FunEntry {formals = [Types.INT], result = Types.UNIT})]
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