structure Types =
struct

  type unique = unit ref

  datatype ty = RECORD of (Symbol.symbol * ty) list * unique
  | NIL
  | INT
  | STRING
  | ARRAY of ty * unique
  | NAME of Symbol.symbol * ty option ref
  | UNIT

  fun symbolTyListToString (symbolTyList: (Symbol.symbol * ty) list) =
    let
        fun pairToString (symbol, ty) = Symbol.name symbol ^ ": " ^ toString ty
        val stringList = List.map pairToString symbolTyList
    in
        String.concatWith ", " stringList
    end
  and toString (RECORD fields) = "RECORD of " ^ (symbolTyListToString (#1 fields))
    | toString NIL = "NIL"
    | toString INT = "INT"
    | toString STRING = "STRING"
    | toString (ARRAY n) = "ARRAY of " ^ (toString (#1 n))
    (*cannot handle recursive tydef for now*)
    | toString (NAME n) = "NAME " ^ (toString (Option.valOf (!(#2 n))))
    | toString UNIT = "UNIT"

end
