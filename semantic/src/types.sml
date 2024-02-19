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

  fun toString (RECORD _) = "RECORD"
    | toString NIL = "NIL"
    | toString INT = "INT"
    | toString STRING = "STRING"
    | toString (ARRAY _) = "ARRAY"
    | toString (NAME n) = "NAME" ^ " " ^ (toString (Option.valOf (!(#2 n))))
    | toString UNIT = "UNIT"

end
