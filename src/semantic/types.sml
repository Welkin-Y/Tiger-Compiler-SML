structure Types =
struct

  type unique = unit ref

  datatype ty = RECORD of unit -> (Symbol.symbol * ty) list * unique * Symbol.symbol
  | NIL
  | INT
  | STRING
  | ARRAY of ty * unique
  | UNIT

  fun symbolTyListToString (symbolTyList: (Symbol.symbol * ty) list) =
    let
        fun pairToString (symbol, ty) = case ty of 
            RECORD f => Symbol.name symbol ^ ": RECORD " ^ Symbol.name (#3 (f()))
            | _ => Symbol.name symbol ^ ": " ^ toString ty
        val stringList = List.map pairToString symbolTyList
    in
        String.concatWith ", " stringList
    end
  and toString (RECORD f) = "RECORD of " ^ (symbolTyListToString (#1 (f())))
    | toString NIL = "NIL"
    | toString INT = "INT"
    | toString STRING = "STRING"
    | toString (ARRAY n) = "ARRAY of " ^ (
      case #1 n of
        RECORD f => "RECORD " ^ (Symbol.name (#3 (f())))
        | a => toString a)
    | toString UNIT = "UNIT"

  fun equals (ty1, ty2) = case (ty1, ty2) of
    (RECORD f1, RECORD f2) => #2 (f1()) = #2 (f2())
    (* | (NIL, NIL) => true *) (*nil cannot equals to nil because nil can only be used when the type can be determined*)
    | (INT, INT) => true
    | (STRING, STRING) => true
    | (ARRAY a1, ARRAY a2) => #2 a1 = #2 a2
    | (UNIT, UNIT) => true
    | (NIL, RECORD _) => true
    | (RECORD _, NIL) => true
    | _ => false

end
