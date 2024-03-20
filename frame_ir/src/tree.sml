signature TREE = 
sig 
  type label = Temp.label
  type size

  datatype stm = SEQ of stm * stm
  | LABEL of label
  | JUMP of exp * label list
  | CJUMP of relop * exp * exp * label * label
  | MOVE of exp * exp
  | EXP of exp

  and exp = BINOP of binop * exp * exp
  | ESEQ of stm * exp
  | NAME of label
  | CONST of int
  | CALL of exp * exp list

  and loc = MEM of exp
  | TEMP of Temp.temp

  and binop = PLUS | MINUS | MUL | DIV 
  | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

  and relop = EQ | NE | LT | GT | LE | GE 
  | ULT | ULE | UGT | UGE

  val getBinop : A.oper -> binop
  val getRelop : A.oper -> relop

  (* val notRel : relop -> relop *)
  (* val commute: relop -> relop *)
end

structure Tree : TREE = 
struct
  type label=Temp.label
  type size = int

  datatype stm = SEQ of stm * stm
  | LABEL of label
  | JUMP of exp * label list
  | CJUMP of relop * exp * exp * label * label
  | MOVE of exp * exp
  | EXP of exp

  and exp = BINOP of binop * exp * exp
  | ESEQ of stm * exp
  | NAME of label
  | CONST of int
  | CALL of exp * exp list

  and loc = TEMP of Temp.temp 
  | MEM of exp 
  
  and binop = PLUS | MINUS | MUL | DIV 
  | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

  and relop = EQ | NE | LT | GT | LE | GE 
  | ULT | ULE | UGT | UGE

  fun getBinop A.PlusOp = PLUS
    | getBinop A.MinusOp = MINUS
    | getBinop A.TimesOp = MUL
    | getBinop A.DivideOp = DIV
  
  fun getRelop A.EqOp = EQ
    | getRelop A.NeqOp = NE
    | getRelop A.LtOp = LT
    | getRelop A.LeOp = LE
    | getRelop A.GtOp = GT
    | getRelop A.GeOp = GE

end

