structure MipsFrame : FRAME =
struct

  (* MIPS field *)
  type register = string
  val RV: Temp.temp = Temp.newtemp()
  val FP: Temp.temp = Temp.newtemp()
  val registers: register list = []
  val tempMap: register Temp.Table.table = Temp.Table.empty
  val wordSize = 4
  val numArgReg = 4

  datatype access = InFrame of int (* memory location at offset X from the frame pointer *)
  | InReg of Temp.temp

  type frame = {name: Temp.label, formals : access list, inFrameSize : int ref}

  datatype frag = PROC of {body: Tree.stm, frame: frame}
  | STRING of Temp.label * string

  fun newFrame (arg : {name: Temp.label, formals: bool list}) : frame = let
        val {name, formals} = arg
        val inFrameSize = ref 0
        fun getFormals (b, acc) = 
            let
              fun helper(b, (acc, inFrameNum, inRegNum)) = 
                  if b orelse inRegNum = numArgReg then (
                      inFrameSize := inFrameNum + 1;
                      (InFrame(~inFrameNum * wordSize)::acc, inFrameNum + 1, inRegNum)
                    ) 
                  else ((InReg(Temp.newtemp()))::acc, inFrameNum, inRegNum + 1)
              val (acc, _, _) = foldl helper ([], 0, 0) formals
            in acc end
      in
        {name = name, formals = getFormals(formals, []), inFrameSize = inFrameSize}
      end

  fun name (frm: frame) : Temp.label = let val {name, ...} = frm in name end

  fun formals (frm: frame) : access list = let val {formals, ...} = frm in formals end

  fun allocLocal (frm: frame) (onFrame : bool) : access = case onFrame of
        true => (
            #inFrameSize frm := !(#inFrameSize frm)+ 1; 
            InFrame(~(!(#inFrameSize frm)) * wordSize)
          )
      | false => InReg(Temp.newtemp()) 

  fun externalCall(s: string, args: Tree.exp list) = Tree.CALL(Tree.NAME(Temp.namedlabel s), args)
  (* Tree.CALL(Tree.NAME(Tree.Temp.namedlabel s), args) p165*)

  fun string(label: Tree.label, str: string) = raise ErrorMsg.impossible "Not implemented"
  (* Tree.DATASTRING(label, str) *)

  fun procEntryExit1 (frm: frame, body: Tree.stm) = body

  fun exp (access: access) (addr: Tree.exp) : Tree.exp = case access of
        InFrame(offset) => Tree.READ(Tree.MEM(Tree.BINOP(Tree.PLUS, addr, Tree.CONST offset)))
      | InReg(temp) => Tree.READ(Tree.TEMP temp)



end