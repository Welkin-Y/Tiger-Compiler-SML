structure MipsFrame : FRAME =
struct

  (* MIPS field *)
  val wordSize = 4

  datatype access = InFrame of int (* indicates a memory location at offset X from the frame pointer *)
  | InReg of Temp.temp

  type frame = {name: Temp.label, formals : access list, inFrameSize : int ref}

  fun newFrame (arg : {name: Temp.label, formals: bool list}) : frame = let
        val {name, formals} = arg
        val inFrameSize = ref 0
        fun getFormals (b, acc) = 
            let
              fun helper(b, (acc, inFrameNum, inRegNum)) = 
                  if b andalso inRegNum < 8 then (
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

  fun allocLocal (frm : frame) (onFrame : bool) : access = case onFrame of
        true => (
            frm.inFrameSize := !frm.inFrameSize + 1; 
            InFrame(~(!(frm.inFrameSize)) * wordSize)
          )
      | false => InReg(Temp.newtemp()) 


  fun exp (access: access) (addr: Tree.exp) : Tree.exp = case access of
        InFrame(offset) => Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.TEMP addr, Tree.CONST offset))
      | InReg(temp) => Tree.TEMP temp

  fun externalCall(s: string, args: Tree.exp list) =
      Tree.CALL(Tree.NAME(Tree.Temp.namedlabel s), args)

end