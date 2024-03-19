structure MipsFrame : FRAME =
struct

val wordSize = 4
datatype access = InFrame of int | InReg of Temp.temp

type frame = {name: Temp.label, formals : access list, inFrameSize : int ref}

fun newFrame (arg : {name: Temp.label, formals: bool list}) : frame = 
let
  {name, formals} = arg
  val inFrameSize = ref 0
  fun getFormals (b, acc) = 
  let
    fun helper(b, (acc, inFrameNum, inRegNum)) => 
        if b andalso inRegNum < 8 then (inFrameSize := inFrameNum + 1;(InFrame(~inFrameNum * wordSize))::acc, inFrameNum + 1, inRegNum) 
        else ((InReg(Temp.newtemp()))::acc, inFrameNum, inRegNum + 1)

    val (acc, _, _) = foldl helper ([], 0, 0) formals
  in 
    acc
  end
in
  {name = name, formals = getFormals(formals, []), inFrameSize = inFrameSize}
end

fun name (frm: frame) : Temp.label = let val {name, _} = frm in name end

fun formals (frm: frame) : access list = let val {_, formals} = frm in formals end

fun allocLocal (frm : frame) (onFrame : bool) : access = case onFrame of
  true => (frm.inFrameSize := !frm.inFrameSize + 1; InFrame(~(!(frm.inFrameSize)) * wordSize))
  | false => InReg(Temp.newtemp()) 
  
end