structure MipsFrame : FRAME =
struct
datatype access = InFrame of int | InReg of Temp.temp
type frame = {name: Temp.label, formals : access list}
fun newFrame (arg : {name: Temp.label, formals: bool list}) : frame = 
let
  {name, formals} = arg
  fun getFormals (b, acc) = 
  let
    fun helper(b, (acc, inFrameNum, inRegNum)) => 
        if b andalso inRegNum < 8 then ((InFrame(inFrameNum * 4))::acc, inFrameNum + 1, inRegNum) 
        else ((InReg(Temp.newtemp()))::acc, inFrameNum, inRegNum+1)
    val (acc, _, _) = foldl helper ([], 0, 0) formals
  in 
    acc
  end
in
  {name = name, formals = getFormals(formals, [])}
end

fun name (frm: frame) : Temp.label = 
fun formals (frm: frame) : access list = 
fun allocLocal (frm : frame) : bool -> access = 
end