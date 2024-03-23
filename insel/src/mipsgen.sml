structure MipsGen: CODEGEN = 
struct
    structure Frame = MipsFrame
    structure F = Frame
    structure T = Tree

    fun codegen (frame: MipsFrame.frame) (irTree: T.stm): Assem.instr list = 
            raise Fail "Not implemented"
    (*  let val ilist = ref (nil: A.instr list)
        fun emit x= ilist := x :: !ilist
        fun result(gen) = let val t = Temp.newtemp() in gen t; t end
        fun munchStm ...
        and munchExp ...
        in munchStm stm;
        rev(!ilist)
        end *)
end