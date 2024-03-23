structure MipsGen: CODEGEN = 
struct
    structure Frame = MipsFrame
    structure F = Frame
    structure T = Tree

     fun codegen (frame: MipsFrame.frame) (irTree: T.stm): Assem.instr list = raise Fail "Not implemented"
end