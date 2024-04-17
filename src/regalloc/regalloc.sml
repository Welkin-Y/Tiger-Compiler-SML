functor Reg_Alloc(Frame: FRAME): REG_ALLOC =
struct
    structure Frame = Frame
    structure M = MakeGraph
    structure Liv = Liveness
    structure Color = Color(Frame)
    type allocation = Frame.register Temp.Table.table 

    fun alloc (instrs, frame) = 
    let
        val (fg, nodes) = M.instrs2graph (instrs)
        handle e => (TextIO.output(TextIO.stdOut, "instrs2graph error\n"); raise e)
        val (igraph, _) = Liv.interferenceGraph fg
        val (color_alloc, _) = Color.color {interference=igraph, initial=Frame.tempMap, spillCost=(fn n => 0), registers=Frame.registers}
    in
        (instrs, color_alloc)
    end

    fun show alloc =
        Temp.Table.appi (fn (t, r) => TextIO.output(TextIO.stdOut, Temp.makestring t ^ " -> " ^ Frame.reg_to_string r ^ "\n")) alloc
end