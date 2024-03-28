signature MAKEGRAPH =
sig
    val instrs2graph: Assem.instr list ->
        Flow.flowgraph * Flow.Graph.node list
end

structure MakeGraph :> MAKEGRAPH =
struct
    val instrs2graph = raise Fail "Not implemented"
end