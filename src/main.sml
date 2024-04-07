structure Main = struct
  structure F = MipsFrame
  structure Tr = Translate(F)
  structure S = Semant(Tr)
  structure Mips = MipsGen
  structure L = Logger
  structure M = MakeGraph

  
  (* structure R = RegAlloc *)

  fun getsome (SOME x) = x
    |   getsome NONE = raise Fail "getsome"

  fun emitproc out (F.PROC{body,frame}) = 
      (* val _ = print ("emit " ^ Frame.name frame ^ "\n") *)
      (* val _ = Printtree.printtree(out,body) *)
      (* val _ = app (fn s => Printtree.printtree(out,s)) stms *)
      let
        (* val _ = Printtree.printtree(TextIO.stdOut,body) *)
        val stms = Canon.linearize body
        (* val _ = app (fn s => Printtree.printtree(TextIO.stdOut,s)) stms *)
        (*val (stmslist, _) = Canon.basicBlocks stms
        val _ = app (fn x => (app (fn s => Printtree.printtree(TextIO.stdOut,s)) x; print "\n")) stmslist *)
        val canon_stms = Canon.traceSchedule(Canon.basicBlocks stms)
        (* val _ = app (fn s => Printtree.printtree(TextIO.stdOut,s)) canon_stms *)
        val instrs = List.concat(map (Mips.codegen frame) canon_stms) 
        val _ = M.instrs2graph instrs
        val format0 = Assem.format(Temp.makestring)
      in  app (fn i => TextIO.output(out,format0 i)) instrs
      end
      
    | emitproc out (F.STRING(lab,s)) = TextIO.output(out,F.string(lab,s))

  fun withOpenFile fname f = 
      let val out = TextIO.openOut fname
      in (f out before TextIO.closeOut out) 
        handle e => (TextIO.closeOut out; raise e)
      end 

  fun compile filename = 
      let 
        val _ = L.log L.WARNING ("Start to compile" ^ filename)
        val absyn = Parse.parse filename
        val frags:Tr.frag list = (FindEscape.findEscape absyn; S.transProg absyn)
        (* val _ = PrintAbsyn.print(TextIO.stdOut,absyn) *)
      in 
        withOpenFile (filename ^ ".s") (fn out => (app (emitproc out) frags))
      end

end



