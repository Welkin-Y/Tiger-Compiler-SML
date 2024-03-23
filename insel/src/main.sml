structure Main = struct
  structure Frame = MipsFrame
  structure F = Frame
  structure Tr = Translate
  structure Mips = MipsGen
  
  (* structure R = RegAlloc *)

  fun getsome (SOME x) = x
  |   getsome NONE = raise Fail "getsome"

  fun emitproc out (F.PROC{body,frame}) = 
      (* val _ = print ("emit " ^ Frame.name frame ^ "\n") *)
        let
        val _ = Printtree.printtree(out,body)
        val stms = Canon.linearize body
        (*         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
        val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
        val instrs =   List.concat(map (Mips.codegen frame) stms') 
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
      let val absyn = Parse.parse filename
        val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
      in 
        withOpenFile (filename ^ ".s") 
        (fn out => (app (emitproc out) frags))
      end

end



