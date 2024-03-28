CM.make("semantic/test/sources.cm");
fun dropNewline str =
    if String.size str > 0 andalso String.sub (str, String.size str - 1) = #"\n"
      then String.extract (str, 0, SOME (String.size str - 1))
        else str

val inp = TextIO.inputLine TextIO.stdIn;
val direct = dropNewline (Option.valOf inp);
val filename = OS.FileSys.fullPath direct;

let  
    val absyn = Parse.parse filename
in 
    PrintAbsyn.print (TextIO.stdOut, absyn);
    Semant.transProg absyn 
end;