CM.make("sources.cm");
fun dropNewline str =
    if String.size str > 0 andalso String.sub (str, String.size str - 1) = #"\n"
      then String.extract (str, 0, SOME (String.size str - 1))
        else str

val inp = TextIO.inputLine TextIO.stdIn;
val direct = dropNewline (Option.valOf inp);

Logger.setLogLevel Logger.DEBUG;
Logger.log Logger.WARNING ("Parse: " ^ direct);
val absyn = ParserSemant.parse TextIO.stdOut direct;
Semant.transProg (absyn); 