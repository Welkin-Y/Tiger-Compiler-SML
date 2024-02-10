CM.make("sources.cm");
fun dropNewline str =
    if String.size str > 0 andalso String.sub (str, String.size str - 1) = #"\n"
      then String.extract (str, 0, SOME (String.size str - 1))
        else str

val inp = TextIO.inputLine TextIO.stdIn;
val direct = dropNewline (Option.valOf inp);
val testPath = OS.FileSys.fullPath direct;
val dir = OS.FileSys.openDir testPath;
OS.FileSys.chDir testPath;



fun runTest (di) =
    case OS.FileSys.readDir di of
    NONE => print "\n------Done------\n"
    | SOME f => parseHelper(f, di)
and parseHelper (f, di) = 
    let 
    val _ = print("\n------Parsing " ^ f ^ "------\n");
    val _ = ParseTest.parse TextIO.stdOut f;
    in
    runTest di
    end;

runTest dir;
OS.FileSys.closeDir dir;


