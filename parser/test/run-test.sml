CM.make("sources.cm");
OS.FileSys.getDir();
val testPath = OS.FileSys.getDir() ^ "/testcases";

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


