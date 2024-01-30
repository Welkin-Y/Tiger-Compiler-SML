CM.make "lexer/src/resources.cm";
(* Test A Simple Variable Assignment *)
val infile = TextIO.openIn "filelist.txt";
fun dropNewline str =
    if String.size str > 0 andalso String.sub (str, String.size str - 1) = #"\n"
      then String.extract (str, 0, SOME (String.size str - 1))
        else str

fun read file = case TextIO.inputLine file of 
                  NONE => []
                | SOME line => dropNewline line::read file;
val parseList = read infile; 
TextIO.closeIn infile;
let fun parse (filename, ()) = (
                print ("Start to parse: " ^ filename ^ "\n");
                Parse.parse filename) handle Overflow => (
                        print("Int Overflow\n")
                )
        in foldl parse () parseList 
end;
