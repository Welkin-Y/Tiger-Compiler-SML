CM.make "sources.cm";
let 
    (*datatype level = DEBUG
    | INFO
    | WARNING 
    | ERROR
    | FATAL*)
    val _ = Logger.setLogLevel Logger.FATAL;
    fun dropNewline str =
            if String.size str > 0 andalso String.sub (str, String.size str - 1) = #"\n"
            then String.extract (str, 0, SOME (String.size str - 1))
            else str

    val inp = TextIO.inputLine TextIO.stdIn;
    val direct = dropNewline (Option.valOf inp);
    val () = print("Input file is" ^ direct ^ "\n")
in
    Main.compile direct
end