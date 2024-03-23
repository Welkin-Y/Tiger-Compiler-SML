CM.make "sources.cm";
let 
    val _ = Logger.setLogLevel Logger.DEBUG
in
    Main.compile "example.tig"
end;