structure Logger :> LOGGER = struct
    datatype level = DEBUG
    | INFO
    | WARNING 
    | ERROR
    | FATAL

    val logPath: string option ref = ref NONE
    val logLevel: level ref = ref INFO

    fun levelToInt DEBUG = 0
        | levelToInt INFO = 1
        | levelToInt WARNING = 2
        | levelToInt ERROR = 3
        | levelToInt FATAL = 4

    fun levelToString INFO = "INFO" 
        | levelToString WARNING = "WARNING" 
        | levelToString ERROR = "ERROR"
        | levelToString FATAL = "FATAL"
        | levelToString DEBUG = "DEBUG"

    fun humanReadableTime() = let val currentTime = Time.now () (* Get the current time *)
            fun timeToString n = if n < 10 then "0" ^ Int.toString n else Int.toString n
            in
                let val date = Date.fromTimeLocal currentTime (* Convert time to date *)
                in
                    let val year = Date.year date
                        and month = Date.fmt "%b" date (* Get the abbreviated month *)
                        and day = Date.day date
                    in
                        let val hours = Time.toSeconds currentTime mod (24*60*60) div (60*60)
                            and minutes = Time.toSeconds currentTime mod (60*60) div 60
                            and seconds = Time.toSeconds currentTime mod 60
                        in  
                            Int.toString year^ "-" ^
                            month ^ "-" ^
                            Int.toString day ^ " " ^
                            timeToString(IntInf.toInt hours) ^ ":" ^
                            timeToString(IntInf.toInt minutes) ^ ":" ^
                            timeToString(IntInf.toInt seconds)
                        end
                    end
                end
            end

    fun setLogPath(path:string) = logPath := (SOME path)
    fun setLogLevel(l:level) = logLevel := l

    (* Function to log a message with a given log level *)
    fun log level message = 
            if levelToInt level >= levelToInt (!logLevel) then
                let
                    val outStream = case !logPath of
                            NONE => TextIO.openAppend "tiger_compiler.log"  (* Open log file in append mode *)
                        | SOME path => TextIO.openAppend path 
                    val timestamp = humanReadableTime() (* Get current time as string *)
                    val logEntry = String.concat [timestamp, " [", levelToString level, "] ", message, "\n"]
                in
                    TextIO.output (outStream, logEntry);  (* Write log entry to file *)
                    TextIO.closeOut outStream  (* Close the file stream *)
                end
            else ()
end
