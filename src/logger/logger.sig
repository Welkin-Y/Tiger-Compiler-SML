signature LOGGER = 
sig
    datatype level = DEBUG
  | INFO
  | WARNING 
  | ERROR
  | FATAL

  val logPath: string option ref
  val logLevel: level ref 

  val levelToString: level -> string
  val levelToInt: level -> int 

  val setLogPath: string -> unit
  val setLogLevel: level -> unit

  val log: level -> string -> unit

end