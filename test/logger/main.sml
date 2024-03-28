CM.make("sources.cm");
(* Logger.setLogPath("mylogpath.log"); *)
Logger.setLogLevel(Logger.ERROR); (* default Logger.INFO*)
Logger.log Logger.DEBUG "Debug message";
Logger.log Logger.INFO "Hello, tiger compiler log!";
Logger.log Logger.WARNING "THIS IS A WARNING!";
Logger.log Logger.ERROR "ERROR MESSAGE!";
Logger.log Logger.FATAL "FATAL ERROR!";
