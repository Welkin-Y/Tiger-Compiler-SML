CM.make "sources.cm";
(if not (SMLofNJ.exportML("compiler"))
then (print("Heap image saved..\n");(OS.Process.exit OS.Process.success):int)
else (Main.compile(hd(CommandLine.arguments())); 
       print("Exiting...\n");
       (OS.Process.exit OS.Process.success):int))
(*sml drew.sml*)
(*sml @SMLload=compiler.amd64-linux example.tig*)



