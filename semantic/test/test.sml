CM.make("sources.cm");
val absyn = ParserSemant.parse TextIO.stdOut "example.tig";
Semant.transProg (absyn); 