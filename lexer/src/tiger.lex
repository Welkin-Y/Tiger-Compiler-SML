type pos = int
type lexresult = Tokens.token
val commentDepth : int ref = ref 0
val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%% 
%s COMMENT;
%%

\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>"(*"   => (commentDepth := !commentDepth+1; YYBEGIN COMMENT; continue());

<INITIAL>","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL>"var"  	=> (Tokens.VAR(yypos,yypos+3));
<INITIAL>"let"     => (Tokens.LET(yypos, yypos+3));
<INITIAL>"end"     => (Tokens.END(yypos, yypos+3));
<INITIAL>"123"	=> (Tokens.INT(123,yypos,yypos+3));
<INITIAL>"V"     => (Tokens.ID("V", yypos, yypos+1));
<INITIAL>":="    => (Tokens.ASSIGN(yypos, yypos+2));
<INITIAL>" "     => (continue());
<INITIAL>.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
<COMMENT> "(*" => (commentDepth := !commentDepth+1; continue());
<COMMENT>"*)"   => (commentDepth := !commentDepth-1; if !commentDepth = 0 then YYBEGIN INITIAL else (); continue());
<COMMENT> .    => (continue());


