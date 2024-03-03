CM.make "lexer/src/resources.cm";
(* Test int with leading zero *)
print "\n\n-----Test int with leading zero------\n";
Parse.parse "lexer/resources/leadingZeroInt_qf.tig";
(* Test overflowed int *)
print "\n\n-----Test overflowed int-----\n";
Parse.parse "lexer/resources/largeInt_qf.tig";
(* Test basic string *)
print "\n\n-----Test string-----\n";
Parse.parse "lexer/resources/string_qf.tig";
(* Test identifier *)
print "\n\n-----Test identifier-----\n";
Parse.parse "lexer/resources/identifier_qf.tig";
(* Test comment *)
print "\n\n-----Test comment-----\n";
Parse.parse "lexer/resources/comment_qf.tig";
print "\n\n-----Test comment-----\n";
Parse.parse "lexer/resources/vehicle.tig";