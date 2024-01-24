CM.make "lexer/src/resources.cm";
(* Test A Simple Variable Assignment *)
Parse.parse "lexer/resources/basic.tig";
(* Test Simple Comment *)
Parse.parse "lexer/resources/numbers.tig";
(* Parse.parse "../resources/basicComment.tig";
(* Test 8 Queens *)
Parse.parse "../resources/queens.tig"; *)
