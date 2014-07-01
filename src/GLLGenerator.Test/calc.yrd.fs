module CalcParser
open Yard.Generators.GLL.AST
open Yard.Generators.GLL.Parser
type Token =
    | DIV
    | EOF
    | LBRACE
    | MINUS
    | MULT
    | NUMBER
    | PLUS
    | POW
    | RBRACE

let tokenToNumber = function
    | DIV -> 14
    | EOF -> 15
    | LBRACE -> 16
    | MINUS -> 17
    | MULT -> 18
    | NUMBER -> 19
    | PLUS -> 20
    | POW -> 21
    | RBRACE -> 22

(*
    0 -> expr
    1 -> factor
    2 -> factorOp
    3 -> powExpr
    4 -> powOp
    5 -> term
    6 -> termOp
    7 -> yard_rule_binExpr_1
    8 -> yard_rule_binExpr_3
    9 -> yard_rule_binExpr_5
    10 -> yard_rule_yard_many_1_2
    11 -> yard_rule_yard_many_1_4
    12 -> yard_rule_yard_many_1_6
    13 -> yard_start_rule
    14 -> DIV
    15 -> EOF
    16 -> LBRACE
    17 -> MINUS
    18 -> MULT
    19 -> NUMBER
    20 -> PLUS
    21 -> POW
    22 -> RBRACE
    FIRST(expr) = [|LBRACE;NUMBER|]
    FIRST(factor) = [|LBRACE;NUMBER|]
    FIRST(factorOp) = [|DIV;MULT|]
    FIRST(powExpr) = [|LBRACE;NUMBER|]
    FIRST(powOp) = [|POW|]
    FIRST(term) = [|LBRACE;NUMBER|]
    FIRST(termOp) = [|MINUS;PLUS|]
    FIRST(yard_rule_binExpr_1) = [|LBRACE;NUMBER|]
    FIRST(yard_rule_binExpr_3) = [|LBRACE;NUMBER|]
    FIRST(yard_rule_binExpr_5) = [|LBRACE;NUMBER|]
    FIRST(yard_rule_yard_many_1_2) = [|MINUS;PLUS|]
    FIRST(yard_rule_yard_many_1_4) = [|DIV;MULT|]
    FIRST(yard_rule_yard_many_1_6) = [|POW|]
    FIRST(yard_start_rule) = [|LBRACE;NUMBER|]
    FOLLOW(expr) = [|EOF;RBRACE|]
    FOLLOW(factor) = [|DIV;EOF;MINUS;MULT;PLUS;RBRACE|]
    FOLLOW(factorOp) = [|LBRACE;NUMBER|]
    FOLLOW(powExpr) = [|DIV;EOF;MINUS;MULT;PLUS;POW;RBRACE|]
    FOLLOW(powOp) = [|LBRACE;NUMBER|]
    FOLLOW(term) = [|EOF;MINUS;PLUS;RBRACE|]
    FOLLOW(termOp) = [|LBRACE;NUMBER|]
    FOLLOW(yard_rule_binExpr_1) = [|EOF;RBRACE|]
    FOLLOW(yard_rule_binExpr_3) = [|EOF;MINUS;PLUS;RBRACE|]
    FOLLOW(yard_rule_binExpr_5) = [|DIV;EOF;MINUS;MULT;PLUS;RBRACE|]
    FOLLOW(yard_rule_yard_many_1_2) = [|EOF;RBRACE|]
    FOLLOW(yard_rule_yard_many_1_4) = [|EOF;MINUS;PLUS;RBRACE|]
    FOLLOW(yard_rule_yard_many_1_6) = [|DIV;EOF;MINUS;MULT;PLUS;RBRACE|]
    FOLLOW(yard_start_rule) = [|EOF|]
*)
let private productions = [|[Ntrm 7];[Ntrm 0];[Ntrm 5;Ntrm 10];[Ntrm 6;Ntrm 5;Ntrm 10];[];[Trm 17];[Trm 20];[Ntrm 8];[Ntrm 1;Ntrm 11];[Ntrm 2;Ntrm 1;Ntrm 11];[];[Trm 14];[Trm 18];[Ntrm 9];[Ntrm 3;Ntrm 12];[Ntrm 4;Ntrm 3;Ntrm 12];[];[Trm 21];[Trm 16;Ntrm 0;Trm 22];[Trm 19]|]
let private actions = [|(14,2),[11];(14,11),[9];(14,12),[16];(15,10),[4];(15,11),[10];(15,12),[16];(16,0),[0];(16,1),[13];(16,3),[18];(16,5),[7];(16,7),[2];(16,8),[8];(16,9),[14];(16,13),[1];(17,6),[5];(17,10),[3];(17,11),[10];(17,12),[16];(18,2),[12];(18,11),[9];(18,12),[16];(19,0),[0];(19,1),[13];(19,3),[19];(19,5),[7];(19,7),[2];(19,8),[8];(19,9),[14];(19,13),[1];(20,6),[6];(20,10),[3];(20,11),[10];(20,12),[16];(21,4),[17];(21,12),[15];(22,10),[4];(22,11),[10];(22,12),[16]|] 
let parse tokens = ParserBase(13, 15, actions, productions, (Seq.map tokenToNumber tokens)).parse()
