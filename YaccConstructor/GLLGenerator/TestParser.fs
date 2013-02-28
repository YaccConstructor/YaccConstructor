module Parser
open Yard.Generators.GLL.Parser
(*
    0 -> A
    1 -> B
    2 -> S
    3 -> yard_start_rule
    4 -> EOF
    5 -> a
    6 -> b
    7 -> c
    8 -> d
    FIRST(A) = [|a;c|]
    FIRST(B) = [|a;b|]
    FIRST(S) = [|a;b;c|]
    FIRST(yard_start_rule) = [|a;b;c|]
    FOLLOW(A) = [|a;b;c;d|]
    FOLLOW(B) = [|EOF;a;b;c;d|]
    FOLLOW(S) = [|EOF;d|]
    FOLLOW(yard_start_rule) = [|EOF|]
*)
let private productions = [|[];[Nonterminal 1;Nonterminal 2];[Nonterminal 0;Nonterminal 2;Terminal 8];[Nonterminal 2];[Terminal 7];[Terminal 5];[Terminal 6];[Terminal 5]|]
let private actions = [|(4,2),[0];(4,3),[3];(5,0),[5];(5,1),[7];(5,2),[1;2];(5,3),[3];(6,1),[6];(6,2),[1];(6,3),[3];(7,0),[4];(7,2),[2];(7,3),[3];(8,2),[0]|] 
let parse tokens = ParserBase(3, 4, actions, productions, tokens).parse()
