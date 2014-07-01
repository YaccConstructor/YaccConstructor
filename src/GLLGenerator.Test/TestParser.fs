module Parser
open Yard.Generators.GLL.AST
open Yard.Generators.GLL.Parser
type Token =
    | A
    | B
    | C
    | D
    | EOF

let tokenToNumber = function
    | A -> 4
    | B -> 5
    | C -> 6
    | D -> 7
    | EOF -> 8

(*
    0 -> nA
    1 -> nB
    2 -> nS
    3 -> yard_start_rule
    4 -> A
    5 -> B
    6 -> C
    7 -> D
    8 -> EOF
    FIRST(nA) = [|A;C|]
    FIRST(nB) = [|A;B|]
    FIRST(nS) = [|A;B;C|]
    FIRST(yard_start_rule) = [|A;B;C|]
    FOLLOW(nA) = [|A;B;C;D|]
    FOLLOW(nB) = [|A;B;C;D;EOF|]
    FOLLOW(nS) = [|D;EOF|]
    FOLLOW(yard_start_rule) = [|EOF|]
*)
let private productions = [|[];[Ntrm 1;Ntrm 2];[Ntrm 0;Ntrm 2;Trm 7];[Ntrm 2];[Trm 6];[Trm 4];[Trm 5];[Trm 4]|]
let private actions = [|(4,0),[5];(4,1),[7];(4,2),[1;2];(4,3),[3];(5,1),[6];(5,2),[1];(5,3),[3];(6,0),[4];(6,2),[2];(6,3),[3];(7,2),[0];(8,2),[0];(8,3),[3]|] 
let parse tokens = ParserBase(3, 8, actions, productions, (Seq.map tokenToNumber tokens)).parse()
