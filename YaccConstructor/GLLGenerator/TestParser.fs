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
    FOLLOW(A) = [| a; b; c; d |]
    FOLLOW(B) = [| EOF; a; b; c; d |]
    FOLLOW(S) = [| EOF; d |]
    FOLLOW(yard_start_rule) = [| EOF |]
*)

let private parse_A () =
    if _tokens.[pos] = 7
        then addDescriptor 2 // parse_yard_rule4_A
    if _tokens.[pos] = 5
        then addDescriptor 3 // parse_yard_rule5_A
    continueExecution ()

let private parse_yard_rule4_A () =
    if _tokens.[pos] <> 7 then continueExecution () else
    pos <- pos + 1
    pop(); continueExecution()

let private parse_yard_rule5_A () =
    if _tokens.[pos] <> 5 then continueExecution () else
    pos <- pos + 1
    pop(); continueExecution()

let private parse_B () =
    if _tokens.[pos] = 6
        then addDescriptor 5 // parse_yard_rule6_B
    if _tokens.[pos] = 5
        then addDescriptor 6 // parse_yard_rule7_B
    continueExecution ()

let private parse_yard_rule6_B () =
    if _tokens.[pos] <> 6 then continueExecution () else
    pos <- pos + 1
    pop(); continueExecution()

let private parse_yard_rule7_B () =
    if _tokens.[pos] <> 5 then continueExecution () else
    pos <- pos + 1
    pop(); continueExecution()

let private parse_S () =
    if Array.exists ((=) _tokens.[pos]) [| 4; 8 |]
        then addDescriptor 8 // parse_yard_rule0_S
    if Array.exists ((=) _tokens.[pos]) [| 5; 6 |]
        then addDescriptor 9 // parse_yard_rule1_S
    if Array.exists ((=) _tokens.[pos]) [| 5; 7 |]
        then addDescriptor 12 // parse_yard_rule2_S
    continueExecution ()

let private parse_yard_rule0_S () =
    pop(); continueExecution()

let private parse_yard_rule1_S () =
    push (10,pos) // parse_yard_nonterm0_rule1_S
    _parseFunctions.[4] () // parse_B
let private parse_yard_nonterm0_rule1_S () =
    push (11,pos) // parse_yard_nonterm1_rule1_S
    _parseFunctions.[7] () // parse_S
let private parse_yard_nonterm1_rule1_S () =
    pop(); continueExecution()

let private parse_yard_rule2_S () =
    push (13,pos) // parse_yard_nonterm0_rule2_S
    _parseFunctions.[1] () // parse_A
let private parse_yard_nonterm0_rule2_S () =
    push (14,pos) // parse_yard_nonterm1_rule2_S
    _parseFunctions.[7] () // parse_S
let private parse_yard_nonterm1_rule2_S () =
    if _tokens.[pos] <> 8 then continueExecution () else
    pos <- pos + 1
    pop(); continueExecution()

let private parse_yard_start_rule () =
    push (16,pos) // parse_yard_nonterm0_rule3_yard_start_rule
    _parseFunctions.[7] () // parse_S
let private parse_yard_nonterm0_rule3_yard_start_rule () =
    pop(); continueExecution()

let parse tokens =
    init tokens [| continueExecution; parse_A; parse_yard_rule4_A; parse_yard_rule5_A; parse_B; parse_yard_rule6_B; parse_yard_rule7_B; parse_S; parse_yard_rule0_S; parse_yard_rule1_S; parse_yard_nonterm0_rule1_S; parse_yard_nonterm1_rule1_S; parse_yard_rule2_S; parse_yard_nonterm0_rule2_S; parse_yard_nonterm1_rule2_S; parse_yard_start_rule; parse_yard_nonterm0_rule3_yard_start_rule |]
    parse_yard_start_rule ()
