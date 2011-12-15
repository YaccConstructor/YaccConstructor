// Learn more about F# at http://fsharp.net

module GNESCCCoreSimpleTest

open Yard.Generators.GNESCCGenerator
open Yard.Generators.GNESCCGenerator.AST
open NUnit.Framework
open Yard.Generators
open LexCommon

type AnsAst =
    | ANode of AnsAst list
    | ALeaf

let printInput lexer getName =
    try 
        for i in 1..100000000 do
            printf "(%d: %A) "
                    ((lexer :> ILexer).Get i).tag
                    (getName ((lexer :> ILexer).Get i).tag)
    with
        | _ -> ()
    printfn ""

let run path tables actions regexp getTag getName =
    let lexer = LexCommon.Lexer(path, getTag)
    printInput lexer getName

    let parseRes = 
        let ti = new TableInterpreter(tables)
        ti.Run lexer
    parseRes

let compareRes list1 list2 =
    let rec compareList list1 list2 =
        let rec compareStruct tree1 tree2 =
            match tree1, tree2 with
            | Node (l1,_,_), ANode l2 ->
                compareList l1 l2
            | Leaf _, ALeaf -> true
            | _ -> false
        match list1, list2 with
        | h1::t1, h2::t2 ->
            if not (compareStruct h1 h2) then false
            else compareList t1 t2
        | [], [] -> true
        | _ -> false

    let rec compAns l1 l2 =
        match l1, l2 with
        | ALeaf, ALeaf -> 0
        | ALeaf, ANode _ -> 1
        | ANode _, ALeaf -> -1
        | ANode l1, ANode l2 ->
            let rec inner l1 l2 =
                match l1, l2 with
                | [], [] -> 0
                | _, [] -> 1
                | [], _ -> -1
                | h1::t1, h2::t2 ->
                    match compAns h1 h2 with
                    | 0 -> inner t1 t2
                    | x -> x
            inner l1 l2    

    let rec compOut l1 l2 =
        match l1, l2 with
        | Leaf _, Leaf _ -> 0
        | Leaf _, Node _ -> 1
        | Node _, Leaf _ -> -1
        | Node (l1,_,_), Node (l2,_,_) ->
            let rec inner l1 l2 =
                match l1, l2 with
                | [], [] -> 0
                | _, [] -> 1
                | [], _ -> -1
                | h1::t1, h2::t2 ->
                    match compOut h1 h2 with
                    | 0 -> inner t1 t2
                    | x -> x
            inner l1 l2

    compareList
        (List.sortWith compOut list1)
        (List.sortWith compAns list2)

let dir = @"../../../../Tests/GNESCC/simple/"

[<TestFixture>]
type ``GNESCC core tests with simple lexer`` () =

    [<Test>]
    member test.``Simple grammar test``() =
        let regexp     = GNESCC.Regexp_first_grammar.ruleToRegex
        let actions    = GNESCC.Actions_first_grammar.ruleToAction
        let tables     = GNESCCGenerator.Tables_first_grammar.tables
        let getTag     = GNESCCGenerator.Tables_first_grammar.getTag
        let getName    = GNESCCGenerator.Tables_first_grammar.getName
        let path = dir + "first_grammar/input.txt"
        let rightValue = [ANode [ALeaf; ANode[ALeaf; ANode[ALeaf; ANode[ALeaf; ANode[ALeaf]]]]]]
        //let rightValue = [ANode [ALeaf; ANode[ALeaf; ANode[ALeaf; ANode[ALeaf]]]]]

        run path tables actions regexp getTag getName
        |> (fun x -> Assert.IsTrue <| compareRes x rightValue)

    [<Test>]
    member test.``Nonassociative test``() =
        let regexp     = GNESCC.Regexp_add.ruleToRegex
        let actions    = GNESCC.Actions_add.ruleToAction
        let tables     = GNESCCGenerator.Tables_add.tables
        let getTag     = GNESCCGenerator.Tables_add.getTag
        let getName    = GNESCCGenerator.Tables_add.getName
        let path = dir + "add/input.txt"
        let rightValue =
            [ANode[ANode[ANode[ALeaf];ALeaf;ANode[ALeaf]];ALeaf;ANode[ALeaf]];
             ANode[ANode[ALeaf];ALeaf;ANode[ANode[ALeaf];ALeaf;ANode[ALeaf]]]]
        
        run path tables actions regexp getTag getName
        |> (fun x -> Assert.IsTrue <| compareRes x rightValue)
