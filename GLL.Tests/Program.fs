open YC.Frontends.YardFrontend
open YC.Parsing.GLL
open YC.Parsing.Common.GraphInput
open YC.Core.Conversions.ExpandMeta
open YC.Core.Conversions.ExpandEbnfStrict
open YC.Core
open YC.Parsing.Common.GLL
open YC.Parsing.GLL.AbstractParser
open YC.API

let grammarsDir =  @""

let getInput epsilonTag tokenizer file = 
    let tokens = 
        System.IO.File.ReadAllLines(file)
        |> Seq.map (fun s -> s.Split [|' '|] |> Seq.map tokenizer)
        |> Seq.concat
        |> Array.ofSeq

    new LinearIputWithErrors(tokens, epsilonTag, [||])

let getParserSource grammarFile conv = 
    let fe = new YardFrontend()
    let gen = new GLL()
    generate (System.IO.Path.Combine(grammarsDir, grammarFile))
             fe gen 
             None
             conv
             [|""|]
             [] :?> ParserSourceGLL

let run grammarFile inputFile =
    let conv = [new ExpandEbnf() :> Conversion; new ExpandMeta() :> Conversion]
    let parser = getParserSource grammarFile conv
    let start = System.DateTime.Now
    let input  = getInput parser.EpsilonInputTag parser.StringToToken inputFile
    let tree = buildAst parser input
    printfn "processing time = %A" (System.DateTime.Now - start)
    let n, e, t, amb = tree.CountCounters
    tree.AstToDot "out.dot"
    printfn "%A, %A" n e
    ()


[<EntryPoint>]
let main argv =
    // N LBR N PLUS N RBR
    run argv.[0] argv.[1] 
    0 // return an integer exit code
