module GLLAbstractParserCFTests

open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

open NUnit.Framework
open QuickGraph

open YaccConstructor.API
open Yard.Generators.GLL.AbstractParser
open Yard.Generators.GLL.AbstractParserCF
open Yard.Generators.GLL.ParserCommon
open AbstractAnalysis.Common

let grammarsDir =  @"./GLL.AbstractParserCF.Tests/" 

let getParserSource grammarFile =    
    generate (grammarsDir + grammarFile)
             "YardFrontend" "GLLGenerator" 
             None
             ["ExpandMeta"]
             [] :?> ParserSourceGLL

let convertNucleo n =
    let n = Char.ToUpper(n)
    if n = 'T'
    then 'U'
    elif Array.contains n [|'A';'C';'G';'U';|]
    then n
    else 'G'

let find16sInLinearInput (input: string) = 
    let parserSource16sMid = getParserSource "R16S_19_27.yrd"
    let tokenizeGenome (input: string) =
        let mkTokenizer strToToken ch = (convertNucleo ch).ToString() |> strToToken
        input.ToCharArray() |> Array.map (mkTokenizer parserSource16sMid.StringToToken)
    
    let linearInput = LinearInput([|for i in 0 .. input.Length - 1 -> i * 1<positionInInput>|], tokenizeGenome input)
    getAllRangesForStartStateWithLength parserSource16sMid linearInput

let toYardGr (path : string) (outPath : string) =    
    let lines = System.IO.File.ReadLines path
    use out = new System.IO.StreamWriter (outPath : string)
    out.WriteLine("[<Start>]")
    lines |> Seq.iter (fun line -> do
                            Regex.Replace(line, "->", ":") 
                            |> String.map (fun ch -> if ch <> ':' && not (Char.IsLetterOrDigit(ch)) && ch <> ' '
                                                     then '|'
                                                     else Char.ToUpper ch) 
                            |> fun x -> Regex.Replace(x, "(\d+)", "s$1") 
                            |> out.WriteLine
                      )
    out.Close()

let parseCF = Yard.Generators.GLL.AbstractParserCF.parse

let testCFParser grammar1 grammar2 =
    let source1, source2 = getParserSource grammar1, getParserSource grammar2
    parseCF source1 source2

[<TestFixture>]
type ``GLLAbstractCFParserTests`` () =
    
    // infinite loop (embedded recursion)
//    [<Test>]
//    member this.``Test_CF``() =        
//        let gss1, gss2, count = testCFParser "BracketsLeft.yrd" "BracketsRight.yrd"
//        gss1.ToDot (grammarsDir + "gss.dot")
//        gss2.ToDot (grammarsDir + "gss2.dot")
//        printfn "%i" count
//        Directory.GetCurrentDirectory() |> printfn "%s"        
//        Assert.IsTrue true

    [<Test>]
    member this.``Brackets``() =
        let gss1, gss2, count = testCFParser "BracketsLeft.yrd" "Brackets_nonrec.yrd" false
        let x = getAllCompleteRangesForState gss1 (0<positionInGrammar>) (1<positionInGrammar>)
        printfn ""
        x |> Seq.iter (fun (s, e) -> printf "(%i, %i); " s e)
        Assert.IsTrue true

    [<Test>]
    member this.``Cycle_brackets``() =
        let gss1, gss2, count = testCFParser "BracketsLeft.yrd" "Cycle_brackets.yrd" false
        let x = getAllCompleteRangesForState gss1 (0<positionInGrammar>) (1<positionInGrammar>)
        printfn ""
        x |> Seq.iter (fun (s, e) -> printf "(%i, %i); " s e)
        Assert.IsFalse false

    [<Test>]
    member this.``Simple_substr``() =
        let gss1, gss2, count = testCFParser "Simple_substr_t.yrd" "Simple_substr_d.yrd" false
        let x = getAllCompleteRangesForState gss1 (0<positionInGrammar>) (1<positionInGrammar>)
        printfn ""
        x |> Seq.iter (fun (s, e) -> printf "(%i, %i); " s e)
        Assert.IsFalse false

    [<Test>]
    member this.``Check_linear_16s``() =
        let source16s = new StreamReader(grammarsDir + "trainset.fa")
        let testData = [| for i in 0 .. 1 -> source16s.ReadLine() |> ignore; source16s.ReadLine() |]
        source16s.Close()
        
//        use out = new System.IO.StreamWriter (grammarsDir + "converted.txt")
//        let res = testData.[0].ToCharArray() |> Array.map convertNucleo
//        out.Write (new string(res))
//        out.Close()
        
        testData.[0] <- testData.[0].Substring(480, 600)
        let result = testData |> Array.map find16sInLinearInput 
        result |> Array.iter (Seq.iter (fun (s, e, l) -> printf "(%i, %i, %i) " s e l))

        Assert.IsTrue true

    [<Test>]
    member this.``Find_mid_in_compressed_16s``() =
        //toYardGr (grammarsDir + "output.txt") (grammarsDir + "compressed_16s.yrd")
        let gss1, gss2, count = testCFParser "R16S_19_27.yrd" "16s_compressed_480_1080.yrd" true
        let x = getAllRangesForState gss1 (524<positionInGrammar>) //(827<positionInGrammar>)
        printfn "%i, %i, %i" count gss1.VertexCount gss2.VertexCount
        x |> Seq.iter (fun (s, e) -> printf "(%i, %i); " s e)
        Assert.IsFalse false