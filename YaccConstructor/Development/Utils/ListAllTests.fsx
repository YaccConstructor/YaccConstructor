#I "../../Main/bin/Release/"

#r "Common.dll"
#r "YardFrontend.dll"
#r "GNESCCGenerator.dll"
#r "FSharp.PowerPack"

open Yard.Core
open System.IO
open Microsoft.FSharp.Text.StructuredFormat
open Microsoft.FSharp.Text.StructuredFormat.LayoutOps

let outPath = "Generated.fs"

#load "Test.fsx"
open Test

let basePath = "../../../Tests/GNESCC"
let ARf = "GNESCC.Actions"
let TRf = "GNESCCGenerator.Tables"
let RRf = "GNESCC.Regexp"


let listAllFiles ext = 
    System.IO.Directory.GetFiles(basePath, ext, System.IO.SearchOption.AllDirectories)
    |> Seq.map (fun (s:string) ->s.Replace("\\","/") ) 
let listLocalFiles path ext = 
    System.IO.Directory.GetFiles(path, ext)
    |> Seq.map (fun (s:string) ->s.Replace("\\","/") ) 
let printFiles lst = Seq.iter (printfn "%A") lst
let composeTests =
    let grammars = listAllFiles "*.yrd" 
    //printFiles grammars
    grammars
    |>Seq.map
        ( fun p ->
            let inputs = 
                listLocalFiles (Path.GetDirectoryName p) "*.in"
                |> List.ofSeq
            let grName = Path.GetFileName p
            let suffix = grName.Split('.').[0]
            let lexer = 
                listLocalFiles (Path.GetDirectoryName p) "*.fsl"
                |> List.ofSeq
                |> function | hd::tl -> hd | [] -> ""            
            {
                FullGrammarPath     = p
                FullLexerPath       = lexer
                FullInputFilesPaths = inputs
                ActionReplacement   = ARf,ARf + "_" + suffix
                TablesReplacement   = TRf,TRf + "_" + suffix
                RegexpReplacement   = RRf,RRf + "_" + suffix
            })

let layoutTest test = 
    [
        [ "FullGrammarPath = "
          ; "\"" + test.FullGrammarPath + "\"" 
        ]
        |> List.map wordL         
        |> spaceListL
        ;
         ("FullInputFilesPaths = " |> wordL)
         @@--
         (test.FullInputFilesPaths 
          |> List.map (fun x -> "\""+x+"\"" |> wordL)
          |> semiListL       
          |> fun l -> [ wordL "["; l; wordL "]"] 
          |> spaceListL)
        ;
         [ "FullLexerPath = "
          ; "\"" + test.FullLexerPath + "\"" 
         ]
         |> List.map wordL         
         |> spaceListL
        ;
         [ wordL "ActionReplacement = " 
          ; [ wordL ("\"" + fst test.ActionReplacement + "\"")                     
            ; wordL ("\"" + snd test.ActionReplacement + "\"")
            ] |> tupleL
         ]
         |> spaceListL
        ;
         [ wordL "RegexpReplacement = " 
          ; [ wordL ("\"" + fst test.RegexpReplacement + "\"")                     
            ; wordL ("\"" + snd test.RegexpReplacement + "\"")
            ] |> tupleL
         ]
         |> spaceListL
        ;

         [ wordL "TablesReplacement = "
          ; [ wordL ("\"" + fst test.TablesReplacement + "\"")                     
            ; wordL ("\"" + snd test.TablesReplacement + "\"")
            ] |> tupleL
         ]
         |> spaceListL
    ]
    |> aboveListL    
    |> fun l -> [("{" |> wordL)  @@-- l ;"};" |> wordL] |> spaceListL 
    
let layoutTests =
    wordL "let tests = "
    @@--
    (List.ofSeq composeTests
     |> List.map layoutTest 
     |> aboveListL
     |> squareBracketL)
    |> Display.layout_to_string {FormatOptions.Default with PrintWidth = 120}

let test grammarPath inputPaths = 1
    
do printfn "%s" layoutTests
