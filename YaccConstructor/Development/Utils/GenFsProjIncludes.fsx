#I "../../Main/bin/Release/"
#r "Common.dll"
#r "YardFrontend.dll"
#r "GNESCCGenerator.dll"

let outFile = "tests.fsproj.inc"

open Yard.Core
open System.IO

#load "Test.fsx"
open Test

#load "Tests.fsx"
open Tests

let writeAllLines file (lines: string seq) =
    System.IO.File.WriteAllLines(file,lines,System.Text.Encoding.UTF8)


let genInclude test =
    let compileInc inc = 
        "    <Compile Include= \"" + inc + "\" />"
    let fslex fsl =
          "    <FsLex Include=\"" + fsl + "\">\n"
        + "         <OtherFlags> --unicode </OtherFlags>\n"
        + "    </FsLex>"
    let tName = System.IO.Path.GetFileName test.FullGrammarPath
    let lexer = System.IO.Path.GetFileName test.FullLexerPath

 
    [compileInc (tName + ".tables.fs")
     ;compileInc (tName + ".regexp.fs")
     ;(if lexer <> "" then fslex lexer else "")
     ;(if lexer <> "" then compileInc (System.IO.Path.GetFileNameWithoutExtension lexer + ".fs") else "")
     ;compileInc (tName + ".actions.fs")]

do Seq.map genInclude tests
   |> Seq.concat
   |> writeAllLines outFile