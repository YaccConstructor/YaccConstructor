#I "../../Main/bin/Release/"
#r "Common.dll"
#r "YardFrontend.dll"
#r "GNESCCGenerator.dll"

let outFile = @"../../GNESCCCore.Test/GNESCCCoreTests.fs"

open Yard.Core
open System.IO

#load "Test.fsx"
open Test

#load "Tests.fsx"
open Tests

let writeAllLines file (lines: string seq) =
    System.IO.File.WriteAllLines(file,lines,System.Text.Encoding.UTF8)

let readAllLines file =
    System.IO.File.ReadAllLines(file,System.Text.Encoding.UTF8)


let genInclude test =
    let tab = "    "
    let getName s =
        System.IO.Path.GetFileName(s).Split('.').[0]     
    seq
     {for inPath in  test.FullInputFilesPaths do
          let testName =  getName inPath
          let lexName = getName test.FullGrammarPath
          let resFile = inPath.Replace(".in", ".res")
          let rVal () = System.IO.File.ReadAllText(resFile)
          if System.IO.File.Exists(resFile) && rVal().Trim() <> ""
          then
             yield tab + "[<Test>]"
             yield tab + "member test." + testName + "() ="
             yield tab + tab + "let tables     = " + snd test.TablesReplacement + ".tables"
             yield tab + tab + "let regexp     = " + snd test.RegexpReplacement + ".ruleToRegex"
             yield tab + tab + "let actionsMap = " + snd test.ActionReplacement + ".ruleToAction"
             yield tab + tab + "let rightValue = " + rVal ()
             yield tab + tab + "let lexer      = fun buf -> Lexer_" + lexName + ".Lexer(buf):>ILexer"
             yield tab + tab + "let res = run @\"" + ("../" + inPath) + "\" tables actionsMap regexp lexer"
             yield tab + tab + "Assert.AreEqual(rightValue,res)"
             yield ""
     }

do 
   
   [
    yield! readAllLines "UnitTestsTemplate.fs"
    yield! 
      Seq.map genInclude tests
      |> Seq.concat
   ]
   |> writeAllLines outFile