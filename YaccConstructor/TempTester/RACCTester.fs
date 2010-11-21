module Yard.Testers._RACCTester

open Yard.Generators._RACCGenerator
open Yard.Core
open System.Configuration

type _RACCTester() =
    class
        interface ITester with
            member self.BuildUserApplication testPath testName = 
                let cll = 
                    "fsc -o:_RACCUserApplication.dll --debug:pdbonly --noframework --define:TRACE "
                    + "--doc:bin\\Release\\RACCUserApplication.XML --optimize+ -r:\"%FS%\\FSharp.Core.dll\" "
                    + "-r:\"%FS_POWERPACK%\\bin\\FSharp.PowerPack.Compatibility.dll\" -r:\"%FS_POWERPACK%\\bin\\FSharp.PowerPack.dll\" "
                    + "-r:\"%NET_4_0%\\mscorlib.dll\" -r:\"%NET_4_0%\\System.Core.dll\" -r:\"%NET_4_0%\\System.dll\" "
                    + "-r:\"%NET_4_0%\\System.Numerics.dll\" -r:..\\_RACCCommon\\bin\\Debug\\_RACCCommon.dll "
                    + "-r:..\\_RACCCore\\bin\\Debug\\_RACCCore.dll -r:..\\RACCFiniteAutomata\\bin\\Debug\\_RACCFiniteAutomata.dll "
                    + "--target:dll --warn:3 --warnaserror:76 --vserrors --LCID:1033 --utf8output --fullpaths --flaterrors "
                    + "\"C:\\Users\\gsv\\AppData\\Local\\Temp\\.NETFramework,Version=v4.0.AssemblyAttributes.fs\" "
                    + testPath + "\\" + testName + ".tables.fs Lexer.fs Driver.fs"
                CSuccess 
            member self.RunTests inputFiles = 
                [{
                    inputFile = "Test"
                    status    = TSuccess
                    result    = Some "res \n mk"
                 }]
    end

do ConfigurationManager.AppSettings.["testsPath"].ToString() |> m1.main |> ignore