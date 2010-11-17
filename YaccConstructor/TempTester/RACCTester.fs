module Yard.Testers._RACCTester

open Yard.Generators._RACCGenerator
open Yard.Core
open System.Configuration

type _RACCTester() =
    class
        interface ITester with
            member self.BuildUserApplication testPath testName = 
                let g = m1.main ""
                CSuccess 
            member self.RunTests inputFiles = 
                [{
                    inputFile = "Test"
                    status    = TSuccess
                    result    = Some "res \n mk"
                 }]
    end

do ConfigurationManager.AppSettings.["testsPath"].ToString() |> m1.main |> ignore