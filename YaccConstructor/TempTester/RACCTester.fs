namespace Yard.Testers._RACCTester

open  Yard.Generators._RACCGenerator
open Yard.Core

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