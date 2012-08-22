module PerformanceTests

let private run (f,p) = 
    PTRunner.run f p

let Run () = ()
   (* List.iter run [RACCCoreTests.RACCPerformanceTests.Test1]*)