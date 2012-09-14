module BasicUnitTest

open NUnit.Framework

(*[<TestFixture>]
type ``RACC core tests`` () =    
    inherit RACCCoreTests.``RACC core tests``()

[<TestFixture>]
type ``RACC parse error position tests`` () =    
    inherit RACCCoreTests.``RACC parse error position tests``()
    *)
(*[<TestFixture>] 
type ``Yard frontend tests`` () =    
    inherit YardFrontendTester .``YARD frontend Tests``()*)

[<TestFixture>]
type ``Irony frontend tests`` () =    
    inherit  IronyFrontendTests.``Irony frontend tests``()

[<TestFixture>]
type ``Convertions tests`` () =    
    inherit  ConvertionsTests.``Convertions tests``()

//[<TestFixture>]
//type ``GNESCC core tests`` () =    
//    inherit GNESCCCoreTests.``GNESCC core tests``()
//
//[<TestFixture>]
//type ``GNESCC generator tests`` () =    
//    inherit  GNESCGeneratorTests.``GNESCC generator test``()