open Shuffle.Parsing
open NUnit.Framework
open NUnit.Framework

let testShuffleLinearInput grammars input = 
    //let timer = System.DateTime.UtcNow
    let isParsed = parseShuffledGrammarsLinearInput grammars input
    isParsed
    
    //printfn "Measured time is : %A" (timer - System.DateTime.UtcNow)

//A25 A29 A83 A65 A64 A66 A75 A14 A99
//let input = [| "A75"; "A14"; "A99"; "A68"; "A14"; "A65" ;"A64" ; "A66" ; "A7" |]//; "A74"; "A89"; "A56"; "A37"; "A43"; "A78"; "A88"; "A58"; "A33" |]
//let input = [| "A25"; "A29"; "A83"; "A65"; "A64"; "A66"; "A75"; "A14"; "A99" |]
        
[<TestFixture>]
type ``GLL shuffle tests`` () =
    [<Test>]
    member test.``2 simple grammars. Correct``() =
        let grammars = [|"../../grammars/D.yrd";
                         "../../grammars/M.yrd"|]
        let input = [|"D";"D";"M";"D";"M";"D"|]
        Assert.IsTrue(testShuffleLinearInput grammars input)
    
    //[<Test>]
    member test.``2 simple grammars. Uncorrect``() =
        let grammars = [|"../../grammars/D.yrd";
                         "../../grammars/M.yrd"|]
        let input = [|"D";"D";"D";"D";"D";"D"|]
        Assert.IsFalse(testShuffleLinearInput grammars input)

    [<Test>]
    member test.``one is extra``() =
        let grammars = [|"../../grammars/only_abc.yrd";
                         "../../grammars/only_cd.yrd";
                         "../../grammars/only_d.yrd";|]
        let input = [|"A";"B";"C";"D"|]
        Assert.IsTrue(testShuffleLinearInput grammars input)     
    
    //[<Test>]
    member test.``BaselineDomain grammars``() =
        let grammars = [|"../../grammars/D.yrd";
                         "../../grammars/M.yrd"|]
        let input = [|"D";"D";"M";"D";"M";"D"|]
        testShuffleLinearInput grammars input  