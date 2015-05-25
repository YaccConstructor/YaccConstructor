open Microsoft.FSharp.Text.Lexing
open System.IO

let addString (line : string) =
    use wr = new StreamWriter("C:\\Users\\Ekaterina\\Desktop\\Calc\\10_exec\\LR1_TestTime.csv", true)
    wr.Write(line)

let genStringPlus n = "0" + String.replicate n "+1";
let genStringMult n = "1" + String.replicate n "*1";

let testLoop lowerBound step upperBound = 
    for i in lowerBound .. step .. upperBound do
        printf "n = %d \n" i
        let testStr = (genStringPlus (i / 4)) + "+" + (genStringMult (i / 4))
        addString (i.ToString() + ";")
        let mutable allDurations = "";
        for j in 1 .. 10 do
            let lexbuf = LexBuffer<_>.FromString testStr
            let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
            let y = ParserSLR1.s LexerSLR1.tokenize lexbuf
            stopWatch.Stop() //end time
            allDurations <- allDurations + stopWatch.Elapsed.TotalMilliseconds.ToString() + ";"
            printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds
        addString(allDurations)
        addString("\n")
    ()

let oneNmanyTimes n times =
    printf "n = %d \n" n
    let testStr = (genStringPlus (n / 4)) + "+" + (genStringMult (n / 4))
    addString("duration" + "\n")
    for j in 1 .. times do        
        let lexbuf = LexBuffer<_>.FromString testStr
        let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
        let y = ParserLR1.s LexerLR1.tokenize lexbuf
        stopWatch.Stop() //end time
        addString(stopWatch.Elapsed.TotalMilliseconds.ToString() + "\n")
        printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds
    ()

let oneTest str = 
    //let lexbuf = Lexing.LexBuffer<_>.FromString str
    //while not lexbuf.IsPastEndOfStream do printfn "%A" (LexerLR1.tokenize lexbuf)
    //===========LR(0)=================
    printfn "LR(0)"
    let lexbuf = LexBuffer<_>.FromString str
    let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
    let y = ParserLR0.s LexerLR0.tokenize lexbuf
    stopWatch.Stop() //end time
    printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds
    //===========LR(1)=================
    printfn "\nLR(1)"
    let lexbuf = LexBuffer<_>.FromString str
    let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
    let y = ParserLR1.s LexerLR1.tokenize lexbuf
    stopWatch.Stop() //end time
    printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds
    //===========SLR(1)=================
    printfn "\nSLR(1)"
    let lexbuf = LexBuffer<_>.FromString str
    let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
    let y = ParserSLR1.s LexerSLR1.tokenize lexbuf
    stopWatch.Stop() //end time
    printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds
    //===========LALR(1)=================
    printfn "\nLALR(1)"
    let lexbuf = LexBuffer<_>.FromString str
    let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
    let y = ParserLALR1.s LexerLALR1.tokenize lexbuf
    stopWatch.Stop() //end time
    printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds
    printfn "\n\nResult: %A" y

let LR1Test str = 
    //===========LR(1)=================
    printfn "LR(0)"
    let lexbuf = LexBuffer<_>.FromString str
    let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
    let y = ParserCalcLR1.calc LexerCalcLR1.tokenize lexbuf
    stopWatch.Stop() //end time
    printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds

let LALR1Test str = 
    //===========LR(1)=================
    printfn "LALR(0)"
    let lexbuf = LexBuffer<_>.FromString str
    let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
    let y = ParserCalcLALR1.calc LexerCalcLALR1.tokenize lexbuf
    stopWatch.Stop() //end time
    printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds

let calcStr = "x=5;y=x*2;z=((17*x+36*y)+(8-(4*y-x/10)))+250;" //40 tokens

let repeate srt n = String.replicate n srt

let calcLoop lowerBound step upperBound =
    for i in lowerBound .. step .. upperBound do
        printf "n = %d \n" i
        let testStr = repeate calcStr i
        addString (i.ToString() + ";")
        let mutable allDurations = "";
        for j in 1 .. 10 do
            let lexbuf = LexBuffer<_>.FromString testStr
            let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
            let y = ParserCalcLR1.calc LexerCalcLR1.tokenize lexbuf
            stopWatch.Stop() //end time
            allDurations <- allDurations + stopWatch.Elapsed.TotalMilliseconds.ToString() + ";"
            printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds
        addString(allDurations)
        addString("\n")
    ()

[<EntryPoint>]
let main args =
    calcLoop 1 10 25000
    //let n = 350000
    //let testStr = (genStringPlus (n / 4)) + "+" + (genStringMult (n / 4))
    //oneTest "1+1" //"1+1*1+1+0+1" //testStr
    //addString("n;duration1;duration2;duration3;duration4;duration5;duration6;duration7;duration8;duration9;duration10;\n")
    //testLoop 2000 4000 1000000
    //oneNmanyTimes(1000000)
    0
