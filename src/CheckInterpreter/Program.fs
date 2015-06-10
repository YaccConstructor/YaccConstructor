open Microsoft.FSharp.Text.Lexing
open System.IO

let addString (line : string, file : string) =
    use wr = new StreamWriter(file, true)
    wr.Write(line)

let genStringPlus n = "0" + String.replicate n "+1";
let genStringMult n = "1" + String.replicate n "*1";

let testLoop file lowerBound step upperBound = 
    for i in lowerBound .. step .. upperBound do
        printf "n = %d \n" i
        let testStr = (genStringPlus (i / 4)) + "+" + (genStringMult (i / 4))
        addString (i.ToString() + ";", file)
        let mutable allDurations = "";
        for j in 1 .. 10 do
            let lexbuf = LexBuffer<_>.FromString testStr
            let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
            let y = ParserLR0.s LexerLR0.tokenize lexbuf
            stopWatch.Stop() //end time
            allDurations <- allDurations + stopWatch.Elapsed.TotalMilliseconds.ToString() + ";"
            printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds
        addString(allDurations, file)
        addString("\n", file)
    ()

let testLoopLR0 file lowerBound step upperBound = 
    for i in lowerBound .. step .. upperBound do
        printf "n = %d \n" i
        let testStr = (genStringPlus (i / 4)) + "+" + (genStringMult (i / 4))
        addString (i.ToString() + ";", file)
        let mutable allDurations = "";
        for j in 1 .. 10 do
            let lexbuf = LexBuffer<_>.FromString testStr
            let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
            let y = ParserLR0.s LexerLR0.tokenize lexbuf
            stopWatch.Stop() //end time
            allDurations <- allDurations + stopWatch.Elapsed.TotalMilliseconds.ToString() + ";"
            //printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds
        addString(allDurations, file)
        addString("\n", file)
    ()

let testLoopLR1 file lowerBound step upperBound = 
    for i in lowerBound .. step .. upperBound do
        printf "n = %d \n" i
        let testStr = (genStringPlus (i / 4)) + "+" + (genStringMult (i / 4))
        addString (i.ToString() + ";", file)
        let mutable allDurations = "";
        for j in 1 .. 10 do
            let lexbuf = LexBuffer<_>.FromString testStr
            let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
            let y = ParserLR1.s LexerLR1.tokenize lexbuf
            stopWatch.Stop() //end time
            allDurations <- allDurations + stopWatch.Elapsed.TotalMilliseconds.ToString() + ";"
            //printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds
        addString(allDurations, file)
        addString("\n", file)
    ()

let testLoopSLR1 file lowerBound step upperBound = 
    for i in lowerBound .. step .. upperBound do
        printf "n = %d \n" i
        let testStr = (genStringPlus (i / 4)) + "+" + (genStringMult (i / 4))
        addString (i.ToString() + ";", file)
        let mutable allDurations = "";
        for j in 1 .. 10 do
            let lexbuf = LexBuffer<_>.FromString testStr
            let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
            let y = ParserSLR1.s LexerSLR1.tokenize lexbuf
            stopWatch.Stop() //end time
            allDurations <- allDurations + stopWatch.Elapsed.TotalMilliseconds.ToString() + ";"
            //printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds
        addString(allDurations, file)
        addString("\n", file)
    ()

let testLoopLALR1 file lowerBound step upperBound = 
    for i in lowerBound .. step .. upperBound do
        printf "n = %d \n" i
        let testStr = (genStringPlus (i / 4)) + "+" + (genStringMult (i / 4))
        addString (i.ToString() + ";", file)
        let mutable allDurations = "";
        for j in 1 .. 10 do
            let lexbuf = LexBuffer<_>.FromString testStr
            let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
            let y = ParserLALR1.s LexerLALR1.tokenize lexbuf
            stopWatch.Stop() //end time
            allDurations <- allDurations + stopWatch.Elapsed.TotalMilliseconds.ToString() + ";"
            //printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds
        addString(allDurations, file)
        addString("\n", file)
    ()


let oneNmanyTimes file n times =
    printf "n = %d \n" n
    let testStr = (genStringPlus (n / 4)) + "+" + (genStringMult (n / 4))
    addString("duration" + "\n", file)
    for j in 1 .. times do        
        let lexbuf = LexBuffer<_>.FromString testStr
        let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
        let y = ParserLR1.s LexerLR1.tokenize lexbuf
        stopWatch.Stop() //end time
        addString(stopWatch.Elapsed.TotalMilliseconds.ToString() + "\n", file)
        printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds
    ()

let calcLR1Test str = 
    //===========LR(1)=================
    printfn "LR(0)"
    let lexbuf = LexBuffer<_>.FromString str
    let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
    let y = ParserCalcLR1.calc LexerCalcLR1.tokenize lexbuf
    stopWatch.Stop() //end time
    printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds

let calcLALR1Test str = 
    //===========LR(1)=================
    printfn "LALR(0)"
    let lexbuf = LexBuffer<_>.FromString str
    let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
    let y = ParserCalcLALR1.calc LexerCalcLALR1.tokenize lexbuf
    stopWatch.Stop() //end time
    printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds

let calcSLR1Test str = 
    //===========LR(1)=================
    printfn "SLR(0)"
    let lexbuf = LexBuffer<_>.FromString str
    let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
    let y = ParserCalcSLR1.calc LexerCalcSLR1.tokenize lexbuf
    stopWatch.Stop() //end time
    printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds


let calcStr = "x=5;y=x*2;z=((17*x+36*y)+(8-(4*y-x/10)))+250;" //40 tokens
let calcStrTokens = 40;

let repeate srt n = String.replicate n srt

let calcLoopLALR1 file lowerBound step upperBound =
    for i in lowerBound .. step .. upperBound do
        let len = i * calcStrTokens
        printf "n = %d \n" len
        let testStr = repeate calcStr i
        addString (len.ToString(), file)
        let mutable allDurations = ""
        for j in 1 .. 5 do
            let lexbuf = LexBuffer<_>.FromString testStr
            let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
            let y = ParserCalcLALR1.calc LexerCalcLALR1.tokenize lexbuf
            stopWatch.Stop() //end time
            allDurations <- allDurations + ";" + stopWatch.Elapsed.TotalMilliseconds.ToString()
            //printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds
        addString(allDurations, file)
        addString("\n", file)
    ()

let calcLoopLR1 file lowerBound step upperBound =
    for i in lowerBound .. step .. upperBound do
        let len = i * calcStrTokens
        printf "n = %d \n" len
        let testStr = repeate calcStr i
        addString (len.ToString(), file)
        let mutable allDurations = ""
        for j in 1 .. 5 do
            let lexbuf = LexBuffer<_>.FromString testStr
            let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
            let y = ParserCalcLR1.calc LexerCalcLR1.tokenize lexbuf
            stopWatch.Stop() //end time
            allDurations <- allDurations + ";" + stopWatch.Elapsed.TotalMilliseconds.ToString()
            //printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds
        addString(allDurations, file)
        addString("\n", file)
    ()

let calcLoopSLR1 file lowerBound step upperBound =
    for i in lowerBound .. step .. upperBound do
        let len = i * calcStrTokens
        printf "n = %d \n" len
        let testStr = repeate calcStr i
        addString (len.ToString(), file)
        let mutable allDurations = ""
        for j in 1 .. 5 do
            let lexbuf = LexBuffer<_>.FromString testStr
            let stopWatch = System.Diagnostics.Stopwatch.StartNew() //start time
            let y = ParserCalcSLR1.calc LexerCalcSLR1.tokenize lexbuf
            stopWatch.Stop() //end time
            allDurations <- allDurations + ";" + stopWatch.Elapsed.TotalMilliseconds.ToString()
            //printfn "Duration: %f ms" stopWatch.Elapsed.TotalMilliseconds
        addString(allDurations, file)
        addString("\n", file)
    ()

[<EntryPoint>]
let main args =
    calcLALR1Test calcStr
    (*let fileLR0 = "C:\\Users\\Ekaterina\\Desktop\\Calc_PC\\LR0_TestTime.csv"
    testLoopLR0 fileLR0 2000 100000 10000000
    *)

    (*let fileLR1 = "C:\\Users\\Ekaterina\\Desktop\\Calc_PC\\LR1_TestTime.csv"
    addString("n;duration1;duration2;duration3;duration4;duration5\n", fileLR1)
    calcLoopLR1 fileLR1 2000000 10000 3000000
    *)

    (*let fileLALR1 = "C:\\Users\\Ekaterina\\Desktop\\Calc_PC\\LALR1_TestTime.csv"
    addString("n;duration1;duration2;duration3;duration4;duration5\n", fileLALR1)
    calcLoopLALR1 fileLALR1 2000000 10000 2180000
    *)

    (*let fileSLR1 = "C:\\Users\\Ekaterina\\Desktop\\Calc_PC\\SLR1_TestTime.csv"
    addString("n;duration1;duration2;duration3;duration4;duration5\n", fileSLR1)
    calcLoopSLR1 fileSLR1 2000000 10000 2180000
    *)   
   
    (*let fileLR1 = "C:\\Users\\Ekaterina\\Desktop\\Calc_PC\\LR1_TestTime.csv"
    addString("n;duration1;duration2;duration3;duration4;duration5;duration6;duration7;duration8;duration9;duration10\n", fileLR1)
    calcLoopLR1 fileLR1 1 1000 265001
    *)

    (*let fileSLR1 = "C:\\Users\\Ekaterina\\Desktop\\Calc_PC\\SLR1_TestTime.csv"
    addString("n;duration1;duration2;duration3;duration4;duration5\n", fileSLR1)
    calcLoopSLR1 fileSLR1 1 1000 265001
    *)
    
    (*let fileLALR1 = "C:\\Users\\Ekaterina\\Desktop\\Calc_PC\\LALR1_TestTime.csv"
    addString("n;duration1;duration2;duration3;duration4;duration5;duration6;duration7;duration8;duration9;duration10\n", fileLALR1)
    calcLoopLALR1 fileLALR1 1 1000 265001
    *)
    0
