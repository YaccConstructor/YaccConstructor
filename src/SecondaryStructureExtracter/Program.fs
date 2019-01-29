module Program
//open System.IO
//open System.Drawing
//
//open Drawing
//open BioParser
//open DataPreprocessing
//open YC.Frontends.YardFrontend
//open YC.Core
//open IL
//
//open Yard.Core
//open System.Collections.Generic
//
//let formatOutCSVString meta arr flg =
//        [
//            yield "\"" + meta + "\""
//            for i in arr -> uint32 i |> string
//            yield flg
//        ]    
//        |> String.concat ","
//        |> fun x -> x + "\n"
//
//let positiveToUIntArray isGpu fastaFile sortNum outFilePath (parser:BioParser) legend =
//    //let fe = new YardFrontend()
//    let data = getDataFrom16sBase fastaFile sortNum
//    let mutable start = System.DateTime.Now
//    let mutable cnt = 0
//    data
//    |> fun x -> 
//        printfn "L=%A" x.Length
//        x
//    |> Array.iteri (fun i (id,gen) ->
//        printfn "gene %A len:%A" i (gen.Length)
//        let name = sprintf "%s_%i_%s_%A.bmp" (id.Substring(1,6).Trim('[').Trim(']')) i (string id.[(String.length id) - 1]) (System.DateTime.Now.ToFileTime())
//        //let path = "../../positive/" + ([for i in 1..sortNum - 1 -> id.Split().[i]] |> String.concat("/")) + "/" 
//        //Directory.CreateDirectory(path) |> ignore
//        let parsResult = parser.Parse isGpu gen
//        //let picture = toIntArray 110 "s1" 
//        let pict = new ParsingPicture(gen.Length, parsResult) 
//
//        pict.Draw(legend, (name))
////        formatOutCSVString id picture "p"
////        |> fun x -> System.IO.File.AppendAllText(outFilePath, x)
//        cnt <- cnt + 1
//        if cnt % 10 = 0 then 
//            printfn "processing time = %A" (System.DateTime.Now - start)
//            start <- System.DateTime.Now
//        )
////        if gen.Length >= 512 then 
////            let picture =
//// toIntArray 512 "s1" (parser.Parse isGpu (gen.Substring(i,512)))  
////            formatOutCSVString (outFilePath + id.Split().[0]) picture "\"p\""
////            //|> fun x -> System.IO.File.AppendAllText("outGreenGenes.csv",x)
////            |> fun x -> System.IO.File.AppendAllText(outFilePath, x)
////            )
//
//let drawPositiveExamples isGpu (legend:(string*Color) list) fastaFile sortNum (parser:BioParser)=
//    let data = getData fastaFile //getDataFrom16sBase fastaFile sortNum
//   // let path = "../../positive/"
//   // Directory.CreateDirectory("../../positive2/") |> ignore
//    for i in 0..5 do 
//        let (id, gen) = data.[i]       
//        let path = @"D:\YC\YaccConstructor\tests\Bio.Pictures\bin\Release\"  + (string i) + ".tex" // "../../positive/" + ([for i in 1..sortNum - 1 -> id.Split().[i]] |> String.concat("/")) + "/" 
//        printfn "path=%A"  path
//        //Directory.CreateDirectory(path) |> ignore
//        //if gen.Length >= 560 then 
//        let picture = new ParsingPicture (gen.Length, parser.Parse isGpu gen)
//        //picture.Draw(legend, path)
//        picture.DrawInTex("s1",gen,path)
////    let path = "../../positive/" + fst(legend.[0]) + ".bmp"
////    if (snd(data.[5000])).Length >= 560 then 
////            let picture = new ParsingPicture (560, parser.Parse isGpu (snd(data.[5000])))
////            picture.Draw(legend, path)
//
//
//let drawNegativeExamples isGpu minLength maxLength legend fastaFiles (parser:BioParser) =
//    let path = "../../negative/"
//    let random = System.Random()
//    Directory.CreateDirectory(path) |> ignore
//    for f in fastaFiles do
//        let id, gen, intervals16s = getCompleteGenomeData f
//        let filteredGen = removeIntervals gen intervals16s
//        for i in 0 .. 10 .. filteredGen.Length - maxLength - 1 do
//            let length = random.Next(minLength, maxLength)
//            let name = sprintf "%s_%i_%i.bmp" id i length
//            let picture = new ParsingPicture(length, parser.Parse isGpu filteredGen.[i .. i + length - 2])
//            picture.Draw(legend, (path + name))
//
//[<EntryPoint>]
//let main argv = 
//    let inputPath = argv.[0]
//    let outFilePath = argv.[1]
//    let grammar = argv.[2]
//    let fe = new YardFrontend()
//    let loadIL = fe.ParseGrammar(grammar) 
//    let rules = loadIL.grammar.[0].rules
//    let parser = new BioParser(grammar)
//    let nonterms = HashSet<string>() 
//    for rule in rules do
//
//        if rule.name.text.Contains("h") || rule.isStart 
//        then nonterms.Add(rule.name.text) |> ignore   
//    let nonterms = nonterms |> Seq.toList
//   // let colors = [Color.Red; Color.Blue; Color.Green; Color.HotPink; Color.Violet; Color.Orange; Color.Brown; Color.Black]
////    let tmp = colors |> List.map(fun x -> x * 3) //|> Seq.toList
////    let RGBcolors = getCombinations [tmp; tmp; tmp] |> List.map (fun x -> Color.FromArgb(x.[0],x.[1],x.[2]))
//    let legend = [for i in 0..nonterms.Length-1 -> 
//                                                    match nonterms.[i] with
//                                                    |"s1" -> (nonterms.[i],Color.Black)
//                                                    //|"s0" -> (nonterms.[i],Color.Red)
//                                                    |_ -> (nonterms.[i],Color.White)
//                                                    ]
//    //let genomeFiles = 
//    //"C:/Users/User/Desktop/folder/YaccConstructor/tests/data/bio/complete_genome/"
//        //Directory.GetFiles(inputPath, "*.txt", SearchOption.AllDirectories)
// //   let legend = [(parser.StartNonTerm, Color.Black)]
//    //drawNegativeExamples true 560 560 legend genomeFiles
//    //for i in 0..legend.Length-1 do
//    
//   
//   //rawPositiveExamples true legend  "C:/Users/User/Desktop/folder/YaccConstructor/tests/Bio.Pictures/SILVA_128_SSURef_Nr99_tax_silva_first_500k_lines.fasta" 2
//   
//    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
//
//    //let inputPath = @"data_filtered_selected.fasta"
//
//    positiveToUIntArray true inputPath 1 outFilePath parser legend//"C:/Users/User/Desktop/folder/GG/gg_16s_format.fasta" 2//"C:/Users/User/Desktop/folder/YaccConstructor/tests/Bio.Pictures/SILVA_128_SSURef_Nr99_tax_silva_first_500k_lines.fasta" 2
//    stopWatch.Stop()
//    printfn "%f" stopWatch.Elapsed.TotalMilliseconds
//    System.Console.ReadKey() |> ignore
//    //picts: C:\Users\User\Downloads\data_filtered_selected.fasta 
//    //negativeToUintArray true 512 512 genomeFiles outFilePath
//    //@"D:\YC\YaccConstructor\tests\data\bio\trna\semples"
//    //drawPositiveExamples true legend @"D:\YC\YaccConstructor\tests\data\bio\trna\semples" 1 parser
//    0