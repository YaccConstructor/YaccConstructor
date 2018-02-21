// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.IO
open System.Drawing

open Drawing
open BioParser
open DataPreprocessing
open Yard.Frontends.YardFrontend
open Yard.Core.IL.Rule
open Yard.Core.IL.Production
open Yard.Core
open System.Collections.Generic

let getElements rule = 
    match rule.body with 
    | PSeq(e, a, l) -> e 
    | PRef(t, a) -> [Conversions.TransformAux.createDefaultElem(PRef(t, a))]
    | PToken t -> [Conversions.TransformAux.createDefaultElem(PToken t)]

let rec getCombinations nonterms =
    match nonterms with
    | h::[] -> List.fold (fun acc elem -> [elem]::acc) [] h
    | h::t -> List.fold (fun cacc celem -> (List.fold (fun acc elem -> (elem::celem)::acc) [] h) @ cacc) [] (getCombinations t)
    | _ -> []


let formatOutCSVString meta arr flg =
        [
            yield "\"" + meta + "\""
            for i in arr -> uint32 i |> string
            yield flg
        ]    
        |> String.concat ","
        |> fun x -> x + "\n"

let negativeToUintArray isGpu minLength maxLength fastaFiles outFilePath (parser:BioParser) =    
    let path = "../../negative/"
    let random = System.Random()
    let cnt = ref 0//18211
    let cntF = ref 0
    Directory.CreateDirectory(path) |> ignore
    for f in fastaFiles do
        incr cntF
        let id, gen, intervals16s = getCompleteGenomeData f
        let filteredGen = removeIntervals gen intervals16s
        for i in 0 .. 50 .. filteredGen.Length - maxLength - 1 do
            //if !cnt < 2611 
            //then ()
            //else
            let length = random.Next(minLength, maxLength)
            let name = sprintf "%s_%i_%i" id i length
            let str = filteredGen.[i .. i + length - 2]
            let start = System.DateTime.Now
            let parsed = parser.Parse isGpu str            
            let picture = toIntArray length "s0" parsed
            formatOutCSVString name picture "\"n\""
            |> fun x -> System.IO.File.AppendAllText(outFilePath, x)
            printfn "processing time = %A" (System.DateTime.Now - start)
            printfn "file %A gene %A" !cntF !cnt             
            incr cnt


let positiveToUIntArray isGpu fastaFile sortNum outFilePath (parser:BioParser)=
    let fe = new YardFrontend()
    let data = getDataFrom16sBase fastaFile sortNum
    
    data
    |> fun x -> 
        printfn "L=%A" x.Length
        x
    |> Array.iteri (fun i (id,gen) ->
        printfn "gene %A" i
        //let path = "../../positive/" + ([for i in 1..sortNum - 1 -> id.Split().[i]] |> String.concat("/")) + "/" 
        //Directory.CreateDirectory(path) |> ignore
        //if gen.Length >= 1000 then 
        for i in 0 .. 20 .. gen.Length - 512 - 1 do
            let picture = toIntArray 512 "s0" (parser.Parse isGpu (gen.Substring(i,512)))  
            formatOutCSVString (id.Split().[0] + "_" + (string i)) picture "\"p\""
            |> fun x -> System.IO.File.AppendAllText(outFilePath, x)
            )

let drawPositiveExamples isGpu (legend:(string*Color) list) fastaFile sortNum (parser:BioParser)=
    let data = getDataFrom16sBase fastaFile sortNum
   // let path = "../../positive/"
   // Directory.CreateDirectory("../../positive2/") |> ignore
    for i in 0..50 do 
        let (id, gen) = data.[i]       
        let path = "../../positive/" + ([for i in 1..sortNum - 1 -> id.Split().[i]] |> String.concat("/")) + "/" 
        Directory.CreateDirectory(path) |> ignore
        if gen.Length >= 560 then 
            let picture = new ParsingPicture (560, parser.Parse isGpu gen)
            picture.Draw(legend, path + id.Split().[0] + ".bmp")
//    let path = "../../positive/" + fst(legend.[0]) + ".bmp"
//    if (snd(data.[5000])).Length >= 560 then 
//            let picture = new ParsingPicture (560, parser.Parse isGpu (snd(data.[5000])))
//            picture.Draw(legend, path)


let drawNegativeExamples isGpu minLength maxLength legend fastaFiles (parser:BioParser) =
    let path = "../../negative/"
    let random = System.Random()
    Directory.CreateDirectory(path) |> ignore
    for f in fastaFiles do
        let id, gen, intervals16s = getCompleteGenomeData f
        let filteredGen = removeIntervals gen intervals16s
        for i in 0 .. 10 .. filteredGen.Length - maxLength - 1 do
            let length = random.Next(minLength, maxLength)
            let name = sprintf "%s_%i_%i.bmp" id i length
            let picture = new ParsingPicture(length, parser.Parse isGpu filteredGen.[i .. i + length - 2])
            picture.Draw(legend, (path + name))

[<EntryPoint>]
let main argv = 
    let inputPath = argv.[0]
    let outFilePath = argv.[1]
    let grammar = argv.[2]
    let fe = new YardFrontend()
    let loadIL = fe.ParseGrammar(grammar) 
    let rules = loadIL.grammar.[0].rules
    let parser = new BioParser(grammar)
    let nonterms = HashSet<string>() 
    for rule in rules do

        if rule.name.text.Contains("h") || rule.isStart 
        then nonterms.Add(rule.name.text) |> ignore   
    let nonterms = nonterms |> Seq.toList
   // let colors = [Color.Red; Color.Blue; Color.Green; Color.HotPink; Color.Violet; Color.Orange; Color.Brown; Color.Black]
//    let tmp = colors |> List.map(fun x -> x * 3) //|> Seq.toList
//    let RGBcolors = getCombinations [tmp; tmp; tmp] |> List.map (fun x -> Color.FromArgb(x.[0],x.[1],x.[2]))
    let legend = [for i in 0..nonterms.Length-1 -> 
                                                    match nonterms.[i] with
                                                    |"h11" -> (nonterms.[i],Color.Red)
                                                    |"h17" -> (nonterms.[i],Color.Blue)
                                                    |"h10" -> (nonterms.[i],Color.Green)
                                                    |"h8" -> (nonterms.[i],Color.HotPink)
                                                    |"h13" -> (nonterms.[i],Color.HotPink)
                                                    |"h9" -> (nonterms.[i],Color.Violet)
                                                    |"h15" -> (nonterms.[i],Color.Violet)
                                                    |"h12" -> (nonterms.[i],Color.Orange)
                                                    |"h16" -> (nonterms.[i],Color.Orange)
                                                    |"h7" -> (nonterms.[i],Color.Orange)
                                                    |"h14" -> (nonterms.[i],Color.Brown)
                                                    |"h6" -> (nonterms.[i],Color.Brown)
                                                    |_ -> (nonterms.[i],Color.Black)
                                                    ]
    //let genomeFiles = 
    //"C:/Users/User/Desktop/folder/YaccConstructor/tests/data/bio/complete_genome/"
        //Directory.GetFiles(inputPath, "*.txt", SearchOption.AllDirectories)
 //   let legend = [(parser.StartNonTerm, Color.Black)]
    //drawNegativeExamples true 560 560 legend genomeFiles
    //for i in 0..legend.Length-1 do
    //rawPositiveExamples true legend  "C:/Users/User/Desktop/folder/YaccConstructor/tests/Bio.Pictures/SILVA_128_SSURef_Nr99_tax_silva_first_500k_lines.fasta" 2
    positiveToUIntArray true inputPath 2 outFilePath parser
    //negativeToUintArray true 512 512 genomeFiles outFilePath
    0