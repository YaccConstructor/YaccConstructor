open System.IO
open System.Drawing
open BioParser
open Argu
open Generation

//type CLIArguments =
//    | Paths of inp_path:string * out_path:string
//    //| Format of flag:int
//
//with
//    interface IArgParserTemplate with
//        member s.Usage =
//            match s with
//            | Paths _ -> "specify wotking paths (input : output)."
//            //| Format _ -> "set an output format (1 for scv, 2 for pics, 3 for both)."
//

    
    
let getData path = 
    let lst = new ResizeArray<_>()
    let input = System.IO.File.ReadAllLines(path)
    for i in 0..3..input.Length - 1 do
        lst.Add((input.[i], input.[i+1], input.[i+2]))
    lst.ToArray()   


let processInput inpPath grammar len (formats: Output list) =
    let mutable start = System.DateTime.Now
    let outDir = "../../out_" + System.DateTime.Now.ToString("dd/MM/yyyy") + "_" + len.ToString() + "/"
    Directory.CreateDirectory(outDir) |> ignore
    let data = getData inpPath
    let parser = new BioParser(grammar)
    data
    |> Array.iteri (fun i (id, cls, seq) ->
        let parsed = parser.Parse seq
        for f in formats do
            match f with
            | CSVString ->
                let csv = new CSVString(id, cls, len, parsed)              
                csv.Generate parser.StartNonTerm outDir
            | BMPImage ->
                let legend = [(parser.StartNonTerm, Color.Black)]
                let path = outDir + cls + "/" 
                Directory.CreateDirectory(path) |> ignore
                let img = new BMPImage(len, parsed)
                img.Generate legend (path + id.[1..] + ".bmp")
            | TEXImage ->
                let img = new TEXImage(len, parsed)
                img.Generate parser.StartNonTerm (outDir + id.[1..] + ".tex") seq
            | _ -> failwith("Unsupported output format")
        if i % 10 = 0 then 
            printfn "processing time = %A" (System.DateTime.Now - start)
            start <- System.DateTime.Now
        )

[<EntryPoint>]
let main argv =
    let inpPath = "../../data/simple_test.txt"
    let grammar = "../../data/grammar.txt"
    processInput inpPath grammar 128 [Output.CSVString; Output.BMPImage; Output.TEXImage]
    0