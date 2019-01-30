open System.IO
open System.Drawing
open BioParser
open Generation
open Argu

type CLIArguments = 
    | [<AltCommandLine("-g"); Mandatory>] Grammar of string
    | [<AltCommandLine("-i"); Mandatory>] InputFile of string
    | [<AltCommandLine("-o")>] OutputPath of string
    | [<AltCommandLine("-t")>] OutputTypes of Output list
    interface IArgParserTemplate with
        member s.Usage = 
            match s with
            | Grammar _ -> "Specify a grammar which describe secondary structure fetures for extraction."
            | InputFile _ -> "Specify a path to file with sequences for processing. File should be in FASTA format."
            | OutputPath _ -> "Specify a file for vectors or path to root directori for figures."
            | OutputTypes _ -> "Specify output formats. Available options are CSV, BMP, TEX"
            
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
        formats
        |> List.iter (fun f -> 
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
            )
        if i % 10 = 0 then 
            printfn "processing time = %A" (System.DateTime.Now - start)
            start <- System.DateTime.Now
        )

[<EntryPoint>]
let main argv =
    
    let argParser = ArgumentParser.Create<CLIArguments>()
    let args = argParser.Parse argv
    
    let inputFile = args.GetResult(<@ InputFile @>)
    let grammar = args.GetResult(<@ Grammar @>)
    let outputFormats = args.GetResult(<@ OutputTypes @>, defaultValue=[CSV])
    
    processInput inputFile grammar 128 outputFormats
    0