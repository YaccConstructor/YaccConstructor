open System.IO
open System.Drawing
open System.Reflection
open BioParser
open Generation
open Argu

type CLIArguments = 
    | [<AltCommandLine("-g"); Mandatory>] Grammar of string
    | [<AltCommandLine("-i"); Mandatory>] Input_File of string
    | [<AltCommandLine("-l"); Mandatory>] Sequence_Length of int
    | [<AltCommandLine("-f")>] Output_Formats of Output list
    interface IArgParserTemplate with
        member s.Usage = 
            match s with
            | Grammar _ -> "Specify a grammar which describes secondary structure fetures for extraction."
            | Input_File _ -> "Specify a path to file with sequences for processing."
            | Sequence_Length  _ -> "Set a sequence length."
            | Output_Formats _ -> "Specify output formats. Available options are CSV, BMP, TEX."
            
let getData path = 
    let lst = new ResizeArray<_>()
    let input = System.IO.File.ReadAllLines(path)
    for i in 0..3..input.Length - 1 do
        lst.Add((input.[i], input.[i+1], input.[i+2]))
    lst.ToArray()   

let processInput inpPath grammar len (formats: Output list) =
    let mutable start = System.DateTime.Now
    let outDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location) +
                 "/out_" + System.DateTime.Now.ToString("dd/MM/yyyy") + "_" + len.ToString() + "/"
    Directory.CreateDirectory(outDir) |> ignore
    printfn "output directory: %s" outDir
    let data = getData inpPath
    let parser = new BioParser(grammar)
    data
    |> Array.iteri (fun i (id, cls, seq) ->
        let parsed = parser.Parse seq
        formats
        |> List.iter (fun f -> 
            match f with
            | CSV ->            
                let csv = new CSV(id, cls, len, parsed)              
                csv.Generate parser.StartNonTerm outDir
            | BMP ->
                let legend = [(parser.StartNonTerm, Color.Black)]
                let path = outDir + cls + "/" 
                Directory.CreateDirectory(path) |> ignore
                let img = new BMP(len, parsed)
                img.Generate legend (path + id.[1..] + ".bmp")
            | TEX ->               
                let img = new TEX(len, parsed)
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
    let inputFile = args.GetResult(<@ Input_File @>)
    let grammar = args.GetResult(<@ Grammar @>)
    let len = args.GetResult(<@ Sequence_Length @>)
    let outputFormats = args.GetResult(<@ Output_Formats @>, defaultValue=[Output.CSV])
    
    processInput inputFile grammar len outputFormats
    0