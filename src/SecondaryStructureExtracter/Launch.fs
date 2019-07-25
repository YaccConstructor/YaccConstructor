open System.IO
open System.Drawing
open System.Reflection
open BioParser
open Generation
open Argu

type CLIArguments = 
    | [<AltCommandLine("-g"); Mandatory>] Grammar of string
    | [<AltCommandLine("-i"); Mandatory>] Input_File of string
    | [<AltCommandLine("-f")>] Output_Formats of Output list
    | [<AltCommandLine("-o")>] Output_Dir of string
    interface IArgParserTemplate with
        member s.Usage = 
            match s with
            | Grammar _ -> "Specify a grammar which describes secondary structure fetures for extraction."
            | Input_File _ -> "Specify a path to file with sequences for processing."
            | Output_Formats _ -> "Specify output formats. Available options are CSV, BMP, TEX."
            | Output_Dir _ -> "Specify a folder for parsing output"
            
let getData path = 
    let lst = new ResizeArray<_>()
    let input = System.IO.File.ReadAllLines(path)
    for i in 0..2..input.Length - 1 do
        lst.Add((input.[i], input.[i+1]))
    lst.ToArray()   

let processInput inpPath grammar (formats: Output list) outDir =
    let mutable start = System.DateTime.Now
    Directory.CreateDirectory(outDir) |> ignore
    let data = getData inpPath
    let parser = new BioParser(grammar)
    System.Console.WriteLine(parser.StartNonTerm)
    let len = snd(data.[0]).Length
    data
    |> Array.iteri (fun i (id, seq) ->
        let parsed = parser.Parse seq
        formats
        |> List.iter (fun f -> 
            match f with
            | CSV ->            
                let csv = new CSV(id, len, parsed)              
                csv.Generate parser.StartNonTerm outDir
            | BMP ->
                let legend = [(parser.StartNonTerm, Color.Black)]
                let path = outDir
                Directory.CreateDirectory(path) |> ignore
                let img = new BMP(len, parsed)
                img.Generate legend (System.IO.Path.Combine(path, id.[1..] + ".bmp"))
            | TEX ->               
                let img = new TEX(len, parsed)
                img.Generate parser.StartNonTerm (outDir + id.[1..] + ".tex") seq
            | _ -> failwith("Unsupported output format")
            )
        if i % 10 = 0 then 
            printfn "%A sequences processed. Processing time = %A" i (System.DateTime.Now - start)
            start <- System.DateTime.Now
        )

[<EntryPoint>]
let main argv =
    
    let argParser = ArgumentParser.Create<CLIArguments>()
    let args = argParser.Parse argv
    let inputFile = args.GetResult(<@ Input_File @>)
    let grammar = args.GetResult(<@ Grammar @>)
    let outputFormats = args.GetResult(<@ Output_Formats @>, defaultValue=[Output.CSV])
    let defaultOutDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location) +
                        "/out"
    let outDir = args.GetResult(<@ Output_Dir @>, defaultValue=defaultOutDir)
    processInput inputFile grammar outputFormats outDir
    0