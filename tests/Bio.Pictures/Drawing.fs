module Drawing

open System.Collections.Generic
open System.Drawing

open MatrixKernels
open BioParser

type ParsingPicture(dim, nonTermLayers: ParsingResult) =

    let drawLayer name color (bmp: Bitmap) = 
        let matrix = 
            match nonTermLayers with
            | CPU dict -> dict.[name].ToArray()
            | GPU dict -> dict.[name].ToArray()
        for i in 0 .. dim - 1 do
            for j in 0 .. dim - 1 do
                if matrix.[i, j] <> 0.0
                then bmp.SetPixel (i, j, color)

    member this.Draw(legend: (string * Color) list, output) = 
        let bmp = new Bitmap(dim, dim)
        legend |> List.iter (fun (n, c) -> drawLayer (Util.NonTerminal n) c bmp)
        bmp.Save(output)