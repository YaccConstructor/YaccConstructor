module Drawing

open System.Collections.Generic
open System.Drawing

open MatrixKernels
open BioParser

let toIntArray dim ntermName (nonTermLayers: ParsingResult) =
    let matrix = 
        match nonTermLayers with
        | CPU dict -> dict.[Util.NonTerminal ntermName].ToArray()
        | GPU dict -> dict.[Util.NonTerminal ntermName].ToArray()
    let bArray = new System.Collections.BitArray(dim * dim / 2,false)
    for i in 0 .. dim - 1 do
        for j in i .. dim-1 do
            if matrix.[i, j] <> 0.0
            then bArray.[i*(i+1)/2 + (dim - 1 - j)] <- true
    let fInfo = typeof<System.Collections.BitArray>.GetField("m_array", System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Instance)
    fInfo.GetValue(bArray) :?> int[] 

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