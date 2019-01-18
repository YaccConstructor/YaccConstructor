﻿module Drawing

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
                else bmp.SetPixel (i, j, Color.White)

    member this.Draw(legend: (string * Color) list, output) = 
        let bmp = new Bitmap(dim, dim)
        legend |> List.iter (fun (n, c) -> drawLayer (Util.NonTerminal n) c bmp)
        bmp.Save(output)

    member this.DrawInTex (nterm, (str:string), output) =
        let preheader =
            """        \documentclass[a2paper,landscape]{article}
                % General document formatting
                \usepackage[margin=0.1in]{geometry}
                \usepackage[parfill]{parskip}
                \usepackage[utf8]{inputenc}
                \usepackage[table]{xcolor}
                % Related to math
                \usepackage{amsmath,amssymb,amsfonts,amsthm}

            \begin{document}
        
        """
        
        let footer = """
            \end{tabular}
            \end{document}"""

        let tblHeader = "\\begin{tabular}{ " + (Array.init (dim + 1) (fun _ -> "c") |> String.concat " ") + "|} \n \\hline \n"

        let mutable res = ""
        let name = Util.NonTerminal nterm
        let matrix = 
            match nonTermLayers with
            | CPU dict -> dict.[name].ToArray()
            | GPU dict -> dict.[name].ToArray()
        for i in 0 .. dim - 1 do
            for j in 0 .. dim - 1 do
                if i < j
                then 
                    if matrix.[i, j] <> 0.0
                    then res <- res + "\cellcolor{blue!25} 1 " 
                    else res <- res + "0 "
                    
                elif i = j then res <- res + "\\textbf{" + string str.[i] + "} "
                 //res <- res + "  & "
                if j <> dim - 1 then res <- res + "&"
            res <- res + "\n \\\\  \n"
        System.IO.File.WriteAllText(output, preheader + tblHeader + res + footer)

