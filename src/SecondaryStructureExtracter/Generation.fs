module Generation

open BioParser
open System.Drawing

type Output =
    | CSV
    | BMP  
    | TEX 
    
type BMP(len, matrices: ParsingResult) =
    
    let drawLayer name color (bmp: Bitmap) =
        let matrix = matrices.[name].ToArray()
        for i in 0 .. len - 1 do
        for j in 0 .. len - 1 do
            if matrix.[i, j] <> 0.0
            then bmp.SetPixel (i, j, color)
            else bmp.SetPixel (i, j, Color.White)

    member this.Generate legend outPath =
        let bmp = new Bitmap(len, len)
        legend |> List.iter (fun (n, c) -> drawLayer (Util.NonTerminal n) c bmp)
        bmp.Save(outPath)
            
type CSV(id, len, matrices: ParsingResult) =
    
    let formatOutCSVString arr =
                [
                    yield "\"" + id + "\""
                    for i in arr -> uint32 i |> string
                ]    
                |> String.concat ","
                |> fun x -> x + "\n"
    
    let toIntArray nterm =
        let matrix = matrices.[nterm].ToArray()
        
        let bArray = new System.Collections.BitArray(len * len / 2,false)
        let mutable k = 0
        for i in 0 .. len - 1 do
            for j in i .. len - 1 do
                if matrix.[i, j] <> 0.0 && k < 24201
                then bArray.[k] <- true
                k <- k + 1
        let fInfo = typeof<System.Collections.BitArray>.GetField("m_array", System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Instance)
        fInfo.GetValue(bArray) :?> int[] 
    
    member this.Generate name outPath =
            let arr = toIntArray(Util.NonTerminal(name))
            formatOutCSVString arr
            |> fun x -> System.IO.File.AppendAllText(outPath + "out.csv", x)
    
type TEX(len, matrices: ParsingResult) =
    
    member this.Generate name outPath (seq: string) =
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

        let tblHeader = "\\begin{tabular}{ " + (Array.init (len + 1) (fun _ -> "c") |> String.concat " ") + "|} \n \\hline \n"

        let mutable res = ""
        let nterm = Util.NonTerminal name
        let matrix = matrices.[nterm].ToArray()
        for i in 0 .. len - 1 do
            for j in 0 .. len - 1 do
                if i < j
                then 
                    if matrix.[i, j] <> 0.0
                    then res <- res + "\cellcolor{blue!25} 1 " 
                    else res <- res + "0 "
                    
                elif i = j then res <- res + "\\textbf{" + string seq.[i] + "} "
                if j <> len - 1 then res <- res + "&"
            res <- res + "\n \\\\  \n"
        System.IO.File.WriteAllText(outPath, preheader + tblHeader + res + footer)