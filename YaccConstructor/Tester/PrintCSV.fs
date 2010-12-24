module PrintCSV

open System.IO

let print (outFile:string) sep data =
    let str = Seq.map (String.concat sep) data |> String.concat "\n"
    let outStrieam =         
        try            
            printf "\n%A\n" outFile
            let t = new FileInfo(outFile.Replace('/','\\'))
            let writer = t.CreateText()
            writer     
        with e -> failwith ("Writer Exception:" + e.ToString())
    outStrieam.Write str
    outStrieam.Close()    

