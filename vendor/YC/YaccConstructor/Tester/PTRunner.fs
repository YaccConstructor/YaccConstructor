module PTRunner

open System.IO
open System.Text.RegularExpressions

let run testFun inFolder = 
    let startTime = ref System.DateTime.Now
    let endTime = ref System.DateTime.Now
    let files =         
        seq {yield! Directory.GetFiles(inFolder)}
        |> Seq.map (fun path -> Path.GetFullPath path)
        |> Seq.filter (fun path -> Regex.Match(path, ".in$").Success)
    let idToFileMap =
        Seq.map 
            (fun (x:string) -> 
                x.Split '_'
                |> fun x -> x.[Array.length x - 1]
                |> fun (s:string) -> s.Split '.'
                |> fun x -> x.[0]
                |> int
                |> fun id -> (id,x) 
            ) 
            files
        |> Seq.sortBy fst

    let resultBuf = ref []
    let formatDataTime dt =
        dt.ToString().Replace('/','_').Replace(':','_')
    Seq.map 
        (fun (id,path) -> 
            startTime := System.DateTime.Now
            let result = testFun path
            resultBuf := [id.ToString();result.ToString()]::!resultBuf
            endTime := System.DateTime.Now
            let t = (!endTime - !startTime)
            printf "\n%A" path
            [id.ToString();t.Hours*3600*1000 + t.Minutes*60*1000 +  t.Seconds*1000 + t.Milliseconds |> string])
        idToFileMap
    |> PrintCSV.print (inFolder  + "/testRes_" + formatDataTime System.DateTime.Now + ".out")  " " 

    PrintCSV.print (inFolder  + "/testRes_" + formatDataTime System.DateTime.Now + ".result.out")  " " !resultBuf
    