//let tester = Yard.Generators.RecursiveAscent.RACCTester((*s :?> _*))
//let s = tester.RunTest

// Maybe we should use fsx?
// http://blogs.msdn.com/b/chrsmith/archive/2008/09/12/scripting-in-f.aspx
// Right-click it in Windows Explorer and choose "Run with F# Interactive.."

open System.IO
open System.Diagnostics
open System.Text.RegularExpressions
open System.Configuration
open Yard.Core

type Target =
    {
        frontend  : Option<string>
        generator : Option<string>
        tester    : Option<string>
    }

let mainApp = ConfigurationManager.AppSettings.["mainApp"].ToString()
let mainPath = ConfigurationManager.AppSettings.["mainPath"].ToString()
let testsPath = ConfigurationManager.AppSettings.["testsPath"].ToString()
let tmpPath = ConfigurationManager.AppSettings.["tmpPath"].ToString()
let targets =
    let _targets = ConfigurationManager.AppSettings.["targets"].ToString()
    _targets.Split [|';'|]
    |> Seq.map (fun s -> s.Replace("(","") |> (fun s -> s.Replace(")","")))
    |> Seq.map (fun s -> s.Split [|','|])
    |> Seq.fold 
        (fun buf lst ->
            let newTarget f g t =
                {
                    frontend  = f
                    generator = g
                    tester    = t
                }
            match List.ofArray lst with
            | []         -> buf
            | f :: []    -> newTarget (Some f) None None :: buf
            | f::g::[]   -> newTarget (Some f) (Some g) None :: buf
            | f::g::t::_ -> newTarget (Some f) (Some g) (Some t) :: buf)
        []
        

let outStrieam =         
    try
        let tail = "_" + System.DateTime.Now.ToString().Replace('/','_').Replace(' ','_').Replace(':','.') + ".csv"
        let t = new FileInfo(ConfigurationManager.AppSettings.["outPath"].ToString() + tail)
        let writer = t.CreateText()             
        writer     
    with e -> failwith ("Writer Exception:" + e.ToString())
                         
let write (str:string) = outStrieam.WriteLine(str)

let closeOutStream _ = outStrieam.Close()


let shellExecute program args =
    let startInfo = new ProcessStartInfo()
    let callResult = ref true
    startInfo.FileName <- program
    startInfo.Arguments <- args
    startInfo.UseShellExecute <- false
    startInfo.CreateNoWindow <- true
    startInfo.WorkingDirectory <- Path.GetFullPath(mainPath)
    startInfo.RedirectStandardError <- true

    let proc = Process.Start(startInfo)
    proc.BeginErrorReadLine()
    proc.ErrorDataReceived.AddHandler(new DataReceivedEventHandler(fun sendingProcess eventArgs ->
        if eventArgs.Data <> null then callResult := false
    ))
    proc.WaitForExit()
    !callResult

let testFiles = 
    seq {yield! Directory.GetFiles(testsPath, "*" ,SearchOption.AllDirectories)}
    |> Seq.map (fun path -> Path.GetFileName path, Path.GetDirectoryName path)
    |> Seq.filter (fun (name,path) -> Regex.Match(name, "test(.*)\.yrd$").Success) 

let loadInputs path =
    seq {yield! Directory.GetFiles(path)}
    |> Seq.map (fun path -> Path.GetFullPath path)
    |> Seq.filter (fun path -> Regex.Match(path, ".in$").Success) 

let loadTester (tName:string) =
    try
        let assembly = System.Reflection.Assembly.Load(tName)
        let inst = assembly.CreateInstance("Yard.Testers." + tName + "." + tName)
        Some (inst :?> ITester)
    with 
    |_ -> eprintf "\n Tester %A could not be loaded \n" tName
          None

let formatTestResults target feRes compileRes testRes grammarName =
    let getName x = 
        match x with
        | Some y -> y
        | None   -> "Undefined"
    let getCompileStatus = 
        match compileRes with
        | CSuccess -> ("SUCCEDED","")
        | CFail msg -> ("FAILED",msg) 

    let getTestStatus tStatus = 
        match tStatus with
        | TSuccess    -> ("SUCCEDED","")
        | TFail msg   -> ("FAILED",msg) 
        | TNoIncluded -> ("NOINCLUDED","")

    let formatAppTestRes res =
        let s,msg = getTestStatus res.status
        res.inputFile + " | " + s + " | " + msg + "|\"" + (if Option.isNone res.result then "" else res.result.Value) + "\"|\n"

    let res =
          " | \n" 
        + " Frontend | " + getName target.frontend + " |\n"
        + " Generator | " + getName target.generator + " |\n"
        + " Tester | " + getName target.tester + " |\n"
        + " Grammar | " + grammarName + " |\n"
        + " Generation result |\n"
        + (if feRes  then "SUCCEDED" else "FAILED") + " |\n"
        +
            if (target.tester.IsSome)
            then
                  " Compilation result |\n"
                + " " + fst getCompileStatus + " | " + snd getCompileStatus + " |\n"
                + " Application tests result |\n"
                + "Input file name | Test status | Message | Result |\n"
                + (List.map formatAppTestRes testRes |> String.concat "")
            else
                ""
    res
         

let () = 
    targets
    |> Seq.iter
        (fun target ->
            Seq.iter 
                (fun (testName, testPath) -> 
                    let args = 
                        match target.frontend with
                        | Some f -> " -f " + f
                        | None -> ""
                        +
                        match target.generator with
                        | Some g -> " -g " + g
                        | None -> ""
                        + " -t " + testName + " --testpath " + testPath
                    let feRes = shellExecute mainApp args
                    printf "%-40s %s\n" testName (if feRes  then "SUCCEDED" else "FAILED")
                    if feRes
                    then
                        match target.tester with
                        | Some t1 ->                        
                            match loadTester t1 with
                            | Some t ->
                                let cRes = t.BuildUserApplication testPath testName 
                                let res = loadInputs testPath |> List.ofSeq |> t.RunTests
                                formatTestResults target feRes cRes res  (testPath + "\\" + testName) 
                                |> write
                            | None -> 
                                formatTestResults 
                                    target feRes 
                                    ("Tester" +  t1 + " could not be loaded" |> CFail) 
                                    [] (testPath + "\\" + testName) 
                                |> write
                        | None  -> formatTestResults 
                                        target feRes 
                                        ("Tester is not specified" |> CFail) 
                                        [] (testPath + "\\" + testName) 
                                   |> write
                    else 
                        formatTestResults 
                            target feRes 
                            ("Generation fail" |> CFail) 
                            [] (testPath + "\\" + testName) 
                        |> write
            ) testFiles)
    closeOutStream ()   