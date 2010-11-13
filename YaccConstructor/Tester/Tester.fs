//let tester = Yard.Generators.RecursiveAscent.RACCTester((*s :?> _*))
//let s = tester.RunTest

// Maybe we should use fsx?
// http://blogs.msdn.com/b/chrsmith/archive/2008/09/12/scripting-in-f.aspx
// Right-click it in Windows Explorer and choose "Run with F# Interactive.."

open System.IO
open System.Diagnostics
open System.Text.RegularExpressions
open System.Configuration

let mainApp = ConfigurationManager.AppSettings.["mainApp"].ToString()
let mainPath = ConfigurationManager.AppSettings.["mainPath"].ToString()
let testsPath = ConfigurationManager.AppSettings.["testsPath"].ToString()
let tmpPath = ConfigurationManager.AppSettings.["tmpPath"].ToString()

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

let () = 
    Seq.iter (fun (testName, testPath) -> 
       printf "%-40s %s\n" testName (if shellExecute mainApp ("-t " + testName + " --testpath " + testPath) then "SUCCEDED" else "FAILED")
    ) testFiles