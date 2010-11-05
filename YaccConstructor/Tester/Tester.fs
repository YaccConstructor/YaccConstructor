//let tester = Yard.Generators.RecursiveAscent.RACCTester((*s :?> _*))
//let s = tester.RunTest

// Maybe we should use fsx?
// http://blogs.msdn.com/b/chrsmith/archive/2008/09/12/scripting-in-f.aspx
// Right-click it in Windows Explorer and choose "Run with F# Interactive.."

open System.IO
open System.Diagnostics
open System.Text.RegularExpressions

let mainApp = @"..\..\..\Main\bin\debug\Main.exe"
let mainPath = @"..\..\..\Main\bin\debug"
let testsPath = @"..\..\..\..\Tests\"
let tmpPath = @"C:\tmp"

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

let testFiles = Seq.filter (fun name -> Regex.Match(name, "test(.*)\.yrd$").Success) (Seq.map Path.GetFileName (seq {yield! Directory.GetFiles(testsPath)}))

let () = 
    Seq.iter (fun testName -> 
       printf "%-40s %s\n" testName (if shellExecute mainApp ("-t "+testName) then "SUCCEDED" else "FAILED")
    ) testFiles
 