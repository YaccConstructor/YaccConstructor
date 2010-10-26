//let tester = Yard.Generators.RecursiveAscent.RACCTester((*s :?> _*))
//let s = tester.RunTest

// Maybe we should use fsx?
// http://blogs.msdn.com/b/chrsmith/archive/2008/09/12/scripting-in-f.aspx
// Right-click it in Windows Explorer and choose "Run with F# Interactive.."

open System.IO
open System.Diagnostics
open System.Text.RegularExpressions

let mainPath = @"..\..\..\Main\bin\debug\Main.exe"
let testsPath = @"..\..\..\..\Tests\"
let tmpPath = @"C:\tmp"

let shellExecute program args =
    let startInfo = new ProcessStartInfo()
    startInfo.FileName <- program
    startInfo.Arguments <- args
    startInfo.UseShellExecute <- false
    startInfo.CreateNoWindow <- true
    //startInfo.WorkingDirectory <- mainPath
    startInfo.RedirectStandardError <- true

    let proc = Process.Start(startInfo)
    //proc.ErrorDataReceived
    proc.WaitForExit()
    ()

let testFiles = Seq.filter (fun name -> Regex.Match(name, "test(.*)\.yrd$").Success) (seq {yield! Directory.GetFiles(testsPath)})

let () = 
    Seq.iter (fun testName -> 
       shellExecute mainPath ("-t "+testName)
    ) testFiles
    printf "sdfsdf"
 