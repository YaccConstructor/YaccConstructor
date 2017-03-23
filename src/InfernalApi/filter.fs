module InfernalApi

open TblReader
open System.Diagnostics


//"cmscan.exe"
// sprintf "--anytrunc --noali --tblout log.txt archaea.cm \"%s\"" resultFileFullName

let runInfernal tool args =
    let workingDir = System.AppDomain.CurrentDomain.BaseDirectory + @"..\..\..\lib\infernal\"
        
    let runInfernal() = 
        let startInfo = new ProcessStartInfo()
        startInfo.FileName <- tool
        startInfo.Arguments <- args
        startInfo.WorkingDirectory <- workingDir
        Process.Start(startInfo).WaitForExit()
    
    runInfernal()

let score inputFile outFile =