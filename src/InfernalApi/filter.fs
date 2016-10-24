module InfernalApi

open TblReader
open System.Diagnostics

let filterWithInfernal resultFileFullName =
    let workingDir = System.AppDomain.CurrentDomain.BaseDirectory + @"..\..\..\infernal\"
        
    let runInfernal() = 
        let startInfo = new ProcessStartInfo()
        startInfo.FileName <- "cmscan.exe"
        startInfo.Arguments <- sprintf "--anytrunc --noali --tblout log.txt archaea.cm \"%s\"" resultFileFullName
        startInfo.WorkingDirectory <- workingDir
        Process.Start(startInfo).WaitForExit()
    //runInfernal()

    getTbl (workingDir + "log.txt")