module Projects

open System.IO

type Mode = Debug|Release

type Platform = 
    | Any
    | X86
    | X64

type Project () = class
   let mutable name = ""
   let mutable folder = ""
   let mutable svnSrcFolder = ""
   let mutable mode = Release
   let mutable platform = X86
   let mutable netVer = "4.0"
   let mutable optimize = true

   let getBuildParams () =
    match mode with
     | Release -> "Release"
     | Debug   -> "Debug"
    |> fun x -> "Configuration",x
    |> fun x -> x::["TargetFrameworkVersion",netVer]
    |> fun x -> 
        match platform with
         | Any -> "AnyCPU"
         | X86 -> "x86"
         | X64 -> "x64"
        |> fun y -> "Platform",y
        |> fun y -> y::x
    |> fun x -> 
        ("Optimize", (if optimize then "true" else "false")) :: x

   let libFolder = "lib"
    
   member self.Name 
     with get () = name
     and set _name = name <- _name

   member self.Folder 
     with get () = folder
     and set _folder = folder <- _folder

   member self.SvnSrcFolder 
     with get () = svnSrcFolder
     and set _folder = svnSrcFolder <- _folder

   member self.Path
     with get () = Path.Combine(folder,name)

   member self.LibPath
     with get () = Path.Combine(folder, libFolder)

   member self.Mode
     with get () = mode
     and set _mode = mode <- _mode

   member self.Platform 
     with get () = platform
     and set _platform = platform <- _platform

   member self.NetVer 
     with get () = netVer
     and set _netVer = netVer <- _netVer

   member self.Optimize
     with get () = optimize
     and set _optimize = optimize <- _optimize

   member self.BuildProperties
     with get () =
        let bParams = getBuildParams()
        printfn @"\nProject %A build configuration: %A" name bParams
        bParams

end