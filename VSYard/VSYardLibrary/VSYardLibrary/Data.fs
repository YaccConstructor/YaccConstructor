namespace VSYardNS

open System.Collections.Generic
open System
open System.ComponentModel.Composition

module SolutionData = 
    let private x = Lazy.Create(fun () -> new Solution())
    let GetData() = x


////
////               YardFile
////
    type YardInfo (id : string,
                   fileName : string,
                   fullPath : string,
                   included: bool) = 
         member this.Id = id
         member this.FileName = fileName
         member this.FullPath = fullPath
         member this.Included = included

    type CoordinateWord (startCoordinate : int, wordLength : int) = 
         member this.StartCoordinate = startCoordinate
         member this.WordLength = wordLength
         member this.EndCoordinate = startCoordinate + wordLength

    type YardFile (yardInfo: YardInfo) as this = 
         member this.Info = yardInfo
      //   member this.PositionToNotTerm
         member this.NotTermToPosition = new Dictionary<string, List<CoordinateWord>>()
         member this.NotTermToDEFPosition = new Dictionary<string, CoordinateWord>()
        // let reparse() = ;
        // member ReParse() = reparse()
         
      //   member this.Tokens = new Array<>
      //   let Reparse() = 

////
////               Project
////

    type ProjectInfo (id : string,
                      fileName : string,
                      fullPath : string,
                      rootYard : YardFile,
                      dicYard: Dictionary<string, YardFile>) =
         member this.extenderCATID = id
         member this.FileName = fileName
         member this.FullPath = fullPath
         member this.RootYard = rootYard
         member this.DicYard = dicYard

    type Project (projectInfo : ProjectInfo) as this =
         member this.Info = projectInfo
       //  let reparse() = this.Info.RootYard.ReParse()
       //  member this.Reparse() = reparse()


////
////               Solution
////

    type Solution () as this =
         let projects = new Dictionary<string, Project>()
         let FirstRunAddProjects (projects: Dictionary<string, Project>) = pr
         member this.Projects = projects
       //  member this.ReParseSolution() = for x in Projects do x.Reparse()

