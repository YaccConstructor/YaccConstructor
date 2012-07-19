namespace VSYardNS
(*
open System.Collections.Generic
open System
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities
open System.Linq
open Yard.Frontends.YardFrontend.Main
open Yard.Frontends.YardFrontend.GrammarParser
open Microsoft.VisualStudio.Language.Intellisense
open Yard.Core.Checkers
open System.Collections.Concurrent

module SolutionData = 
////
////                YardFile
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
         let tokens = reMakeTokens ("")  //Текущие токены (сначала пустые)
         let tree = ParseText("")

         let reMakeTokens (fileText: string) = fileText |> LexString |> List.ofSeq // Получаем токены    


         let getNonterminals (newTree: Yard.Core.IL.Definition.t<_,_> ) = 
                      newTree.grammar |> List.iter (fun elem ->
                      this.NotTermToDEFPosition.Add(fst elem.name)
                      )
         
         // Парсим string
         let parseText(fileText: string) =
            tokens = reMakeTokens (fileText)
            tree = ParseText (fileText)
            

         let reparse() = ParseFile (this.Info.FullPath + this.Info.FileName)
         member this.Info = yardInfo
      //   member this.PositionToNotTerm
         member this.NotTermToPosition = new Dictionary<string, List<CoordinateWord>>()
         member this.NotTermToDEFPosition = new Dictionary<string, CoordinateWord>()


         member ReParse() = reparse()
         
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
         let reparse() = this.Info.RootYard.ReParse()
         member this.Reparse() = reparse()


////
////               Solution
////

    type Solution () as this =
         let projects = new Dictionary<string, Project>()
         let FirstRunAddProjects (addProjects: Dictionary<_,_>) = for kvp in addProjects do projects.Add(kvp.Key,kvp.Value)
       //  let AddProject
         member this.Projects = projects
         member this.ReParseSolution() = for x in Projects do x.Reparse()

    let private x = Lazy<_>.Create(fun () -> new Solution())
    let GetData() = x

    *)