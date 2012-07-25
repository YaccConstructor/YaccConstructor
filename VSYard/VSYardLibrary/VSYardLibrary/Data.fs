namespace VSYardNS

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
open Yard.Core.IL.Production
open Yard.Core.IL.Definition
open System.Collections.Concurrent

module SolutionData = 
    type CoordinateWord (startCoordinate : int, wordLength : int) = 
         member this.StartCoordinate = startCoordinate
         member this.WordLength = wordLength
         member this.EndCoordinate = startCoordinate + wordLength
         
////
////                YardFile
////
    type YardInfo (id : string,
                   fileName : string,
                   fullPath : string) = 
         member this.Id = id
         member this.FileName = fileName
         member this.FullPath = fullPath

    type YardFile (yardInfo: YardInfo) as this =
         let info = yardInfo
         let reMakeTokens (fileText: string) = fileText |> LexString |> List.ofSeq // Получаем токены    
         let mutable tokens = reMakeTokens (String.Empty)  //Текущие токены (сначала пустые)
         let mutable tree = ParseText(String.Empty)
         let mutable positionToNotTerm = Array.create 0 ""
         let notTermToPosition = new Dictionary<string, List<CoordinateWord>>()
         let notTermToDEFPosition = new Dictionary<string, CoordinateWord>()
         //let listOfVisibleYardFile = []
         //let listOfVisibleYardFileHelper = [] : string list
         
         
         

         (*let findInlistOfVisibleYardFileHelper  elem = 
             let isCompair x y =  x = y
             match List.tryFind (isCompair elem) listOfVisibleYardFileHelper with
             | None -> listOfVisibleYardFileHelper = elem :: listOfVisibleYardFileHelper
             | Some _ -> 
         *)
         let addNotTermToDEFPosition node = 
                      let coorWord = CoordinateWord(match node with (a, (b,c,d)) -> b,c)
                      notTermToDEFPosition.Add( fst node, coorWord)
                      for i in coorWord.StartCoordinate .. coorWord.EndCoordinate do
                          Array.set positionToNotTerm i (fst node)

         let rec addNotTermToPosition elem =
                      match elem with
                      |  PRef ((name, (staCoordinate, endCoordinate, _)) , _) -> if (notTermToPosition.ContainsKey(name))
                                                                                 then notTermToPosition.[name].Add(CoordinateWord (staCoordinate, endCoordinate))
                                                                                 else let listCoor = new ResizeArray<CoordinateWord>()
                                                                                      listCoor.Add(CoordinateWord (staCoordinate, endCoordinate))
                                                                                      notTermToPosition.Add(name, listCoor)
                                                                                 for i in staCoordinate .. endCoordinate do
                                                                                      Array.set positionToNotTerm i name
                                                                             
                      |  PMetaRef ((name, (staCoordinate, endCoordinate, _)),_,exprList) ->  
                            if (notTermToPosition.ContainsKey(name))
                            then notTermToPosition.[name].Add(CoordinateWord (staCoordinate, endCoordinate))
                            else let listCoor = new ResizeArray<CoordinateWord>()
                                 listCoor.Add(CoordinateWord (staCoordinate, endCoordinate))
                                 notTermToPosition.Add(name, listCoor)
                            for i in staCoordinate .. endCoordinate do
                                 Array.set positionToNotTerm i name
                            exprList |> List.iter addNotTermToPosition
                      |  PSeq (exprList,_) -> exprList |> List.iter (fun r -> addNotTermToPosition r.rule)
                      |  PPerm exprList    -> exprList |> List.iter (fun r -> addNotTermToPosition r)
                      |  PRepet (expr,_,_)
                      |  PMany expr
                      |  PSome expr
                      |  POpt  expr -> addNotTermToPosition expr
                      |  PAlt (lExpr,rExpr) -> 
                           addNotTermToPosition lExpr
                           addNotTermToPosition rExpr
                      |  PLiteral _ -> ()
                      |  PToken _  -> ()

         let getNonterminals newTree = newTree.grammar |> List.iter (fun node ->
                                                                     if (match node.name with (_,(_,_,path)) -> String.Compare(path, info.FullPath) = 0)
                                                                     then addNotTermToDEFPosition (node.name)
                                                                          addNotTermToPosition (node.body)
                                                                   //  else (match node.name with (_,(_,_,path)) ->  findInlistOfVisibleYardFileHelper path)
                                                                    )
         
         // Парсим string
         let reParseText (fileText: string) =
             notTermToPosition.Clear()
             notTermToDEFPosition.Clear()
             positionToNotTerm <- Array.create fileText.Length ""
             tokens <- reMakeTokens (fileText)
             tree <- ParseText (fileText)
            
         
         let reparse() = ParseFile (info.FullPath + info.FileName)
         member this.Info = info
         member this.PositionToNotTerm = positionToNotTerm
         member this.NotTermToPosition = notTermToPosition
         member this.NotTermToDEFPosition = notTermToDEFPosition


         member this.ReParseText(fileText) = reParseText(fileText)


////
////               Project
////

    type ProjectInfo (id : string,
                      fileName : string,
                      fullPath : string,
                      rootYard : string,
                      dicYard: Dictionary<string, YardFile>) =
         member this.extenderCATID = id
         member this.FileName = fileName
         member this.FullPath = fullPath
         member this.RootYard = rootYard
         member this.DicYard = dicYard

    type Project (projectInfo : ProjectInfo) as this =
         let info = projectInfo
     //    let reparse() = info.RootYard.ReParse()
         member this.Info = info
         member this.ReParseFile(externalCATIDYardFile, text) = 
            let r = info.DicYard.[externalCATIDYardFile]
            r.ReParseText(text)
            r

         member this.GetParseFile(externalCATIDYardFile) = info.DicYard.[externalCATIDYardFile]
     //    member this.ReParse() = reparse()


////
////               Solution
////

    type Solution () as this =
         let projects = new Dictionary<string, Project>()
         let firstRunAddProjects (addProjects: Dictionary<_,_>) = for kvp in addProjects do projects.Add(kvp.Key,kvp.Value)
       //  let AddProject
         member this.Projects = projects
         member this.FirstRunAddProjects(y) = firstRunAddProjects(y)
         member this.ReParseFile (externalCATIDProject, externalCATIDYardFile, text) = projects.[externalCATIDProject].ReParseFile(externalCATIDYardFile, text)
         member this.GetParseFile (externalCATIDProject, externalCATIDYardFile) = projects.[externalCATIDProject].GetParseFile(externalCATIDYardFile)
    //     member this.ReParseSolution() = for x in projects do x.Value.ReParse()


    let private x = new Solution()//Lazy<_>.Create(fun () -> new Solution())
    let GetSolution() = x

    