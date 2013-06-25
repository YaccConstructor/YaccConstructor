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
open Yard.Core.IL
open System.Collections.Concurrent

module SolutionData = 
    type CoordinateWord (startCoordinate : int, endCoordinate : int) = 
         member this.StartCoordinate = startCoordinate
         member this.WordLength = startCoordinate - endCoordinate
         member this.EndCoordinate = endCoordinate
         
////
////                YardFile
////
    type YardInfo (id : string,
                   fileName : string,
                   fullPath : string) = 
         member this.Id = id
         member this.FileName = fileName
         member this.FullPath = fullPath

    type YardFile (yardInfo: YardInfo) =
         let info = yardInfo
         let reMakeTokens (fileText: string) = fileText |> LexString |> List.ofSeq // Получаем токены    
         let mutable tokens = reMakeTokens (String.Empty)  //Текущие токены (сначала пустые)
         let mutable tree = Unchecked.defaultof<_>
         let mutable positionToNotTerm = Array.create 0 ""
         let notTermToPosition = new Dictionary<string, List<CoordinateWord>>()
         let notTermToDEFPosition = new Dictionary<string, CoordinateWord>()
         
         (* Пока что ненужно, но пригодится.
         let listOfVisibleYardFile = []
         //let listOfVisibleYardFileHelper = [] : string list
           let findInlistOfVisibleYardFileHelper  elem = 
             let isCompair x y =  x = y
             match List.tryFind (isCompair elem) listOfVisibleYardFileHelper with
             | None -> listOfVisibleYardFileHelper = elem :: listOfVisibleYardFileHelper
             | Some _ -> 
         *)
         let addNotTermToDEFPosition (node : Source.t) = 
                      let coorWord = CoordinateWord(node.startPos.absoluteOffset, node.endPos.absoluteOffset)
                      notTermToDEFPosition.Add (node.text, coorWord)
                      for i in coorWord.StartCoordinate .. coorWord.EndCoordinate do
                          Array.set positionToNotTerm i node.text

         let rec addNotTermToPosition elem =
                match elem with
                |  PRef (name, _) ->
                    let name, s, e = name.text, name.startPos.absoluteOffset, name.endPos.absoluteOffset
                    if (notTermToPosition.ContainsKey(name))
                    then notTermToPosition.[name].Add(CoordinateWord (s, e))
                    else
                        let listCoor = new ResizeArray<CoordinateWord>()
                        listCoor.Add(CoordinateWord (s, e))
                        notTermToPosition.Add(name, listCoor)
                    for i in s .. e do
                        Array.set positionToNotTerm i name
                                                                             
                |  PMetaRef (name,_,exprList) ->  
                    let name, s, e = name.text, name.startPos.absoluteOffset, name.endPos.absoluteOffset
                    if (notTermToPosition.ContainsKey(name))
                    then notTermToPosition.[name].Add(CoordinateWord (s, e))
                    else
                        let listCoor = new ResizeArray<CoordinateWord>()
                        listCoor.Add(CoordinateWord (s, e))
                        notTermToPosition.Add(name, listCoor)
                    for i in s .. e do
                        Array.set positionToNotTerm i name
                    exprList |> List.iter addNotTermToPosition
                |  PSeq (exprList,_,l) -> exprList |> List.iter (fun r -> addNotTermToPosition r.rule)
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

         let getNonterminals newTree =
            newTree.grammar |> List.iter (fun mod' ->
                mod'.rules |> List.iter (fun node ->
                    if node.name.file = info.FullPath then
                        addNotTermToDEFPosition (node.name)
                        addNotTermToPosition (node.body)
                )
            //  else (match node.name with (_,(_,_,path)) ->  findInlistOfVisibleYardFileHelper path)
            )
         
         // Парсим string
         let reParseText (fileText: string) =
             notTermToPosition.Clear()
             notTermToDEFPosition.Clear()
             positionToNotTerm <- Array.create fileText.Length ""
             tokens <- reMakeTokens (fileText)
             tree <- ParseText fileText info.FullPath
             getNonterminals (tree)
            
         
         let reparse() = ParseFile (info.FullPath + info.FileName)
         member this.Info = info
         member this.PositionToNotTerm = positionToNotTerm
         member this.NotTermToPosition = notTermToPosition
         member this.NotTermToDEFPosition = notTermToDEFPosition
         member this.Tokens = tokens


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

    type Project (projectInfo : ProjectInfo) =
         let info = projectInfo
     //    let reparse() = info.RootYard.ReParse()
         member this.Info = info
         member this.ReParseFile(yardFileName, text) = 
            let r = info.DicYard.[yardFileName]
            r.ReParseText(text)
            r

         member this.GetParseFile(yardFileName) = info.DicYard.[yardFileName]
     //    member this.ReParse() = reparse()


////
////               Solution
////

    type Solution () =
         let projects = new Dictionary<string, Project>()
         let firstRunAddProjects (addProjects: Dictionary<_,_>) = for kvp in addProjects do projects.Add(kvp.Key,kvp.Value)
       //  let AddProject
         member this.Projects = projects
         member this.FirstRunAddProjects(y) = firstRunAddProjects(y)
         member this.ReParseFile (projectFileName, yardFileName, text) = projects.[projectFileName].ReParseFile(yardFileName, text)
         member this.GetParseFile (projectFileName, yardFileName) = projects.[projectFileName].GetParseFile(yardFileName)
         member this.GetParseProject (projectFileName) = projects.[projectFileName]
    //     member this.ReParseSolution() = for x in projects do x.Value.ReParse()


    let private x = new Solution()//Lazy<_>.Create(fun () -> new Solution())
    let GetSolution() = x

    