module Yard.Generators.GLL.AbstractParserCF

open System 
open Microsoft.FSharp.Collections
open System.Collections.Generic
open FSharpx.Collections.Experimental

open Yard.Generators.GLL
open Yard.Generators.Common.DataStructures
open AbstractAnalysis.Common
open Yard.Generators.GLL.ParserCommon
open Yard.Generators.GLL.ParserCommon.CommonFuns

open YC.GLL.GSS

let toInputPos = int >> (*) 1<positionInInput>      //  problem with measures (TODO)

let parse (parser : ParserSourceGLL) (input : ParserSourceGLL) = 

    let gssParser, gssInput = new GSS(), new GSS()

    let startContext =
        let (s1, s2) = (parser.StartState, input.StartState)
        let vertexParser, vertexInput = new GSSVertex(s1, toInputPos s2), new GSSVertex(s2, toInputPos s1)
        gssParser.AddVertex vertexParser |> ignore
        gssInput.AddVertex vertexInput |> ignore
        new ContextCF<_>(s1, s2, vertexParser, vertexInput)

    (gssParser, gssInput)