 
module YC.FST.AbstractLexing.CalcLexer

open Microsoft.FSharp.Collections
open QuickGraph.FST.GraphBasedFst
open QuickGraph.FSA.GraphBasedFsa
open YC.FST.AbstractLexing.Interpreter
open AbstractAnalysis.Common
open AbstractParser.Tokens
open System.Collections.Generic

let fstLexer () = 
   let startState = ResizeArray.singleton 0
   let finishState = ResizeArray.singleton 65535
   let transitions = new ResizeArray<_>()
   transitions.Add(0, (Smbl 65535, Eps), 65535)
   transitions.Add(0, (Smbl 9, Eps), 1)
   transitions.Add(0, (Smbl 10, Eps), 1)
   transitions.Add(0, (Smbl 13, Eps), 1)
   transitions.Add(0, (Smbl 32, Eps), 1)
   transitions.Add(0, (Smbl 40, Eps), 4)
   transitions.Add(0, (Smbl 41, Eps), 5)
   transitions.Add(0, (Smbl 42, Eps), 8)
   transitions.Add(0, (Smbl 43, Eps), 7)
   transitions.Add(0, (Smbl 45, Eps), 3)
   transitions.Add(0, (Smbl 47, Eps), 6)
   transitions.Add(0, (Smbl 48, Eps), 2)
   transitions.Add(0, (Smbl 49, Eps), 2)
   transitions.Add(0, (Smbl 50, Eps), 2)
   transitions.Add(0, (Smbl 51, Eps), 2)
   transitions.Add(0, (Smbl 52, Eps), 2)
   transitions.Add(0, (Smbl 53, Eps), 2)
   transitions.Add(0, (Smbl 54, Eps), 2)
   transitions.Add(0, (Smbl 55, Eps), 2)
   transitions.Add(0, (Smbl 56, Eps), 2)
   transitions.Add(0, (Smbl 57, Eps), 2)
   transitions.Add(1, (Smbl 9, Smbl 0), 1)
   transitions.Add(1, (Smbl 10, Smbl 0), 1)
   transitions.Add(1, (Smbl 13, Smbl 0), 1)
   transitions.Add(1, (Smbl 32, Smbl 0), 1)
   transitions.Add(1, (Smbl 40, Smbl 0), 4)
   transitions.Add(1, (Smbl 41, Smbl 0), 5)
   transitions.Add(1, (Smbl 42, Smbl 0), 8)
   transitions.Add(1, (Smbl 43, Smbl 0), 7)
   transitions.Add(1, (Smbl 45, Smbl 0), 3)
   transitions.Add(1, (Smbl 47, Smbl 0), 6)
   transitions.Add(1, (Smbl 48, Smbl 0), 2)
   transitions.Add(1, (Smbl 49, Smbl 0), 2)
   transitions.Add(1, (Smbl 50, Smbl 0), 2)
   transitions.Add(1, (Smbl 51, Smbl 0), 2)
   transitions.Add(1, (Smbl 52, Smbl 0), 2)
   transitions.Add(1, (Smbl 53, Smbl 0), 2)
   transitions.Add(1, (Smbl 54, Smbl 0), 2)
   transitions.Add(1, (Smbl 55, Smbl 0), 2)
   transitions.Add(1, (Smbl 56, Smbl 0), 2)
   transitions.Add(1, (Smbl 57, Smbl 0), 2)
   transitions.Add(1, (Smbl 65535, Smbl 0), 65535)
   transitions.Add(4, (Smbl 9, Smbl 3), 1)
   transitions.Add(4, (Smbl 10, Smbl 3), 1)
   transitions.Add(4, (Smbl 13, Smbl 3), 1)
   transitions.Add(4, (Smbl 32, Smbl 3), 1)
   transitions.Add(4, (Smbl 40, Smbl 3), 4)
   transitions.Add(4, (Smbl 41, Smbl 3), 5)
   transitions.Add(4, (Smbl 42, Smbl 3), 8)
   transitions.Add(4, (Smbl 43, Smbl 3), 7)
   transitions.Add(4, (Smbl 45, Smbl 3), 3)
   transitions.Add(4, (Smbl 47, Smbl 3), 6)
   transitions.Add(4, (Smbl 48, Smbl 3), 2)
   transitions.Add(4, (Smbl 49, Smbl 3), 2)
   transitions.Add(4, (Smbl 50, Smbl 3), 2)
   transitions.Add(4, (Smbl 51, Smbl 3), 2)
   transitions.Add(4, (Smbl 52, Smbl 3), 2)
   transitions.Add(4, (Smbl 53, Smbl 3), 2)
   transitions.Add(4, (Smbl 54, Smbl 3), 2)
   transitions.Add(4, (Smbl 55, Smbl 3), 2)
   transitions.Add(4, (Smbl 56, Smbl 3), 2)
   transitions.Add(4, (Smbl 57, Smbl 3), 2)
   transitions.Add(4, (Smbl 65535, Smbl 3), 65535)
   transitions.Add(5, (Smbl 9, Smbl 4), 1)
   transitions.Add(5, (Smbl 10, Smbl 4), 1)
   transitions.Add(5, (Smbl 13, Smbl 4), 1)
   transitions.Add(5, (Smbl 32, Smbl 4), 1)
   transitions.Add(5, (Smbl 40, Smbl 4), 4)
   transitions.Add(5, (Smbl 41, Smbl 4), 5)
   transitions.Add(5, (Smbl 42, Smbl 4), 8)
   transitions.Add(5, (Smbl 43, Smbl 4), 7)
   transitions.Add(5, (Smbl 45, Smbl 4), 3)
   transitions.Add(5, (Smbl 47, Smbl 4), 6)
   transitions.Add(5, (Smbl 48, Smbl 4), 2)
   transitions.Add(5, (Smbl 49, Smbl 4), 2)
   transitions.Add(5, (Smbl 50, Smbl 4), 2)
   transitions.Add(5, (Smbl 51, Smbl 4), 2)
   transitions.Add(5, (Smbl 52, Smbl 4), 2)
   transitions.Add(5, (Smbl 53, Smbl 4), 2)
   transitions.Add(5, (Smbl 54, Smbl 4), 2)
   transitions.Add(5, (Smbl 55, Smbl 4), 2)
   transitions.Add(5, (Smbl 56, Smbl 4), 2)
   transitions.Add(5, (Smbl 57, Smbl 4), 2)
   transitions.Add(5, (Smbl 65535, Smbl 4), 65535)
   transitions.Add(8, (Smbl 9, Smbl 8), 1)
   transitions.Add(8, (Smbl 10, Smbl 8), 1)
   transitions.Add(8, (Smbl 13, Smbl 8), 1)
   transitions.Add(8, (Smbl 32, Smbl 8), 1)
   transitions.Add(8, (Smbl 40, Smbl 8), 4)
   transitions.Add(8, (Smbl 41, Smbl 8), 5)
   transitions.Add(8, (Smbl 42, Eps), 9)
   transitions.Add(8, (Smbl 43, Smbl 8), 7)
   transitions.Add(8, (Smbl 45, Smbl 8), 3)
   transitions.Add(8, (Smbl 47, Smbl 8), 6)
   transitions.Add(8, (Smbl 48, Smbl 8), 2)
   transitions.Add(8, (Smbl 49, Smbl 8), 2)
   transitions.Add(8, (Smbl 50, Smbl 8), 2)
   transitions.Add(8, (Smbl 51, Smbl 8), 2)
   transitions.Add(8, (Smbl 52, Smbl 8), 2)
   transitions.Add(8, (Smbl 53, Smbl 8), 2)
   transitions.Add(8, (Smbl 54, Smbl 8), 2)
   transitions.Add(8, (Smbl 55, Smbl 8), 2)
   transitions.Add(8, (Smbl 56, Smbl 8), 2)
   transitions.Add(8, (Smbl 57, Smbl 8), 2)
   transitions.Add(8, (Smbl 65535, Smbl 8), 65535)
   transitions.Add(7, (Smbl 9, Smbl 6), 1)
   transitions.Add(7, (Smbl 10, Smbl 6), 1)
   transitions.Add(7, (Smbl 13, Smbl 6), 1)
   transitions.Add(7, (Smbl 32, Smbl 6), 1)
   transitions.Add(7, (Smbl 40, Smbl 6), 4)
   transitions.Add(7, (Smbl 41, Smbl 6), 5)
   transitions.Add(7, (Smbl 42, Smbl 6), 8)
   transitions.Add(7, (Smbl 43, Smbl 6), 7)
   transitions.Add(7, (Smbl 45, Smbl 6), 3)
   transitions.Add(7, (Smbl 47, Smbl 6), 6)
   transitions.Add(7, (Smbl 48, Smbl 6), 2)
   transitions.Add(7, (Smbl 49, Smbl 6), 2)
   transitions.Add(7, (Smbl 50, Smbl 6), 2)
   transitions.Add(7, (Smbl 51, Smbl 6), 2)
   transitions.Add(7, (Smbl 52, Smbl 6), 2)
   transitions.Add(7, (Smbl 53, Smbl 6), 2)
   transitions.Add(7, (Smbl 54, Smbl 6), 2)
   transitions.Add(7, (Smbl 55, Smbl 6), 2)
   transitions.Add(7, (Smbl 56, Smbl 6), 2)
   transitions.Add(7, (Smbl 57, Smbl 6), 2)
   transitions.Add(7, (Smbl 65535, Smbl 6), 65535)
   transitions.Add(3, (Smbl 9, Smbl 2), 1)
   transitions.Add(3, (Smbl 10, Smbl 2), 1)
   transitions.Add(3, (Smbl 13, Smbl 2), 1)
   transitions.Add(3, (Smbl 32, Smbl 2), 1)
   transitions.Add(3, (Smbl 40, Smbl 2), 4)
   transitions.Add(3, (Smbl 41, Smbl 2), 5)
   transitions.Add(3, (Smbl 42, Smbl 2), 8)
   transitions.Add(3, (Smbl 43, Smbl 2), 7)
   transitions.Add(3, (Smbl 45, Smbl 2), 3)
   transitions.Add(3, (Smbl 47, Smbl 2), 6)
   transitions.Add(3, (Smbl 48, Smbl 2), 2)
   transitions.Add(3, (Smbl 49, Smbl 2), 2)
   transitions.Add(3, (Smbl 50, Smbl 2), 2)
   transitions.Add(3, (Smbl 51, Smbl 2), 2)
   transitions.Add(3, (Smbl 52, Smbl 2), 2)
   transitions.Add(3, (Smbl 53, Smbl 2), 2)
   transitions.Add(3, (Smbl 54, Smbl 2), 2)
   transitions.Add(3, (Smbl 55, Smbl 2), 2)
   transitions.Add(3, (Smbl 56, Smbl 2), 2)
   transitions.Add(3, (Smbl 57, Smbl 2), 2)
   transitions.Add(3, (Smbl 65535, Smbl 2), 65535)
   transitions.Add(6, (Smbl 9, Smbl 5), 1)
   transitions.Add(6, (Smbl 10, Smbl 5), 1)
   transitions.Add(6, (Smbl 13, Smbl 5), 1)
   transitions.Add(6, (Smbl 32, Smbl 5), 1)
   transitions.Add(6, (Smbl 40, Smbl 5), 4)
   transitions.Add(6, (Smbl 41, Smbl 5), 5)
   transitions.Add(6, (Smbl 42, Smbl 5), 8)
   transitions.Add(6, (Smbl 43, Smbl 5), 7)
   transitions.Add(6, (Smbl 45, Smbl 5), 3)
   transitions.Add(6, (Smbl 47, Smbl 5), 6)
   transitions.Add(6, (Smbl 48, Smbl 5), 2)
   transitions.Add(6, (Smbl 49, Smbl 5), 2)
   transitions.Add(6, (Smbl 50, Smbl 5), 2)
   transitions.Add(6, (Smbl 51, Smbl 5), 2)
   transitions.Add(6, (Smbl 52, Smbl 5), 2)
   transitions.Add(6, (Smbl 53, Smbl 5), 2)
   transitions.Add(6, (Smbl 54, Smbl 5), 2)
   transitions.Add(6, (Smbl 55, Smbl 5), 2)
   transitions.Add(6, (Smbl 56, Smbl 5), 2)
   transitions.Add(6, (Smbl 57, Smbl 5), 2)
   transitions.Add(6, (Smbl 65535, Smbl 5), 65535)
   transitions.Add(2, (Smbl 9, Smbl 1), 1)
   transitions.Add(2, (Smbl 10, Smbl 1), 1)
   transitions.Add(2, (Smbl 13, Smbl 1), 1)
   transitions.Add(2, (Smbl 32, Smbl 1), 1)
   transitions.Add(2, (Smbl 40, Smbl 1), 4)
   transitions.Add(2, (Smbl 41, Smbl 1), 5)
   transitions.Add(2, (Smbl 42, Smbl 1), 8)
   transitions.Add(2, (Smbl 43, Smbl 1), 7)
   transitions.Add(2, (Smbl 45, Smbl 1), 3)
   transitions.Add(2, (Smbl 46, Eps), 11)
   transitions.Add(2, (Smbl 47, Smbl 1), 6)
   transitions.Add(2, (Smbl 48, Eps), 12)
   transitions.Add(2, (Smbl 49, Eps), 12)
   transitions.Add(2, (Smbl 50, Eps), 12)
   transitions.Add(2, (Smbl 51, Eps), 12)
   transitions.Add(2, (Smbl 52, Eps), 12)
   transitions.Add(2, (Smbl 53, Eps), 12)
   transitions.Add(2, (Smbl 54, Eps), 12)
   transitions.Add(2, (Smbl 55, Eps), 12)
   transitions.Add(2, (Smbl 56, Eps), 12)
   transitions.Add(2, (Smbl 57, Eps), 12)
   transitions.Add(2, (Smbl 69, Eps), 10)
   transitions.Add(2, (Smbl 101, Eps), 10)
   transitions.Add(2, (Smbl 65535, Smbl 1), 65535)
   transitions.Add(11, (Smbl 9, Eps), 1)
   transitions.Add(11, (Smbl 10, Eps), 1)
   transitions.Add(11, (Smbl 13, Eps), 1)
   transitions.Add(11, (Smbl 32, Eps), 1)
   transitions.Add(11, (Smbl 40, Eps), 4)
   transitions.Add(11, (Smbl 41, Eps), 5)
   transitions.Add(11, (Smbl 42, Eps), 8)
   transitions.Add(11, (Smbl 43, Eps), 7)
   transitions.Add(11, (Smbl 45, Eps), 3)
   transitions.Add(11, (Smbl 47, Eps), 6)
   transitions.Add(11, (Smbl 48, Eps), 13)
   transitions.Add(11, (Smbl 49, Eps), 13)
   transitions.Add(11, (Smbl 50, Eps), 13)
   transitions.Add(11, (Smbl 51, Eps), 13)
   transitions.Add(11, (Smbl 52, Eps), 13)
   transitions.Add(11, (Smbl 53, Eps), 13)
   transitions.Add(11, (Smbl 54, Eps), 13)
   transitions.Add(11, (Smbl 55, Eps), 13)
   transitions.Add(11, (Smbl 56, Eps), 13)
   transitions.Add(11, (Smbl 57, Eps), 13)
   transitions.Add(11, (Smbl 65535, Eps), 65535)
   transitions.Add(12, (Smbl 9, Smbl 1), 1)
   transitions.Add(12, (Smbl 10, Smbl 1), 1)
   transitions.Add(12, (Smbl 13, Smbl 1), 1)
   transitions.Add(12, (Smbl 32, Smbl 1), 1)
   transitions.Add(12, (Smbl 40, Smbl 1), 4)
   transitions.Add(12, (Smbl 41, Smbl 1), 5)
   transitions.Add(12, (Smbl 42, Smbl 1), 8)
   transitions.Add(12, (Smbl 43, Smbl 1), 7)
   transitions.Add(12, (Smbl 45, Smbl 1), 3)
   transitions.Add(12, (Smbl 46, Eps), 11)
   transitions.Add(12, (Smbl 47, Smbl 1), 6)
   transitions.Add(12, (Smbl 48, Eps), 12)
   transitions.Add(12, (Smbl 49, Eps), 12)
   transitions.Add(12, (Smbl 50, Eps), 12)
   transitions.Add(12, (Smbl 51, Eps), 12)
   transitions.Add(12, (Smbl 52, Eps), 12)
   transitions.Add(12, (Smbl 53, Eps), 12)
   transitions.Add(12, (Smbl 54, Eps), 12)
   transitions.Add(12, (Smbl 55, Eps), 12)
   transitions.Add(12, (Smbl 56, Eps), 12)
   transitions.Add(12, (Smbl 57, Eps), 12)
   transitions.Add(12, (Smbl 69, Eps), 10)
   transitions.Add(12, (Smbl 101, Eps), 10)
   transitions.Add(12, (Smbl 65535, Smbl 1), 65535)
   transitions.Add(10, (Smbl 9, Eps), 1)
   transitions.Add(10, (Smbl 10, Eps), 1)
   transitions.Add(10, (Smbl 13, Eps), 1)
   transitions.Add(10, (Smbl 32, Eps), 1)
   transitions.Add(10, (Smbl 40, Eps), 4)
   transitions.Add(10, (Smbl 41, Eps), 5)
   transitions.Add(10, (Smbl 42, Eps), 8)
   transitions.Add(10, (Smbl 43, Eps), 7)
   transitions.Add(10, (Smbl 45, Eps), 3)
   transitions.Add(10, (Smbl 47, Eps), 6)
   transitions.Add(10, (Smbl 48, Eps), 15)
   transitions.Add(10, (Smbl 49, Eps), 15)
   transitions.Add(10, (Smbl 50, Eps), 15)
   transitions.Add(10, (Smbl 51, Eps), 15)
   transitions.Add(10, (Smbl 52, Eps), 15)
   transitions.Add(10, (Smbl 53, Eps), 15)
   transitions.Add(10, (Smbl 54, Eps), 15)
   transitions.Add(10, (Smbl 55, Eps), 15)
   transitions.Add(10, (Smbl 56, Eps), 15)
   transitions.Add(10, (Smbl 57, Eps), 15)
   transitions.Add(10, (Smbl 65535, Eps), 65535)
   transitions.Add(9, (Smbl 9, Smbl 7), 1)
   transitions.Add(9, (Smbl 10, Smbl 7), 1)
   transitions.Add(9, (Smbl 13, Smbl 7), 1)
   transitions.Add(9, (Smbl 32, Smbl 7), 1)
   transitions.Add(9, (Smbl 40, Smbl 7), 4)
   transitions.Add(9, (Smbl 41, Smbl 7), 5)
   transitions.Add(9, (Smbl 42, Smbl 7), 8)
   transitions.Add(9, (Smbl 43, Smbl 7), 7)
   transitions.Add(9, (Smbl 45, Smbl 7), 3)
   transitions.Add(9, (Smbl 47, Smbl 7), 6)
   transitions.Add(9, (Smbl 48, Smbl 7), 2)
   transitions.Add(9, (Smbl 49, Smbl 7), 2)
   transitions.Add(9, (Smbl 50, Smbl 7), 2)
   transitions.Add(9, (Smbl 51, Smbl 7), 2)
   transitions.Add(9, (Smbl 52, Smbl 7), 2)
   transitions.Add(9, (Smbl 53, Smbl 7), 2)
   transitions.Add(9, (Smbl 54, Smbl 7), 2)
   transitions.Add(9, (Smbl 55, Smbl 7), 2)
   transitions.Add(9, (Smbl 56, Smbl 7), 2)
   transitions.Add(9, (Smbl 57, Smbl 7), 2)
   transitions.Add(9, (Smbl 65535, Smbl 7), 65535)
   transitions.Add(15, (Smbl 9, Smbl 1), 1)
   transitions.Add(15, (Smbl 10, Smbl 1), 1)
   transitions.Add(15, (Smbl 13, Smbl 1), 1)
   transitions.Add(15, (Smbl 32, Smbl 1), 1)
   transitions.Add(15, (Smbl 40, Smbl 1), 4)
   transitions.Add(15, (Smbl 41, Smbl 1), 5)
   transitions.Add(15, (Smbl 42, Smbl 1), 8)
   transitions.Add(15, (Smbl 43, Smbl 1), 7)
   transitions.Add(15, (Smbl 45, Smbl 1), 3)
   transitions.Add(15, (Smbl 47, Smbl 1), 6)
   transitions.Add(15, (Smbl 48, Eps), 16)
   transitions.Add(15, (Smbl 49, Eps), 16)
   transitions.Add(15, (Smbl 50, Eps), 16)
   transitions.Add(15, (Smbl 51, Eps), 16)
   transitions.Add(15, (Smbl 52, Eps), 16)
   transitions.Add(15, (Smbl 53, Eps), 16)
   transitions.Add(15, (Smbl 54, Eps), 16)
   transitions.Add(15, (Smbl 55, Eps), 16)
   transitions.Add(15, (Smbl 56, Eps), 16)
   transitions.Add(15, (Smbl 57, Eps), 16)
   transitions.Add(15, (Smbl 65535, Smbl 1), 65535)
   transitions.Add(13, (Smbl 9, Smbl 1), 1)
   transitions.Add(13, (Smbl 10, Smbl 1), 1)
   transitions.Add(13, (Smbl 13, Smbl 1), 1)
   transitions.Add(13, (Smbl 32, Smbl 1), 1)
   transitions.Add(13, (Smbl 40, Smbl 1), 4)
   transitions.Add(13, (Smbl 41, Smbl 1), 5)
   transitions.Add(13, (Smbl 42, Smbl 1), 8)
   transitions.Add(13, (Smbl 43, Smbl 1), 7)
   transitions.Add(13, (Smbl 45, Smbl 1), 3)
   transitions.Add(13, (Smbl 47, Smbl 1), 6)
   transitions.Add(13, (Smbl 48, Eps), 14)
   transitions.Add(13, (Smbl 49, Eps), 14)
   transitions.Add(13, (Smbl 50, Eps), 14)
   transitions.Add(13, (Smbl 51, Eps), 14)
   transitions.Add(13, (Smbl 52, Eps), 14)
   transitions.Add(13, (Smbl 53, Eps), 14)
   transitions.Add(13, (Smbl 54, Eps), 14)
   transitions.Add(13, (Smbl 55, Eps), 14)
   transitions.Add(13, (Smbl 56, Eps), 14)
   transitions.Add(13, (Smbl 57, Eps), 14)
   transitions.Add(13, (Smbl 69, Eps), 10)
   transitions.Add(13, (Smbl 101, Eps), 10)
   transitions.Add(13, (Smbl 65535, Smbl 1), 65535)
   transitions.Add(14, (Smbl 9, Smbl 1), 1)
   transitions.Add(14, (Smbl 10, Smbl 1), 1)
   transitions.Add(14, (Smbl 13, Smbl 1), 1)
   transitions.Add(14, (Smbl 32, Smbl 1), 1)
   transitions.Add(14, (Smbl 40, Smbl 1), 4)
   transitions.Add(14, (Smbl 41, Smbl 1), 5)
   transitions.Add(14, (Smbl 42, Smbl 1), 8)
   transitions.Add(14, (Smbl 43, Smbl 1), 7)
   transitions.Add(14, (Smbl 45, Smbl 1), 3)
   transitions.Add(14, (Smbl 47, Smbl 1), 6)
   transitions.Add(14, (Smbl 48, Eps), 14)
   transitions.Add(14, (Smbl 49, Eps), 14)
   transitions.Add(14, (Smbl 50, Eps), 14)
   transitions.Add(14, (Smbl 51, Eps), 14)
   transitions.Add(14, (Smbl 52, Eps), 14)
   transitions.Add(14, (Smbl 53, Eps), 14)
   transitions.Add(14, (Smbl 54, Eps), 14)
   transitions.Add(14, (Smbl 55, Eps), 14)
   transitions.Add(14, (Smbl 56, Eps), 14)
   transitions.Add(14, (Smbl 57, Eps), 14)
   transitions.Add(14, (Smbl 69, Eps), 10)
   transitions.Add(14, (Smbl 101, Eps), 10)
   transitions.Add(14, (Smbl 65535, Smbl 1), 65535)
   transitions.Add(16, (Smbl 9, Smbl 1), 1)
   transitions.Add(16, (Smbl 10, Smbl 1), 1)
   transitions.Add(16, (Smbl 13, Smbl 1), 1)
   transitions.Add(16, (Smbl 32, Smbl 1), 1)
   transitions.Add(16, (Smbl 40, Smbl 1), 4)
   transitions.Add(16, (Smbl 41, Smbl 1), 5)
   transitions.Add(16, (Smbl 42, Smbl 1), 8)
   transitions.Add(16, (Smbl 43, Smbl 1), 7)
   transitions.Add(16, (Smbl 45, Smbl 1), 3)
   transitions.Add(16, (Smbl 47, Smbl 1), 6)
   transitions.Add(16, (Smbl 48, Eps), 16)
   transitions.Add(16, (Smbl 49, Eps), 16)
   transitions.Add(16, (Smbl 50, Eps), 16)
   transitions.Add(16, (Smbl 51, Eps), 16)
   transitions.Add(16, (Smbl 52, Eps), 16)
   transitions.Add(16, (Smbl 53, Eps), 16)
   transitions.Add(16, (Smbl 54, Eps), 16)
   transitions.Add(16, (Smbl 55, Eps), 16)
   transitions.Add(16, (Smbl 56, Eps), 16)
   transitions.Add(16, (Smbl 57, Eps), 16)
   transitions.Add(16, (Smbl 65535, Smbl 1), 65535)
   new FST<_,_>(startState, finishState, transitions)

let actions () =
   [|

      (fun (gr : FSA<_>) ->
                              None );
      (fun (gr : FSA<_>) ->
                                                           NUMBER(gr) |> Some );
      (fun (gr : FSA<_>) ->
                       MINUS(gr) |> Some );
      (fun (gr : FSA<_>) ->
                       LBRACE(gr) |> Some );
      (fun (gr : FSA<_>) ->
                       RBRACE(gr) |> Some );
      (fun (gr : FSA<_>) ->
                       DIV(gr)|> Some );
      (fun (gr : FSA<_>) ->
                       PLUS(gr)|> Some );
      (fun (gr : FSA<_>) ->
                        POW(gr)|> Some );
      (fun (gr : FSA<_>) ->
                       MULT(gr)|> Some );

   |]


let alphabet () = 
 new HashSet<_>([| Smbl 65535; Smbl 9; Smbl 10; Smbl 13; Smbl 32; Smbl 40; Smbl 41; Smbl 42; Smbl 43; Smbl 45; Smbl 47; Smbl 48; Smbl 49; Smbl 50; Smbl 51; Smbl 52; Smbl 53; Smbl 54; Smbl 55; Smbl 56; Smbl 57; Smbl 46; Smbl 69; Smbl 101;|])

let tokenize eof approximation = Tokenize (fstLexer()) (actions()) (alphabet()) eof approximation
