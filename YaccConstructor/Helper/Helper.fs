module Yard.Core.Helper

open Yard.Core
open Yard.Core.IL
open Yard.Core.IL.Production
open Yard.Core.IL.Definition
open Yard.Core.Checkers
open NUnit.Framework
open System.Linq
open System.IO

let filesAreEqual file1 file2 =
    let all1 = File.ReadAllBytes file1
    let all2 = File.ReadAllBytes file2
    Assert.AreEqual (all1.Length, all2.Length)
    Assert.IsTrue(Array.forall2 (=) all1 all2)