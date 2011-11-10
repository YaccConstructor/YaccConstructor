namespace VSYardNS

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Classification
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Tagging
open Microsoft.VisualStudio.Utilities

type TokenTag () = class
    interface ITag 
    end