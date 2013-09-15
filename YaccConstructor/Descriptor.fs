module Yard.Gemnerators.GLL.Descriptor
open System
open System.Collections.Generic

[<Struct>]
type Descriptor =
   struct
      val Index: int
      val Label: int
      val Stack: Collections.Stack
      new(i: int, l: int, s: Collections.Stack) = { Index = i; Label = l; Stack = s }
   end



    