//  Copyright 2010-2011 Konstantin Ulitin
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace YC.Frontends.FsYaccFrontend

open YC.Core


/// Parser of FsYacc grammars. Usually it is files with .fsy extension.
/// Also might work with OCamlYacc grammars(.mly) 

type FsYaccFrontend() = 
    inherit Frontend()
        override this.Name = "FsYaccFrontend"
        override this.ParseGrammar t = 
            match t with
            | (:? System.String as s) -> Main.ParseFile s
            | _ -> IL.emptyGrammarDefinition
        override this.ProductionTypes =
            Reflection.FSharpType.GetUnionCases typeof<IL.Production<string,string>>
            |> List.ofArray
            |> List.map (fun unionCase -> unionCase.Name)
        override this.ParseGrammarFromStr str = 
            Main.ParseString str
    

// For testing switch to Console App and then switch back to Class Library
// Delete action code from yard_option_8 and yard_option_11
// Add %token HEAD
// Add <string> to some terminals

module Run = 

    open Microsoft.FSharp.Text.Lexing

   
//    let filename = @"..\..\..\AntlrToYard\Parser.fsy" 
    let filename = @"..\..\..\..\tests\data\FsYacc\5.fsy" 
//    let content = System.IO.File.ReadAllText(filename)
//    Lexer.source := content
//    let reader = new System.IO.StringReader(content)
//    let lexbuf = LexBuffer<_>.FromTextReader reader//LexBuffer<_>.FromChars  ("abc/* def */foo".ToCharArray())
//    let lexems = seq { 
//                       while not lexbuf.IsPastEndOfStream do
//                             yield Lexer.token lexbuf  
//                      }
//    lexems |> Seq.iter (fun x -> printf "%A ; " x)
    printf "%A\n" <| YC.Frontends.FsYaccFrontend.Main.ParseFile filename
    ()