//  Program.fs
//
//  Copyright 2010 Anastasia Nishnevich <Anastasia.Nishnevich@gmail.com>
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software:you can redistribute it and/or modify
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


module Program


open FsCheck
open FsCheck.Prop
open Microsoft.FSharp.Text.Lexing
open Yard.Core.Main
open Main.Program
open Yard.Core.GrammarParser
open NUnit.Framework



[<TestFixture>]
type ``RACC core tests`` () =    
    inherit RACCCoreTests.``RACC core tests``()
        


[<TestFixture>]
type ``YARD frontend Tests`` ()=    
    
           
//FrontEnd 
//main tests
        (*let fpars = ParseFile @"..\..\..\..\Tests\test002.yrd" 
        [<Test>] member test.``Run_Common test`` () = 
                   let rcom = run_common @"..\..\..\..\Tests\test002.yrd"
                   Assert.AreEqual(rcom.ToString(), "Microsoft.FSharp.Text.Lexing.LexBuffer`1[System.Char]") 



        [<Test>] member test.``ParseFile test`` () =
                   
                   Assert.AreEqual(fpars.ToString(), "Yard.Core.IL+Definition+t`2[System.Tuple`2[System.String,System.Tuple`2[System.Int32,System.Int32]],System.Tuple`2[System.String,System.Tuple`2[System.Int32,System.Int32]]]")
           


        [<Test>] member test.``ApplyConvertion Test`` () = 
                   let s = ApplyConvertion fpars (new Yard.Core.Convertions.ExpandMeta.ExpandMeta())
                   Assert.AreEqual(s.ToString(),"")*)
                   
// FrontEnd 
// Parser tests 
                 

 
        
       
        [<Test>] member test.``tagOfToken test`` () =
                    let tagOfTokenProp (t:token)=  (tagOfToken t >= 0) && (tagOfToken t <= 19)
                    Check.Quick  tagOfTokenProp


        [<Test>] member test.``tokenTagToTokenId test`` () =
                    let tokenTagToTokenIdProp (i:int ) = ((i>=0) && (i<=20)) ==> (lazy((tokenTagToTokenId i).GetType() = typeof<tokenId>)) 
                    let excProp(i:int) = ((i<0) || (i>20))==> throws<System.Exception,_> (lazy (raise <| System.Exception("tokenTagToTokenId: bad token")))
                    Check.Quick tokenTagToTokenIdProp  
                    Check.Quick excProp 
        

        [<Test>] member test.``tokenToString test`` () = 
                     let strList = ["PATTERN";"PARAM";"PREDICATE";"ACTION";"STRING";"LIDENT";"UIDENT";"COMMUT";"DLESS";"DGREAT";
                                        "RPAREN";"LPAREN";"QUESTION"; 
                                        "PLUS";"STAR";"BAR";"EQUAL";"SEMICOLON";"COLON";"EOF" ] 
                            
                            
                     let tokenTostringProp (t:token) = List.exists(fun e -> e = token_to_string t ) strList
                     Check.Quick tokenTostringProp







