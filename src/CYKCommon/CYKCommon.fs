//  CYK.fs contains common types and functions for CYK.
//
//  Copyright 2012 Semen Grigorev <rsdpisuy@gmail.com>
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

namespace Yard.Generators.CYKGenerator

//(16|   16| 16|8       |       8  |)
//(E |->  Z|  X|<lblName|lblWeight>|)
type rule = uint64 

type CYKGrammar = array<rule>

[<Struct>]
type Rule =
    val RuleName : uint16
    val R1 : uint16
    val R2 : uint16
    val Label : uint8
    val Weight : uint8
    new (ruleName : uint16, r1 : uint16, r2 : uint16, lbl : uint8, weight : uint8) = 
        { RuleName = ruleName; R1 = r1; R2 = r2; Label = lbl; Weight = weight }
   
[<AutoOpen>]
module RuleHelpers =
    
    let buildRule rName r1 r2 lblName lblWeight =
        let lbl = (uint16 lblName <<< 8) ||| uint16 lblWeight
        let r1 = (uint32 rName <<< 16) ||| uint32 r1
        let r2 = (uint32 r2 <<< 16) ||| uint32 lbl
        let r =  (uint64 r1 <<< 32) ||| uint64 r2
        r

    let getRuleCortege (rule:rule) =
        let r1,r2 = uint32 ((rule >>> 32) &&&  0xFFFFFFFFUL), uint32 (rule &&& 0xFFFFFFFFUL)
        let rName,r1 = uint16 ((r1 >>> 16) &&& 0xFFFFFFFFu), uint16 (r1 &&& 0xFFFFFFFFu)
        let r2,lbl = uint16 ((r2 >>> 16) &&& 0xFFFFFFFFu), uint16 (r2 &&& 0xFFFFFFFFu)
        let lblName,lblWeight = uint8 ((lbl >>> 8) &&& uint16 0xFFFFFFFFu), uint8 (lbl &&& uint16 0xFFFFFFFFu)
        rName, r1, r2, lblName, lblWeight

    let getRuleStruct (rule:rule) =
        let r1,r2 = uint32 ((rule >>> 32) &&&  0xFFFFFFFFUL), uint32 (rule &&& 0xFFFFFFFFUL)
        let rName,r1 = uint16 ((r1 >>> 16) &&& 0xFFFFFFFFu), uint16 (r1 &&& 0xFFFFFFFFu)
        let r2,lbl = uint16 ((r2 >>> 16) &&& 0xFFFFFFFFu), uint16 (r2 &&& 0xFFFFFFFFu)
        let lblName,lblWeight = uint8 ((lbl >>> 8) &&& uint16 0xFFFFFFFFu), uint8 (lbl &&& uint16 0xFFFFFFFFu)
        new Rule(rName, r1, r2, lblName, lblWeight)

type CYKToken<'tag,'value>(tag:'tag, value:'value) =
    member x.Tag = tag
    member x.Value = value
