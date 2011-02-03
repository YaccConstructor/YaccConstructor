//  AtmBuilder.fs contains functions for automata building
//
//  Copyright 2010 Semen Grigorev <rsdpisuy@gmail.com>
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


namespace Yard.Generators.RACCGenerator

type AtmBuilder(enumerator:Enumerator) =
    class
        let concat lAtm rAtm lbl = 
            {
                NIDToStateMap = Seq.fold 
                                    (fun (d:System.Collections.Generic.IDictionary<_,_>) elt -> d.Add(elt);d) 
                                    lAtm.NIDToStateMap
                                    rAtm.NIDToStateMap

                NStartState   = lAtm.NStartState
                NFinaleState  = rAtm.NFinaleState
                NRules        = lAtm.NRules + rAtm.NRules 
                                |> Set.add
                                        {
                                        FromStateID = lAtm.NFinaleState
                                        ToStateID   = rAtm.NStartState
                                        Label       = lbl
                                        Symbol      = Epsilon
                                        }
            }

        let alt atm1 atm2 alt1Slbl alt1Elbl alt2Slbl alt2Elbl =
            let startStateID = enumerator.Next()
            let finaleStateID = enumerator.Next()
            {        
            
                NIDToStateMap = 
                    let d = 
                        Seq.fold 
                            (fun (d:System.Collections.Generic.IDictionary<_,_>) elt -> d.Add(elt);d) 
                            atm1.NIDToStateMap
                            atm2.NIDToStateMap
                    d.Add(startStateID,startStateID)
                    d.Add(finaleStateID,finaleStateID)
                    d
                        

                NStartState   = startStateID
                NFinaleState  = finaleStateID
                NRules        =   atm1.NRules + atm2.NRules 
                                + (set [
                                        {
                                            FromStateID = startStateID
                                            ToStateID   = atm1.NStartState
                                            Label       = alt1Slbl
                                            Symbol      = Epsilon
                                        }
                                        ;{
                                            FromStateID = startStateID
                                            ToStateID   = atm2.NStartState
                                            Label       = alt2Slbl
                                            Symbol      = Epsilon
                                        }
                                        ;{
                                            FromStateID = atm1.NFinaleState
                                            ToStateID   = finaleStateID
                                            Label       = alt1Elbl
                                            Symbol      = Epsilon
                                        }
                                        ;{
                                            FromStateID = atm2.NFinaleState
                                            ToStateID   = finaleStateID
                                            Label       = alt2Elbl
                                            Symbol      = Epsilon
                                        }
                                     ])
            }

        let cls atm clsSlbl clsElbl =
            let startStateID = enumerator.Next()
            let finaleStateID = enumerator.Next()
            {        
            
                NIDToStateMap = 
                    let d = atm.NIDToStateMap
                    d.Add(startStateID,startStateID)
                    d.Add(finaleStateID,finaleStateID)
                    d
                        

                NStartState   = startStateID
                NFinaleState  = finaleStateID
                NRules        =   atm.NRules
                                + set
                                    [
                                        {
                                            FromStateID = atm.NStartState
                                            ToStateID   = atm.NFinaleState
                                            Label       = Omega
                                            Symbol      = Epsilon
                                        }
                                        ;{
                                            FromStateID = atm.NFinaleState
                                            ToStateID   = atm.NStartState
                                            Label       = Omega
                                            Symbol      = Epsilon
                                        }
                                        ;{
                                            FromStateID = startStateID
                                            ToStateID   = atm.NStartState
                                            Label       = clsSlbl
                                            Symbol      = Epsilon
                                        }
                                        ;{
                                            FromStateID = atm.NFinaleState
                                            ToStateID   = finaleStateID
                                            Label       = clsElbl
                                            Symbol      = Epsilon
                                        }
                                    ]                                         
            }

        let opt atm clsSlbl clsElbl =
            let startStateID = enumerator.Next()
            let finaleStateID = enumerator.Next()
            {        
            
                NIDToStateMap = 
                    let d = atm.NIDToStateMap
                    d.Add(startStateID,startStateID)
                    d.Add(finaleStateID,finaleStateID)
                    d
                        

                NStartState   = startStateID
                NFinaleState  = finaleStateID
                NRules        =   atm.NRules
                                + set
                                    [
                                        {
                                            FromStateID = atm.NStartState
                                            ToStateID   = atm.NFinaleState
                                            Label       = Omega
                                            Symbol      = Epsilon
                                        }                                        
                                        ;{
                                            FromStateID = startStateID
                                            ToStateID   = atm.NStartState
                                            Label       = clsSlbl
                                            Symbol      = Epsilon
                                        }
                                        ;{
                                            FromStateID = atm.NFinaleState
                                            ToStateID   = finaleStateID
                                            Label       = clsElbl
                                            Symbol      = Epsilon
                                        }
                                    ]                                         
            }


        let addInHead sttInfo smb lbl atm =
            let stateID = enumerator.Next()
            let stateVal = if Option.isNone sttInfo then stateID else Option.get sttInfo
            {
                NIDToStateMap = 
                    let d = System.Collections.Generic.Dictionary(atm.NIDToStateMap)
                    //let p = Seq.append d (dict[stateID,stateVal])
                    //System.Collections.Generic.Dictionary(d)
                    d.Add(stateID,stateVal)
                    d
                    
                        

                NStartState   = stateID
                NFinaleState  = atm.NFinaleState
                NRules        = atm.NRules 
                                |>Set.add
                                    {
                                        FromStateID = stateID
                                        ToStateID   = atm.NStartState
                                        Label       = lbl
                                        Symbol      = smb
                                    } 
            }

        let append sttInfo smb lbl atm =        
            let stateID = enumerator.Next()
            let stateVal = if Option.isNone sttInfo then stateID else Option.get sttInfo
            {
                NIDToStateMap = 
                    let d = atm.NIDToStateMap
                    d.Add(stateID,stateVal)                    
                    d
                        

                NStartState   = atm.NStartState
                NFinaleState  = stateID
                NRules        = atm.NRules 
                                |>Set.add
                                    {
                                        FromStateID = atm.NFinaleState
                                        ToStateID   = stateID
                                        Label       = lbl
                                        Symbol      = smb
                                    } 
            }

        let trivial sttInfo1 sttInfo2 smb lbl =
            let startStateID = enumerator.Next()
            let finaleStateID = enumerator.Next()
            let startStateVal = if Option.isNone sttInfo1 then startStateID else Option.get sttInfo1
            let finaleStateVal = if Option.isNone sttInfo2 then finaleStateID else Option.get sttInfo2
            {
                NIDToStateMap = dict [(startStateID,startStateVal);(finaleStateID,finaleStateVal)]                        
                NStartState   = startStateID
                NFinaleState  = finaleStateID
                NRules        = Set.singleton
                                    {
                                        FromStateID = startStateID
                                        ToStateID   = finaleStateID
                                        Label       = lbl
                                        Symbol      = smb
                                    }
            }

        member self.Concat lAtm rAtm lbl = concat lAtm rAtm lbl
        member self.Alt atm1 atm2 alt1Slbl alt1Elbl alt2Slbl alt2Elbl = 
            alt atm1 atm2 alt1Slbl alt1Elbl alt2Slbl alt2Elbl
        member self.Cls  atm clsSlbl clsElbl = cls atm clsSlbl clsElbl
        member self.Opt  atm clsSlbl clsElbl = opt atm clsSlbl clsElbl
        member self.AddInHead sttInfo smb lbl atm = addInHead sttInfo smb lbl atm
        member self.Append sttInfo smb lbl atm = append sttInfo smb lbl atm
        member self.Trivial sttInfo1 sttInfo2 smb lbl = trivial sttInfo1 sttInfo2 smb lbl
    end