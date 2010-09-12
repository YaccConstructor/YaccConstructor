// AtmBuilder.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.
//
//Implementation of NLFA to DLFA convertion

namespace Yard.Generators.RACC

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
                    let d = 
                        atm.NIDToStateMap
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


        member self.Concat lAtm rAtm lbl = concat lAtm rAtm lbl
        member self.Alt atm1 atm2 alt1Slbl alt1Elbl alt2Slbl alt2Elbl = 
            alt atm1 atm2 alt1Slbl alt1Elbl alt2Slbl alt2Elbl
        member self.Cls  atm clsSlbl clsElbl = cls atm clsSlbl clsElbl
    end