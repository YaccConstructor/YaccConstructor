// Tables.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators._RACCGenerator

type Tables<'gt, 'item, 'atmDictKey, 'stateSmb, 'stateVal, 'lbl 
             when 
                'gt : comparison 
                and 'stateVal : comparison 
                and 'lbl : comparison> =
    {
        gotoSet      : Set<(int*'gt)>;
        automataDict : System.Collections.Generic.IDictionary<'atmDictKey,DLFA<'stateSmb, 'stateVal, 'lbl>>;
        items        : List<'item>;
    }