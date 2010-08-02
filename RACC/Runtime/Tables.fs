// Table.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Core.Tables

open Yard.Core.Grammar

type RuleToActionTable(fileName) =
    inherit TypeConvertableTable<int,string>(fileName)
    override this.fileSuffix = "rule_to_action"
    
type GotoTables(fileName) =
    inherit TypeConvertableTable<int,Set<Item.t<string>>>(fileName)
    override this.fileSuffix = "goto"
    
type ItemsTable(fileName) =
    inherit Table<Set<Item.t<string>>>(fileName)
    override this.fileSuffix = "items"
    
type StartTNetmsTable(fileName) =
    inherit Table<string list>(fileName)
    override this.fileSuffix = "start_nterms"    