module Logger

open QuickGraph.FSA.GraphBasedFsa
open GenericGraphs

type LoggerState<'a when 'a: equality> = {
    PathMaker: string -> string
    IsOn: bool
    FsaToDot: FSA<'a> -> string-> unit }

let create pathMaker isOn fsaToDot = {
    PathMaker = pathMaker
    IsOn = isOn
    FsaToDot = fsaToDot }

let disabledLogger = {
    PathMaker = id
    IsOn = false
    FsaToDot = fun f s -> () }

let private doIfNeeded action (state: LoggerState<_>) =
    if state.IsOn
    then action ()

let logGenericCfg cfg methodName (state: LoggerState<_>) =
    let a = fun () -> BidirectGraphFuns.toDot cfg methodName <| state.PathMaker ("cfg_" + methodName + ".dot")
    doIfNeeded a state

let logPreDdg ddg methodName (state: LoggerState<_>) =
    let a = fun () -> BidirectGraphFuns.toDot ddg methodName <| state.PathMaker ("preddg_" + methodName + ".dot")
    doIfNeeded a state

let logDdg ddg methodName (state: LoggerState<_>) =
    let a = fun () -> BidirectGraphFuns.toDot ddg methodName <| state.PathMaker ("ddg_" + methodName + ".dot")
    doIfNeeded a state

let logFsa fsa methodName (state: LoggerState<_>) =
    let a = fun () -> state.FsaToDot fsa <| state.PathMaker ("fsa_" + methodName + ".dot")
    doIfNeeded a state