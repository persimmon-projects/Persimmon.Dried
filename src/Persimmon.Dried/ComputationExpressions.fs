namespace Persimmon.Dried

open System.Diagnostics
open Persimmon
open Runner

type PropertiesState<'T> = {
  RunnerParams: Parameters
  PrettyParams: PrettyParameters
  Properties: Prop seq
  Sample: 'T
}

type PropertiesBuilder(name: string) =
  new() = PropertiesBuilder("")
  member __.Yield(()) = {
    RunnerParams = Parameters.Default
    PrettyParams = Pretty.Parameters.Default
    Properties = Seq.empty
    Sample = ()
  }
  [<CustomOperation("verbosity")>]
  member __.Verbosity(s, v) =
    { s with PrettyParams = { Verbosity = v } }
  [<CustomOperation("minSuccessfulTests")>]
  member __.MinSuccessfulTests(s, v) =
    { s with RunnerParams = { s.RunnerParams with MinSuccessfulTests = v } }
  [<CustomOperation("minSize")>]
  member __.MinSize(s, v) =
    { s with RunnerParams = { s.RunnerParams with MinSize = v } }
  [<CustomOperation("maxSize")>]
  member __.MaxSize(s, v) =
    { s with RunnerParams = { s.RunnerParams with MaxSize = v } }
  [<CustomOperation("prngState")>]
  member __.PrngState(s, v) =
    { s with RunnerParams = { s.RunnerParams with PrngState = v } }
  [<CustomOperation("workers")>]
  member __.Workers(s, v) =
    { s with RunnerParams = { s.RunnerParams with Workers = v } }
  [<CustomOperation("callback")>]
  member __.Callback(s, v) =
    { s with RunnerParams = { s.RunnerParams with Callback = v } }
  [<CustomOperation("maxDiscardRatio")>]
  member __.MaxDiscardRatio(s, v) =
    { s with RunnerParams = { s.RunnerParams with MaxDiscardRatio = v } }
  [<CustomOperation("apply")>]
  member __.Apply(s, p) =
    { s with Properties = seq { yield! s.Properties; yield p } }
  [<CustomOperation("applyReturn")>]
  member __.ApplyReturn(s, p: Prop<'T>) =
    {
      RunnerParams = s.RunnerParams
      PrettyParams = s.PrettyParams
      Properties = seq { yield! s.Properties; yield p :> Prop }
      Sample = p.Sample
    }
  member __.Delay(f: unit -> _) = f
  member __.Run(f) =
    try
      let s = f ()
      let meta = { Name = Some name; Parameters = [] }
      let body () =
        let watch = Stopwatch.StartNew()
        let res = s.Properties |> PropImpl.all |> check s.RunnerParams
        watch.Stop()
        match res.Status with
        | Proved _ | Passed ->
          Done(meta, NonEmptyList.singleton (AssertionResult.Passed s.Sample), watch.Elapsed)
        | Skipped s ->
          Done(meta, NonEmptyList.singleton (AssertionResult.NotPassed (NotPassedCause.Skipped s)), watch.Elapsed)
        | Failed _
        | Exhausted ->
          let v = Violated (Result.prettyTestRes res |> Pretty.pretty s.PrettyParams)
          Done(meta, NonEmptyList.singleton (NotPassed v), watch.Elapsed)
        | PropException (_, e, _) ->
          let v = Violated (Result.prettyTestRes res |> Pretty.pretty s.PrettyParams)
          Error(meta, [e], [v], watch.Elapsed)
      TestCase(meta, body)
    with e ->
      TestCase.makeError (Some name) [] e
