namespace Persimmon.Dried

open Persimmon
open Runner

type PropertyBuilder(name: string) =
  new() = PropertyBuilder("")
  member val RunnerParameters = Parameters.Default with get, set
  member val PrettyParameters = Pretty.Parameters.Default with get, set
  member this.Yield(()) = (this.RunnerParameters, this.PrettyParameters)
  [<CustomOperation("verbosity")>]
  member __.Verbosity((p, _), v) =
    (p, { Verbosity = v })
  [<CustomOperation("minSuccessfulTests")>]
  member __.MinSuccessfulTests((p, pp), v) =
    ({ p with MinSuccessfulTests = v }, pp)
  [<CustomOperation("minSize")>]
  member __.MinSize((p, pp), v) =
    ({ p with MinSize = v }, pp)
  [<CustomOperation("maxSize")>]
  member __.MaxSize((p, pp), v) =
    ({ p with MaxSize = v }, pp)
  [<CustomOperation("prngState")>]
  member __.PrngState((p, pp), v) =
    ({ p with Parameters.PrngState = v }, pp)
  [<CustomOperation("workers")>]
  member __.Workers((p, pp), v) =
    ({ p with Workers = v }, pp)
  [<CustomOperation("callback")>]
  member __.Callback((p, pp), v) =
    ({ p with Callback = v }, pp)
  [<CustomOperation("maxDiscardRatio")>]
  member __.MaxDiscardRatio((p, pp), v) =
    ({ p with MaxDiscardRatio = v }, pp)
  [<CustomOperation("apply")>]
  member __.Apply((prms, prettyPrms), p: Prop) =
    let meta = { Name = name; Parameters = [] }
    let body () =
      let res =
        p
        |> PropImpl.bind PropImpl.applyResult
        |> check prms
      match res.Status with
      | Proved _ | Passed ->
        Done(meta, NonEmptyList.singleton (AssertionResult.Passed()))
      | Failed _
      | Exhausted ->
        let v = Violated (Result.prettyTestRes res |> Pretty.pretty prettyPrms)
        Done(meta, NonEmptyList.singleton (NotPassed v))
      | PropException (_, e, _) ->
        let v = Violated (Result.prettyTestRes res |> Pretty.pretty prettyPrms)
        Error(meta, [e], [v])
    TestCase(meta, body)
  member __.Delay(f: unit -> _) = f
  member __.Run(f) =
    try
      f ()
    with e ->
      TestCase.makeError name [] e
