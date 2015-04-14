module Persimmon.Dried.Runner

open FsRandom

type Status =
  | Passed
  | Proved of PropArg<obj> list
  | Skipped of string
  | Failed of PropArg<obj> list * Set<string>
  | Exhausted
  | PropException of PropArg<obj> list * exn * Set<string>

type Result = {
  Status: Status
  Succeeded: int
  Discarded: int
  FreqMap: FreqMap<obj list>
  Time: int64
}

type TestCallback() =
  abstract member OnPropEval: string * int * int * int -> unit
  default __.OnPropEval(_, _, _, _) = ()
  abstract member OnTestResult: string * Result -> unit
  default __.OnTestResult(_, _) = ()
  member this.Chain(testCallback: TestCallback): TestCallback = { new TestCallback() with
      override __.OnPropEval(name, threadIdx, succeeded, discarded) =
        this.OnPropEval(name, threadIdx, succeeded, discarded)
        testCallback.OnPropEval(name, threadIdx, succeeded, discarded)
      override __.OnTestResult(name, result) =
        this.OnTestResult(name, result)
        testCallback.OnTestResult(name, result) }

type Parameters = {
  MinSuccessfulTests: int
  MinSize: int
  MaxSize: int
  PrngState: PrngState
  Workers: int
  Callback: TestCallback
  MaxDiscardRatio: float32
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Parameters =

  let Default = {
    MinSuccessfulTests = 100
    MinSize = 0
    MaxSize = Gen.Parameters.Default.Size
    PrngState = Gen.Parameters.Default.PrngState
    Workers = 1
    Callback = new TestCallback()
    MaxDiscardRatio = 5.0f
  }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Result =

  let isPassed r =
    match r.Status with
    | Passed | Proved _ | Skipped _ -> true
    | _ -> false

  open Pretty
  open Helper

  let prettyTestRes res = Pretty(fun prms ->
    let labels ls =
      if Set.isEmpty ls then ""
      else "> Labels of failing property: " -/ (ls |> Set.map (fun x -> x.ToString()) |> String.concat newLine)
    let s =
      match res.Status with
      | Proved(args) -> "OK, proved property." -/ (PropArg.pretty args |> pretty prms)
      | Passed -> "OK, passed "+ string res.Succeeded + " tests."
      | Skipped s -> "OK, skipped tests." -/ ("  reason: " + s)
      | Failed(args, l) ->
        "Falsified after " + string res.Succeeded + " passed tests."
          -/ labels l
          -/ (PropArg.pretty args |> pretty prms)
      | Exhausted ->
        "Gave up after only "+ string res.Succeeded + " passed tests. " + string res.Discarded + " tests were discarded."
      | PropException(args,e,l) ->
        "Exception raised on property evaluation."
          -/ labels l
          -/ (PropArg.pretty args |> pretty prms)
          -/ "> Exception: "
          + (prettyExn e |> pretty prms)
    let t = if prms.Verbosity <= 1 then "" else "Elapsed time: " + prettyTime res.Time
    s -/ t -/ pretty prms (FreqMap.pretty res.FreqMap)
  )

module private Impl =

  open System.Diagnostics

  let assertParams prms =
    if prms.MinSuccessfulTests <= 0 then
      invalidArg "MinSuccessfulTests" "require: MinSuccessfulTest > 0"
    elif prms.MaxDiscardRatio <= 0.0f then
      invalidArg "MaxDiscardRatio" "require: MaxDiscardRatio > 0.0f"
    elif prms.MinSize < 0 then
      invalidArg "MinSize" "require: MinSize >= 0"
    elif prms.MaxSize < prms.MinSize then
      invalidArg "MaxSize" "require: MinSize <= MaxSize"
    elif prms.Workers <= 0 then
      invalidArg "Workers" "require: Workers > 0"
     
  let check name prms (p: Prop) =

    assertParams(prms)

    let iterations = ceil (float prms.MinSuccessfulTests / float prms.Workers)
    let sizeStep = float (prms.MaxSize - prms.MinSize) / (iterations * float prms.Workers)
    let stop = ref false

    let workerFun workerIdx =
      let mutable n = 0  // passed tests
      let mutable d = 0  // discarded tests
      let mutable res: Option<Result> = None
      let mutable fm = FreqMap.empty<obj list>
      let mutable rng = prms.PrngState
      while not !stop && Option.isNone res && float n < iterations do
        let size = float prms.MinSize + (sizeStep * float (workerIdx + (prms.Workers * (n + d))))
        let propRes = p.Apply({ Size = round size |> int; PrngState = rng })
        fm <- if List.isEmpty propRes.Collected then fm else  FreqMap.add propRes.Collected fm
        match propRes.Status with
        | Undecided ->
          d <- d + 1
          prms.Callback.OnPropEval("", workerIdx, n, d)
          if n + d > prms.MinSuccessfulTests && 1.0f + float32 prms.Workers * prms.MaxDiscardRatio * float32 n < float32 d then
            res <- Some { Status = Exhausted; Succeeded = n; Discarded = d; FreqMap = fm; Time = 0L }
        | True ->
          n <- n + 1
          prms.Callback.OnPropEval("", workerIdx, n, d)
        | Proof ->
          n <- n + 1
          res <- Some { Status = Proved propRes.Args; Succeeded = n; Discarded = d; FreqMap = fm; Time = 0L }
          stop := true
        | False ->
          res <- Some { Status = Failed(propRes.Args, propRes.Labels); Succeeded = n; Discarded = d; FreqMap = fm; Time = 0L }
          stop := true
        | Exception e ->
          res <- Some { Status = PropException(propRes.Args, e, propRes.Labels); Succeeded = n; Discarded = d; FreqMap = fm; Time = 0L }
          stop := true
        | PropStatus.Skipped s ->
          res <- Some { Status = Skipped s; Succeeded = n; Discarded = d; FreqMap = fm; Time = 0L }
          stop := true
        rng <- rng.Next64Bits() |> snd
      match res with
      | None ->
        if prms.MaxDiscardRatio * float32 n > float32 d then { Status = Passed; Succeeded = n; Discarded = d; FreqMap = fm; Time = 0L }
        else { Status = Exhausted; Succeeded = n; Discarded = d; FreqMap = fm; Time = 0L }
      | Some res -> res

    let merge r1 r2 =
      let { Status = st1; Succeeded = s1; Discarded = d1; FreqMap = fm1; Time = _ } = r1
      let { Status = st2; Succeeded = s2; Discarded = d2; FreqMap = fm2; Time = _ } = r2
      if st1 <> Passed && st1 <> Exhausted then
        { Status = st1; Succeeded = s1 + s2; Discarded = d1 + d2; FreqMap = FreqMap.append fm1 fm2; Time = 0L }
      elif st2 <> Passed && st2 <> Exhausted then
        { Status = st2; Succeeded = s1 + s2; Discarded = d1 + d2; FreqMap = FreqMap.append fm1 fm2; Time = 0L }
      else
        if s1 + s2 >= prms.MinSuccessfulTests && prms.MaxDiscardRatio * float32 (s1 + s2) >= float32 (d1 + d2) then
          { Status = Passed; Succeeded = s1 + s2; Discarded = d1 + d2; FreqMap = FreqMap.append fm1 fm2; Time = 0L }
        else
          { Status = Exhausted; Succeeded = s1 + s2; Discarded = d1 + d2; FreqMap = FreqMap.append fm1 fm2; Time = 0L }

    let watch = Stopwatch.StartNew()

    let r =
      if prms.Workers < 2 then workerFun 0
      else
        try
          let fs = [ 0 .. prms.Workers ] |> List.map (fun idx -> async {
            return workerFun idx
          })
          let zeroRes = { Status = Passed; Succeeded = 0; Discarded = 0; FreqMap = FreqMap.empty; Time = 0L }
          async {
            let! l = Async.Parallel fs
            return Seq.fold merge zeroRes l
          }
          |> Async.RunSynchronously
        finally
          stop := true

    watch.Stop()
    let timedRes = { r with Time = watch.ElapsedMilliseconds }
    prms.Callback.OnTestResult(name, timedRes)
    timedRes

  let run name prms p = check name prms p |> ignore

  let mainRunner prms p =
    if Result.isPassed <| check "" prms p then 0
    else -1

let check prms p = Impl.check "" prms p
let run name prms p = Impl.run name prms p
let mainRunner prms p = Impl.mainRunner prms p

let createConsoleReporter verbosity =
  let prettyPrms = { Verbosity = verbosity }
  { new TestCallback() with
    override __.OnTestResult(name, res) =
      if verbosity > 0 then
        if name = "" then
          let s =
            (if Result.isPassed res then "+ " else "! ")
              + Pretty.pretty prettyPrms (Result.prettyTestRes res)
          printf "\r%s\n" (Pretty.format s "" "" 75)
        else
          let s =
            (if Result.isPassed res then "+ " else "! ") + name + ": "
             + Pretty.pretty prettyPrms (Result.prettyTestRes res)
          printf "\r%s\n" (Pretty.format s "" "" 75) }
