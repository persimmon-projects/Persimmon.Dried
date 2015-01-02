module Persimmon.Dried.Runner

open FsRandom

type Parameters = {
  MinSuccessfulTests: int
  MinSize: int
  MaxSize: int
  PrngState: PrngState
  Workers: int
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
    MaxDiscardRatio = 5.0f
  }

type Status =
  | Passed
  | Proved of PropArg<obj> list
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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Result =

  let isPassed r =
    match r.Status with
    | Passed | Proved _ -> true
    | _ -> false

module Runner =

  open System

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
     
  let check prms (p: Prop) =

    assertParams(prms)

    let iterations = ceil (float prms.MinSuccessfulTests / float prms.Workers)
    let sizeStep = float (prms.MaxSize - prms.MinSize) / (iterations * float prms.Workers)
    let stop = ref false
    let genPrms = { Gen.Parameters.Default with PrngState = prms.PrngState }

    let workerFun workerIdx =
      let mutable n = 0  // passed tests
      let mutable d = 0  // discarded tests
      let mutable res: Option<Result> = None
      let mutable fm = FreqMap.empty<obj list>
      let mutable rng = genPrms.PrngState
      while not !stop && Option.isNone res && float n < iterations do
        let size = (float prms.MinSize) + (sizeStep * float (workerIdx + (prms.Workers * (n + d))))
        let propRes = p.Apply({ Size = round size |> int; PrngState = rng })
        fm <- if List.isEmpty propRes.Collected then fm else  FreqMap.add propRes.Collected fm
        match propRes.Status with
        | Undecided ->
          d <- d + 1
          if n + d > prms.MinSuccessfulTests && 1 + prms.Workers * int prms.MaxDiscardRatio * n < d then
            res <- Some { Status = Exhausted; Succeeded = n; Discarded = d; FreqMap = fm; Time = 0L }
        | True ->
          n <- n + 1
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
          rng <- rng.Next64Bits() |> snd
      match res with
      | None ->
        if int prms.MaxDiscardRatio * n > d then { Status = Passed; Succeeded = n; Discarded = d; FreqMap = fm; Time = 0L }
        else { Status = Exhausted; Succeeded = n; Discarded = d; FreqMap = fm; Time = 0L }
      | Some res -> res

    let merge r1 r2 =
      let { Status = st1; Succeeded = s1; Discarded = d1; FreqMap = fm1; Time = _ } = r1
      let { Status = st2; Succeeded = s2; Discarded = d2; FreqMap = fm2; Time = _ } = r1
      if st1 <> Passed && st1 <> Exhausted then
        { Status = st1; Succeeded = s1 + s2; Discarded = d1 + d2; FreqMap = FreqMap.append fm1 fm2; Time = 0L }
      elif st2 <> Passed && st2 <> Exhausted then
        { Status = st2; Succeeded = s1 + s2; Discarded = d1 + d2; FreqMap = FreqMap.append fm1 fm2; Time = 0L }
      else
        if s1 + s2 >= prms.MinSuccessfulTests && int prms.MaxDiscardRatio * (s1 + s2) >= (d1 + d2) then
          { Status = Passed; Succeeded = s1 + s2; Discarded = d1 + d2; FreqMap = FreqMap.append fm1 fm2; Time = 0L }
        else
          { Status = Exhausted; Succeeded = s1 + s2; Discarded = d1 + d2; FreqMap = FreqMap.append fm1 fm2; Time = 0L }

    let start = DateTime.UtcNow.Ticks

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

    { r with Time = DateTime.UtcNow.Ticks - start }

  let run prms p = check prms p |> ignore

  let mainRunner prms p =
    if Result.isPassed <| check prms p then 0
    else -1
