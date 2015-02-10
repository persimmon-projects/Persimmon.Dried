namespace Persimmon.Dried

open System

type PropArg<'T> = {
  Label: string
  Arg: 'T
  Shrinks: int
  OrigArg: 'T
  PrettyArg: Pretty
  PrettyOrigArg: Pretty
}
with
  member this.BoxedTypeParam() = {
    Label = this.Label
    Arg = box this.Arg
    Shrinks = this.Shrinks
    OrigArg = box this.OrigArg
    PrettyArg = this.PrettyArg
    PrettyOrigArg = this.PrettyOrigArg
  }

module PropArg =

  open Pretty
  open Helper

  let pretty args = Pretty(fun prms ->
    if Seq.isEmpty args then ""
    else
      seq {
        for (a, i) in args |> Seq.mapi (fun i a -> (a, i)) ->
          let l = "> "+ (if a.Label = "" then "ARG_" + string i else a.Label)
          let s =
            if a.Shrinks = 0 then ""
            else newLine + l + "_ORIGINAL: " + a.PrettyOrigArg.Apply(prms)
          l + ": " + a.PrettyArg.Apply(prms) + "" + s
      }
      |> String.concat newLine)

[<CustomEquality;NoComparison>]
type PropStatus =
  | Proof
  | True
  | False
  | Undecided
  | Exception of exn
  | Skipped of string
with
  override this.Equals(other: obj) =
    match other with
    | :? PropStatus as other ->
      match this, other with
      | Proof, Proof
      | True, True
      | False, False
      | Undecided, Undecided
      | Skipped _, Skipped _
      | Exception _, Exception _ -> true
      | _ -> false
    | _ -> false
  override this.GetHashCode() =
    match this with
    | Proof -> 0
    | True -> 1
    | False -> 2
    | Undecided -> 3
    | Exception _ -> 4
    | Skipped _ -> 5

type PropResult = {
  Status: PropStatus
  Args: PropArg<obj> list
  Labels: Set<string>
  Collected: obj list
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PropResult =

  let isSuccess r =
    match r.Status with
    | Proof | True | Skipped _ -> true
    | _ -> false

  let isFailure r =
    match r.Status with
    | False | Exception _ -> true
    | _ -> false

  let isProved r = r.Status = Proof

  let addArg (a: PropArg<'T>) r = { r with Args = a.BoxedTypeParam() :: r.Args }
  let collect (a: 'T) r = { r with Collected = box a :: r.Collected }
  let label s r = { r with Labels = Set.add s r.Labels }

  let merge x y st = {
    Status = st
    Args = List.append x.Args y.Args
    Labels = x.Labels + y.Labels
    Collected = List.append x.Collected y.Collected
  }

  let (&&&) (l: PropResult) (r: PropResult) =
    match l.Status, r.Status with
    | (Skipped _, _) -> l
    | (_, Skipped _) -> r
    | (Exception _,_) -> l
    | (_, Exception _) -> r
    | (False, _) -> l
    | (_, False) -> r
    | (Undecided, _) -> l
    | (_, Undecided) -> r
    | (_, Proof) -> merge l r l.Status
    | (Proof, _) -> merge l r r.Status
    | (True, True) -> merge l r True

  let (|||) (l: PropResult) (r: PropResult) =
    match l.Status, r.Status with
    | (Skipped _, _) -> l
    | (_, Skipped _) -> r
    | (Exception _, _) -> l
    | (_, Exception _) -> r
    | (False, False) -> merge l r False
    | (False, _) -> r
    | (_, False) -> l
    | (Proof, _) -> l
    | (_, Proof) -> r
    | (True, _) -> l
    | (_, True) -> r
    | (Undecided, Undecided) -> merge l r Undecided

  let (++) (l: PropResult) (r: PropResult) =
    match l.Status, r.Status with
    | (Skipped _, _) -> l
    | (_, Skipped _) -> r
    | (Exception _, _) -> l
    | (_, Exception _) -> r
    | (_, Undecided) -> l
    | (Undecided, _) -> r
    | (_, Proof) -> l
    | (Proof, _) -> r
    | (_, True) -> l
    | (True, _) -> r
    | (False, _) -> l

  let (==>) (l: PropResult) (r: PropResult) =
    match l.Status, r.Status with
    | (Skipped _,_) -> l
    | (_, Skipped _) -> r
    | (Exception _,_) -> l
    | (_, Exception _) -> r
    | (False, _) -> merge l r Undecided
    | (Undecided, _) -> l
    | (Proof, _) -> merge l r r.Status
    | (True, _) -> merge l r r.Status

[<AbstractClass>]
type Prop internal () =

  abstract member Apply: GenParameters -> PropResult

  override __.ToString() = "Prop"

module internal PropImpl =

  open PropResult

  let apply f = { new Prop() with
    member __.Apply(prms) =
      try
        f prms
      with e ->
        { Status = Exception e; Args = []; Labels = Set.empty; Collected = [] }
  }

  let map f (p: Prop) = apply (p.Apply >> f)

  let bind (f: _ -> Prop) (p: Prop) =
    apply (fun prms -> (p.Apply(prms) |> f).Apply({ prms with PrngState = prms.PrngState.Next64Bits () |> snd }))

  let combine f p1 p2 =
    p1 |> bind (fun r1 ->
    p2 |> bind (fun r2 ->
      { new Prop() with member __.Apply(_) = f r1 r2 }))

  let applyResult r = apply (fun _ -> r)

  let provedToTrue r =
    match r.Status with
    | Proof -> { r with Status = True }
    | _ -> r

  let undecided = lazy applyResult { Status = Undecided; Args = []; Labels = Set.empty; Collected = [] }
  let falsified = lazy applyResult { Status = False; Args = []; Labels = Set.empty; Collected = [] }
  let proved = lazy applyResult { Status = Proof; Args = []; Labels = Set.empty; Collected = [] }
  let passed = lazy applyResult { Status = True; Args = []; Labels = Set.empty; Collected = [] }
  let exn (e: exn) = applyResult { Status = Exception e; Args = []; Labels = Set.empty; Collected = [] }
  let skip s = applyResult { Status = Skipped s; Args = []; Labels = Set.empty; Collected = [] }

  let applyBool b = if b then proved.Value else falsified.Value

  let sizedProp (f: int -> Prop) =
    apply (fun prms -> provedToTrue ((f prms.Size).Apply(prms)))

  let all (ps: Prop seq) =
    if Seq.isEmpty ps then proved.Value
    else
      apply (fun prms ->
        ps
        |> Seq.map (fun p -> p.Apply(prms))
        |> Seq.reduce (&&&))

  let atLeastOne (ps: Prop seq) =
    if Seq.isEmpty ps then falsified.Value
    else
      apply (fun prms ->
        ps
        |> Seq.map (fun p -> p.Apply(prms))
        |> Seq.reduce (|||))

  let secure (p: unit -> #Prop) =
    try p () :> Prop
    with e -> exn e

  module Gen =
    let (==) (g1: Gen<_>) (g2: Gen<_>) = apply (fun prms ->
      match g1.Gen.DoApply(prms).Retrieve, g2.Gen.DoApply(prms).Retrieve with
      | (None, None) -> proved.Value.Apply(prms)
      | (Some r1, Some r2) when r1.Equals(r2) -> proved.Value.Apply(prms)
      | _ -> falsified.Value.Apply(prms))

//    let rec notEqual (g1: Gen<_>) (g2: Gen<_>) (s: Shrink<_>) pp =
//      forAll g1 (fun r -> forAll g2 (fun x -> notEqual x r s pp) s pp) s pp

    let (!==) (g1:Gen<_>) (g2: Gen<_>) = apply (fun prms ->
      match g1.Gen.DoApply(prms).Retrieve, g2.Gen.DoApply(prms).Retrieve with
      | (None, None) -> falsified.Value.Apply(prms)
      | (Some r1, Some r2) when r1 = r2 -> falsified.Value.Apply(prms)
      | _ -> proved.Value.Apply(prms))

  open Gen

  let someFailing (gs: Gen<_> seq) = gs |> Seq.map ((==) fail) |> atLeastOne
  let noneFailing (gs: Gen<_> seq) = gs |> Seq.map ((!==) fail) |> all

  let raises<'T, 'U when 'T :> exn> (x: Lazy<'U>) =
    try
      x.Force() |> ignore
      false
    with :? 'T -> true

  let within maximumMs (wrappedProp: Lazy<Prop>) =
    let rec attempt prms endTime =
      let result = wrappedProp.Value.Apply(prms)
      if DateTime.UtcNow.Ticks > endTime then
        let r =
          if PropResult.isFailure result then result
          else { Status = False; Args = []; Labels = Set.empty; Collected = [] }
        { r with Labels = Set.singleton "Timeout" }
      else
        if PropResult.isSuccess result then result
        else attempt prms endTime
    { new Prop() with
      member __.Apply(prms) =
        let now = DateTime.UtcNow
        let endTime = now + TimeSpan(0, 0, 0, 0, maximumMs)
        attempt prms endTime.Ticks }

  let collectF (f: _ -> #Prop) = fun t -> apply (fun prms ->
    let prop = f t
    prop.Apply(prms) |> PropResult.collect t)

  let collect t (prop: Prop) = apply (fun prms -> prop.Apply(prms) |> PropResult.collect t)

  let classify c ifTrue (prop: Prop) : Prop = if c then collect ifTrue prop else collect () prop

  let classifyF c ifTrue ifFalse (prop: Prop) : Prop =
    if c then collect ifTrue prop else collect ifFalse prop

  module Persimmon =

    open Persimmon

    let applyAssertionResult (r: AssertionResult<_>) =
      { new Prop() with
        member __.Apply(_) =
          match r with
          | Passed _ -> { Status = True; Args = []; Labels = Set.empty; Collected = [] }
          | NotPassed (Violated msg) ->
            { Status = False; Args = []; Labels = Set.singleton msg; Collected = [] }
          | NotPassed (Skipped s) ->
            { Status = PropStatus.Skipped s; Args = []; Labels = Set.empty; Collected = [] } }

    let applyTestResult (r: TestResult<_>) =
      { new Prop() with
        member __.Apply(_) =
          match r with
          | Done _ -> { Status = True; Args = []; Labels = Set.empty; Collected = [] }
          | Error(_, [], [], _) ->
            let e = System.Exception("Persimmon.TestResult is error, but exn list and NotPassedCasue are empty.")
            { Status = Exception e; Args = []; Labels = Set.empty; Collected = [] }
          | Error(_, [], x::_, _) ->
            match x with
            | Violated msg -> { Status = False; Args = []; Labels = Set.singleton msg; Collected = [] }
            | Skipped s ->
              { Status = PropStatus.Skipped s; Args = []; Labels = Set.empty; Collected = [] }
          | Error(_, e::_, _, _) ->
            { Status = Exception e; Args = []; Labels = Set.empty; Collected = [] } }

type PropApply =
  | PropApply
  static member Instance(PropApply, p: Prop) = p
  static member Instance(PropApply, f) = PropImpl.apply f
  static member Instance(PropApply, b) = PropImpl.applyBool b
  static member Instance(PropApply, r) = PropImpl.applyResult r
  static member Instance(PropApply, r) = PropImpl.Persimmon.applyAssertionResult r
  static member Instance(PropApply, r) = PropImpl.Persimmon.applyTestResult r
  
module PropTypeClass =

  let inline instance (a:^a) (b:^b) =                                                      
    ((^a or ^b) : (static member Instance: ^a * ^b -> Prop) (a, b))

open System.ComponentModel
open PropImpl

// module like class
[<Sealed; NoComparison; NoEquality>]
type PropModule internal () =

  [<EditorBrowsable(EditorBrowsableState.Never)>]
  member __.forAllNoShrink(g: Gen<_>, pp) = fun f -> apply (fun prms ->
    let gr = g.Gen.DoApply(prms)
    let prms = Gen.Parameters.nextSeed prms
    match gr.Retrieve with
    | None -> undecided.Value.Apply(prms)
    | Some x ->
      let p = secure (fun () -> f x)
      let labels = gr.Labels |> Seq.fold (sprintf "%s,%s") ""
      provedToTrue(p.Apply(prms))
      |> PropResult.addArg { Label = labels; Arg = x; Shrinks = 0; OrigArg = x; PrettyArg = pp x; PrettyOrigArg = pp x })

  member inline this.forAllNoShrink(arb: NonShrinkerArbitrary<_>) = fun f ->
    this.forAllNoShrink(arb.Gen, arb.PrettyPrinter) (f >> PropTypeClass.instance PropApply)

  member __.forAllShrink (g: Gen<_>) (shrink: _ -> _ seq) (f: _ -> _) pp = apply (fun prms ->
    let gr = g.Gen.DoApply(prms)
    let prms = Gen.Parameters.nextSeed prms
    let labels = gr.Labels |> Seq.fold (sprintf "%s,%s") ""

    let result x prms =
      let p = secure (fun () -> f x)
      provedToTrue(p.Apply(prms))

    let getFirstFailure (xs: _ seq) prms =
      // Seq cannot be empty
      assert(not <| Seq.isEmpty xs)
      let results = xs |> Seq.map (fun x -> (x, result x prms))
      match results |> Seq.skipWhile (snd >> PropResult.isFailure >> not) with
      | xs when Seq.isEmpty xs -> Choice1Of2 (Seq.head results)
      | xs -> Choice2Of2 (Seq.head xs)

    let replOrig r0 r1 =
      match r0.Args, r1.Args with
      | (a0 :: _, a1 :: ass) ->
        { r1 with Args = { a1 with OrigArg = a0.OrigArg } :: ass }
      | _ -> r1

    let rec shrinker x (r: PropResult) shrinks orig prms =
      let xs = shrink x |> Seq.filter gr.Sieve
      let res = r |> PropResult.addArg { Label = labels; Arg = x; Shrinks = shrinks; OrigArg = orig; PrettyArg = pp x; PrettyOrigArg = pp orig }
      if Seq.isEmpty xs then res
      else
        match getFirstFailure xs prms with
        | Choice1Of2 (_, _) -> res
        | Choice2Of2(x2, r2) -> shrinker x2 (replOrig r r2) (shrinks + 1) orig (Gen.Parameters.nextSeed prms)

    match gr.Retrieve with
    | None -> undecided.Value.Apply(prms)
    | Some x ->
      let r = result x prms
      let prms = Gen.Parameters.nextSeed prms
      if not <| PropResult.isFailure r then
        r |> PropResult.addArg { Label = labels; Arg = x; Shrinks = 0; OrigArg = x; PrettyArg = pp x; PrettyOrigArg = pp x }
      else shrinker x r 0 x prms)

  member inline this.forAll(arb: Arbitrary<_>) = fun f ->
    this.forAllShrink arb.Gen (Shrink.shrink arb.Shrinker) (f >> PropTypeClass.instance PropApply) arb.PrettyPrinter

  [<EditorBrowsable(EditorBrowsableState.Never)>]
  member __.exists(g: Gen<_>, pp) = fun f -> apply (fun prms ->
    let gr = g.Gen.DoApply(prms)
    let prms = Gen.Parameters.nextSeed prms
    match gr.Retrieve with
    | None -> undecided.Value.Apply(prms)
    | Some x ->
      let p = secure (fun () -> f x)
      let labels = gr.Labels |> Seq.fold (sprintf "%s,%s") ""
      let r =
        p.Apply(prms)
        |> PropResult.addArg { Label = labels; Arg = x; Shrinks = 0; OrigArg = x; PrettyArg = pp x; PrettyOrigArg = pp x }
      match r.Status with
      | True -> { r with Status = Proof }
      | False -> { r with Status = Undecided }
      | _ -> r)

  member inline this.exists (arb: NonShrinkerArbitrary<_>) = fun f ->
    this.exists(arb.Gen, arb.PrettyPrinter) (f >> PropTypeClass.instance PropApply)

  member inline this.exists (arb: Arbitrary<_>) = fun f -> this.exists arb.NonShrinker f

  [<EditorBrowsable(EditorBrowsableState.Never)>]
  member __.collect(t, prop) = PropImpl.collect t prop

  member inline this.collect(t) = fun p -> this.collect(t, PropTypeClass.instance PropApply p)

  [<EditorBrowsable(EditorBrowsableState.Never)>]
  member __.classify(c, ifTrue, prop) = PropImpl.classify c ifTrue prop

  member inline this.classify(c, ifTrue) =
    fun p -> this.classify(c, ifTrue, PropTypeClass.instance PropApply p)

  [<EditorBrowsable(EditorBrowsableState.Never)>]
  member __.classify(c, ifTrue, ifFalse, prop) = PropImpl.classifyF c ifTrue ifFalse prop

  member inline this.classify(c, ifTrue, ifFalse: 'T) =
    fun p -> this.classify(c, ifTrue, ifFalse, PropTypeClass.instance PropApply p)
