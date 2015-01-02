namespace Persimmon.Dried

open System

type PropArg<'T> = {
  Label: string
  Arg: 'T
  Shrinks: int
  OrigArg: 'T
}
with
  member this.BoxedTypeParam() = {
    Label = this.Label
    Arg = box this.Arg
    Shrinks = this.Shrinks
    OrigArg = box this.OrigArg
  }

type PropStatus =
  | Proof
  | True
  | False
  | Undecided
  | Exception of exn

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
    | Proof | True -> true
    | _ -> false

  let isFailure r =
    match r.Status with
    | False | Exception _ -> true
    | _ -> false

  let isProved r = r.Status = Proof

  let addArg (a: PropArg<'T>) r = { r with Args = a.BoxedTypeParam() :: r.Args }
  let collect (a: 'T) r = { r with Collected = box a :: r.Collected }

  let merge x y st = {
    Status = st
    Args = List.append x.Args y.Args
    Labels = x.Labels + y.Labels
    Collected = List.append x.Collected y.Collected
  }

[<AutoOpen>]
module PropResultSyntax =

  open PropResult

  let (&&&) (l: PropResult) (r: PropResult) =
    match l.Status, r.Status with
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

  let map f (p: Prop) = { new Prop() with
    member __.Apply(prms) = p.Apply(prms) |> f }

  let label s p = p |> map (fun r -> { r with Labels = Set.add s r.Labels })

  let bind (f: _ -> Prop) (p: Prop) = { new Prop() with
    member __.Apply(prms) = (p.Apply(prms) |> f).Apply(prms) }

  let combine f p1 p2 =
    p1 |> bind (fun r1 ->
    p2 |> bind (fun r2 ->
      { new Prop() with member __.Apply(prms) = f r1 r2 }))

  let apply f = { new Prop() with
    member __.Apply(prms) =
      try
        f prms
      with e ->
        { Status = Exception e; Args = []; Labels = Set.empty; Collected = [] }
  }

  let applyResult r = { new Prop() with
    member __.Apply(prms) = r }

  let provedToTrue r =
    match r.Status with
    | Proof -> { r with Status = True }
    | _ -> r

  let undecided = lazy applyResult { Status = Undecided; Args = []; Labels = Set.empty; Collected = [] }
  let falsified = lazy applyResult { Status = False; Args = []; Labels = Set.empty; Collected = [] }
  let proved = lazy applyResult { Status = Proof; Args = []; Labels = Set.empty; Collected = [] }
  let passed = lazy applyResult { Status = True; Args = []; Labels = Set.empty; Collected = [] }
  let exn(e: exn) = applyResult { Status = Exception e; Args = []; Labels = Set.empty; Collected = [] }

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

  let forAllNoShrink (g1: Gen<_>) (f: _ -> #Prop) = apply (fun prms ->
    let gr = g1.Gen.DoApply(prms)
    match gr.Retrieve with
    | None -> undecided.Value.Apply(prms)
    | Some x ->
      let p = secure (fun () -> f x)
      let labels = gr.Labels |> Seq.fold (sprintf "%s,%s") ""
      provedToTrue(p.Apply(prms))
      |> PropResult.addArg { Label = labels; Arg = x; Shrinks = 0; OrigArg = x })

  let forAllShrink (g: Gen<_>) (shrink: _ -> _ seq) (f: _ -> _) = apply (fun prms ->
    let gr = g.Gen.DoApply(prms)
    let labels = gr.Labels |> Seq.fold (sprintf "%s,%s") ""

    let result x =
      let p = secure (fun () -> f x)
      provedToTrue(p.Apply(prms))

    let getFirstFailure (xs: _ seq) =
      // Seq cannot be empty
      assert(not <| Seq.isEmpty xs)
      let results = xs |> Seq.map (fun x -> (x, result x))
      match results |> Seq.skipWhile (snd >> PropResult.isFailure >> not) with
      | xs when Seq.isEmpty xs -> Choice1Of2 (Seq.head results)
      | xs -> Choice2Of2 (Seq.head xs)

    let replOrig r0 r1 =
      match r0.Args, r1.Args with
      | (a0 :: _, a1 :: ass) ->
        { r1 with Args = { a1 with OrigArg = a0.OrigArg } :: ass }
      | _ -> r1

    let rec shrinker x (r: PropResult) shrinks orig =
      let xs = shrink x |> Seq.filter gr.Sieve
      let res = r |> PropResult.addArg { Label = labels; Arg = x; Shrinks = shrinks; OrigArg = orig }
      if Seq.isEmpty xs then res
      else
        match getFirstFailure xs with
        | Choice1Of2 (x2, r2) -> res
        | Choice2Of2(x2, r2) -> shrinker x2 (replOrig r r2) (shrinks + 1) orig

    match gr.Retrieve with
    | None -> undecided.Value.Apply(prms)
    | Some x ->
      let r = result x
      if not <| PropResult.isFailure r then r |> PropResult.addArg { Label = labels; Arg = x; Shrinks = 0; OrigArg = x }
      else shrinker x r 0 x)

  let forAll (g: Gen<_>) (f: _ -> #Prop) (s1: Shrink<_>) =
    forAllShrink g (Shrink.shrink s1) f

  module Gen =
    let (==) (g1: Gen<_>) (g2: Gen<_>) = apply (fun prms ->
      match g1.Gen.DoApply(prms).Retrieve, g2.Gen.DoApply(prms).Retrieve with
      | (None, None) -> proved.Value.Apply(prms)
      | (Some r1, Some r2) when r1.Equals(r2) -> proved.Value.Apply(prms)
      | _ -> falsified.Value.Apply(prms))

    let rec notEqual (g1: Gen<_>) (g2: Gen<_>) (s: Shrink<_>) =
      forAll g1 (fun r -> forAll g2 (fun x -> notEqual x r s) s) s

    let (!==) (g1:Gen<_>) (g2: Gen<_>) = apply (fun prms ->
      match g1.Gen.DoApply(prms).Retrieve, g2.Gen.DoApply(prms).Retrieve with
      | (None, None) -> falsified.Value.Apply(prms)
      | (Some r1, Some r2) when r1 = r2 -> falsified.Value.Apply(prms)
      | _ -> proved.Value.Apply(prms))

  open Gen

  let someFailing (gs: Gen<_> seq) = gs |> Seq.map ((==) fail) |> atLeastOne
  let noneFailing (gs: Gen<_> seq) = gs |> Seq.map ((==) fail) |> all

  let raises<'T, 'U when 'T :> exn> (x: Lazy<'U>) =
    try
      x.Force() |> ignore
      false
    with :? 'T -> true

  let exists (g: Gen<_>) (f: _ -> #Prop) = apply (fun prms ->
    let gr = g.Gen.DoApply(prms)
    match gr.Retrieve with
    | None -> undecided.Value.Apply(prms)
    | Some x ->
      let p = secure (fun () -> f x)
      let labels = gr.Labels |> Seq.fold (sprintf "%s,%s") ""
      let r = p.Apply(prms) |> PropResult.addArg { Label = labels; Arg = x; Shrinks = 0; OrigArg = x }
      match r.Status with
      | True -> { r with Status = Proof }
      | False -> { r with Status = Undecided }
      | _ -> r)

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
