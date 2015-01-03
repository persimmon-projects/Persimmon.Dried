namespace Persimmon.Dried

[<Sealed; NoComparison; NoEquality>]
type PropModule internal () =
  member __.apply f = PropImpl.apply f
  member __.map f g = PropImpl.map f g
  member __.bind f g = PropImpl.bind f g
  member __.combine f g1 g2 = PropImpl.combine f g1 g2
  member __.applyResult r = PropImpl.applyResult r
  member __.provedToTrue r = PropImpl.provedToTrue r
  member __.undecided = PropImpl.undecided
  member __.falsified = PropImpl.falsified
  member __.proved = PropImpl.proved
  member __.passed = PropImpl.passed
  member __.exn e = PropImpl.exn e
  member __.applyBool b = PropImpl.applyBool b
  member __.sizedProp f = PropImpl.sizedProp f
  member __.all ps = PropImpl.all ps
  member __.atLeastOne ps = PropImpl.atLeastOne ps
  member __.secure p = PropImpl.secure p
  member __.forAllNoShrink(arb: NonShrinkerArbitrary<_>) =
    fun f -> PropImpl.forAllNoShrink arb.Gen f arb.PrettyPrinter
  member this.forAllNoShrink(g1, g2) = fun f ->
    this.forAllNoShrink g1 (fun t -> this.forAllNoShrink g2 (f t))
  member this.forAllNoShrink(g1, g2, g3) = fun f ->
    this.forAllNoShrink g1 (fun t -> this.forAllNoShrink (g2, g3) (f t))
  member this.forAllNoShrink(g1, g2, g3, g4) = fun f ->
    this.forAllNoShrink g1 (fun t -> this.forAllNoShrink (g2, g3, g4) (f t))
  member this.forAllNoShrink(g1, g2, g3, g4, g5) = fun f ->
    this.forAllNoShrink g1 (fun t -> this.forAllNoShrink (g2, g3, g4, g5) (f t))
  member this.forAllNoShrink(g1, g2, g3, g4, g5, g6) = fun f ->
    this.forAllNoShrink g1 (fun t -> this.forAllNoShrink (g2, g3, g4, g5, g6) (f t))
  member this.forAllNoShrink(g1, g2, g3, g4, g5, g6, g7) = fun f ->
    this.forAllNoShrink g1 (fun t -> this.forAllNoShrink (g2, g3, g4, g5, g6, g7) (f t))
  member __.forAllShrink g shrink f = PropImpl.forAllShrink g shrink f
  member __.forAll(arb: Arbitrary<_>) = fun f -> PropImpl.forAll arb.Gen f arb.Shrinker arb.PrettyPrinter
  member this.forAll(a1, a2) = fun f ->
    this.forAll a1 (fun t -> this.forAll a2 (f t))
  member this.forAll(a1, a2, a3) = fun f ->
    this.forAll a1 (fun t -> this.forAll (a2, a3) (f t))
  member this.forAll(a1, a2, a3, a4) = fun f ->
    this.forAll a1 (fun t -> this.forAll (a2, a3, a4) (f t))
  member this.forAll(a1, a2, a3, a4, a5) = fun f ->
    this.forAll a1 (fun t -> this.forAll (a2, a3, a4, a5) (f t))
  member this.forAll(a1, a2, a3, a4, a5, a6) = fun f ->
    this.forAll a1 (fun t -> this.forAll (a2, a3, a4, a5, a6) (f t))
  member this.forAll(a1, a2, a3, a4, a5, a6, a7) = fun f ->
    this.forAll a1 (fun t -> this.forAll (a2, a3, a4, a5, a6, a7) (f t))
  member __.someFailing gs = PropImpl.someFailing gs
  member __.noneFailing gs = PropImpl.noneFailing gs
  member __.raises<'T, 'U when 'T :> exn> x = PropImpl.raises<'T, 'U> x
  member __.exists g f = PropImpl.exists g f
  member __.within maximumMs wrappedProp = PropImpl.within maximumMs wrappedProp
  member __.collect(f: _ -> #Prop) = fun t -> PropImpl.collectF f t
  member __.collect(t) = fun prop -> PropImpl.collect t prop
  member __.classify(c, ifTrue) = fun prop -> PropImpl.classify c ifTrue prop
  member __.classify(c, ifTrue, ifFalse) = fun prop -> PropImpl.classifyF c ifTrue ifFalse prop

[<AutoOpen>]
module Syntax =

  let Prop = PropModule()

  let (&&&) (p1: Prop) (p2: Prop) = Prop.combine (&&&) p1 (Prop.secure (fun () -> p2))
  let (|||) (p1: Prop) (p2: Prop) = Prop.combine (|||) p1 (Prop.secure (fun () -> p2))
  let (++) (p1: Prop) (p2: Prop) = Prop.combine (++) p1 (Prop.secure (fun () -> p2))
  let (==>) (p1: Prop) (p2: Prop) =
    p1
    |> Prop.bind (fun r1 ->
      if PropResult.isProved r1 then p2 |> Prop.map (fun r2 -> PropResult.merge r1 r2 r2.Status)
      elif not <| PropResult.isSuccess r1 then Prop.applyResult { r1 with Status = Undecided }
      else p2 |> Prop.map (fun r2 -> Prop.provedToTrue (PropResult.merge r1 r2 r2.Status)))
  let (==) (p1: Prop) (p: Prop) =
    p1
    |> Prop.bind (fun r1 ->
      p |> Prop.map (fun r2 -> PropResult.merge r1 r2 (if r1.Status = r2.Status then True else False)))
  let (@|) s p = PropImpl.label s p
  let (|@) p s = PropImpl.label s p
