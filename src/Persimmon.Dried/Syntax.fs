namespace Persimmon.Dried

[<AutoOpen>]
module Syntax =

  type PropModule with
    member __.apply f = PropImpl.apply f
    member __.apply r = PropImpl.applyResult r
    [<CompiledName("Apply")>]
    member __.apply b = PropImpl.applyBool b
    member __.map f g = PropImpl.map f g
    member __.bind f g = PropImpl.bind f g
    member __.contramap f g = PropImpl.contramap f g
    member __.combine f g1 g2 = PropImpl.combine f g1 g2
    member __.provedToTrue r = PropImpl.provedToTrue r
    [<CompiledName("Undecied")>]
    member __.undecided = PropImpl.undecided
    [<CompiledName("Falsified")>]
    member __.falsified = PropImpl.falsified
    [<CompiledName("Proved")>]
    member __.proved = PropImpl.proved
    [<CompiledName("Passed")>]
    member __.passed = PropImpl.passed
    [<CompiledName("Exn")>]
    member __.exn e = PropImpl.exn e
    [<CompiledName("ExnNull")>]
    member __.exnNull = lazy (PropImpl.exn null)
    [<CompiledName("Skip")>]
    member __.skip s = PropImpl.skip s
    [<CompiledName("Skip")>]
    member __.skipWithoutMessage = lazy (PropImpl.skip "")
    member __.sizedProp f = PropImpl.sizedProp f
    [<CompiledName("All")>]
    member __.all ps = PropImpl.all ps
    [<CompiledName("AtLeastOne")>]
    member __.atLeastOne ps = PropImpl.atLeastOne ps
    member __.secure p = PropImpl.secure p
    member __.appendLabel n p = PropImpl.appendLabel n p
    member inline this.forAllNoShrink(g1, g2) = fun f ->
      (this.forAllNoShrink g1 (f >> this.forAllNoShrink g2))
        .Snoc(Gen.sample g2.Gen)
    member inline this.forAllNoShrink(g1, g2, g3: NonShrinkerArbitrary<_>) = fun f ->
      (this.forAllNoShrink g1 (f >> this.forAllNoShrink (g2, g3)))
        .Snoc(Gen.sample g2.Gen)
        .Snoc(Gen.sample g3.Gen)
        .Map(function (((a, b), c)) -> (a, b, c))
    member inline this.forAllNoShrink(g1, g2, g3, g4) = fun f ->
      (this.forAllNoShrink g1 (f >> this.forAllNoShrink (g2, g3, g4)))
        .Snoc(Gen.sample g2.Gen)
        .Snoc(Gen.sample g3.Gen)
        .Snoc(Gen.sample g4.Gen)
        .Map(function (((a, b), c), d) -> (a, b, c, d))
    member inline this.forAllNoShrink(g1, g2, g3, g4, g5) = fun f ->
      (this.forAllNoShrink g1 (f >> this.forAllNoShrink (g2, g3, g4, g5)))
        .Snoc(Gen.sample g2.Gen)
        .Snoc(Gen.sample g3.Gen)
        .Snoc(Gen.sample g4.Gen)
        .Snoc(Gen.sample g5.Gen)
        .Map(function ((((a, b), c), d), e) -> (a, b, c, d, e))
    member inline this.forAllNoShrink(g1, g2, g3, g4, g5, g6) = fun f ->
      (this.forAllNoShrink g1 (f >> this.forAllNoShrink (g2, g3, g4, g5, g6)))
        .Snoc(Gen.sample g2.Gen)
        .Snoc(Gen.sample g3.Gen)
        .Snoc(Gen.sample g4.Gen)
        .Snoc(Gen.sample g5.Gen)
        .Snoc(Gen.sample g6.Gen)
        .Map(function (((((a, b), c), d), e), f) -> (a, b, c, d, e, f))
    member inline this.forAllNoShrink(g1, g2, g3, g4, g5, g6, g7) = fun f ->
      (this.forAllNoShrink g1 (f >> this.forAllNoShrink (g2, g3, g4, g5, g6, g7)))
        .Snoc(Gen.sample g2.Gen)
        .Snoc(Gen.sample g3.Gen)
        .Snoc(Gen.sample g4.Gen)
        .Snoc(Gen.sample g5.Gen)
        .Snoc(Gen.sample g6.Gen)
        .Snoc(Gen.sample g7.Gen)
        .Map(function ((((((a, b), c), d), e), f), g) -> (a, b, c, d, e, f, g))
    member inline this.forAll(a1, a2) = fun f ->
      (this.forAll a1 (f >> this.forAll a2))
        .Snoc(Gen.sample a2.Gen)
    member inline this.forAll(a1, a2, a3) = fun f ->
      (this.forAll a1 (f >> this.forAll (a2, a3)))
        .Snoc(Gen.sample a2.Gen)
        .Snoc(Gen.sample a3.Gen)
        .Map(function (((a, b), c)) -> (a, b, c))
    member inline this.forAll(a1, a2, a3, a4) = fun f ->
      (this.forAll a1 (f >> this.forAll (a2, a3, a4)))
        .Snoc(Gen.sample a2.Gen)
        .Snoc(Gen.sample a3.Gen)
        .Snoc(Gen.sample a4.Gen)
        .Map(function (((a, b), c), d) -> (a, b, c, d))
    member inline this.forAll(a1, a2, a3, a4, a5) = fun f ->
      (this.forAll a1 (f >> this.forAll (a2, a3, a4, a5)))
        .Snoc(Gen.sample a2.Gen)
        .Snoc(Gen.sample a3.Gen)
        .Snoc(Gen.sample a4.Gen)
        .Snoc(Gen.sample a5.Gen)
        .Map(function ((((a, b), c), d), e) -> (a, b, c, d, e))
    member inline this.forAll(a1, a2, a3, a4, a5, a6) = fun f ->
      (this.forAll a1 (f >> this.forAll (a2, a3, a4, a5, a6)))
        .Snoc(Gen.sample a2.Gen)
        .Snoc(Gen.sample a3.Gen)
        .Snoc(Gen.sample a4.Gen)
        .Snoc(Gen.sample a5.Gen)
        .Snoc(Gen.sample a6.Gen)
        .Map(function (((((a, b), c), d), e), f) -> (a, b, c, d, e, f))
    member inline this.forAll(a1, a2, a3, a4, a5, a6, a7) = fun f ->
      (this.forAll a1 (f >> this.forAll (a2, a3, a4, a5, a6, a7)))
        .Snoc(Gen.sample a2.Gen)
        .Snoc(Gen.sample a3.Gen)
        .Snoc(Gen.sample a4.Gen)
        .Snoc(Gen.sample a5.Gen)
        .Snoc(Gen.sample a6.Gen)
        .Snoc(Gen.sample a7.Gen)
        .Map(function ((((((a, b), c), d), e), f), g) -> (a, b, c, d, e, f, g))
    [<CompiledName("Throws")>]
    member __.raises<'T, 'U when 'T :> exn> x = PropImpl.raises<'T, 'U> x
    member __.within maximumMs wrappedProp = PropImpl.within maximumMs wrappedProp
    member __.collect(f: _ -> #Prop) = fun t -> PropImpl.collectF f t
    member __.generic(p) = Prop<_>((), p)

  let Prop = PropModule()

  open PropTypeClass
  open PropResult

  let inline (.&.) p1 (p2: Lazy<_>) =
    Prop.combine (&&&) (instance PropApply p1) (Prop.secure (fun () -> instance PropApply p2.Value))
  let inline  (.|.) p1 (p2: Lazy<_>) =
    Prop.combine (|||) (instance PropApply p1) (Prop.secure (fun () -> instance PropApply p2.Value))
  let inline (++) p1 (p2: Lazy<_>) =
    Prop.combine (++) (instance PropApply p1) (Prop.secure (fun () -> instance PropApply p2.Value))
  let inline (==>) p1 (p2: Lazy<_>) =
    instance PropApply p1
    |> Prop.bind (fun r1 ->
      if PropResult.isProved r1 then
        instance PropApply p2.Value |> Prop.map (fun r2 -> PropResult.merge r1 r2 r2.Status)
      elif not <| PropResult.isSuccess r1 then Prop.apply { r1 with Status = Undecided }
      else
        instance PropApply p2.Value
        |> Prop.map (fun r2 -> Prop.provedToTrue (PropResult.merge r1 r2 r2.Status)))
  let inline (==) p1 p =
    instance PropApply p1
    |> Prop.bind (fun r1 ->
      instance PropApply p
      |> Prop.map (fun r2 ->
        PropResult.merge r1 r2 (if r1.Status = r2.Status then True else False)))
  let inline (@|) s p = instance PropApply p |> Prop.appendLabel s
  let inline (|@) p s = s @| p

  let property (name: string) = PropertiesBuilder(name)

  let arbitrary = ArbitraryBuilder()

  module UseTestNameByReflection =
    let property = PropertiesBuilder()
