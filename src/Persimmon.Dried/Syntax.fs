namespace Persimmon.Dried

[<AutoOpen>]
module Syntax =

  type PropModule with
    member __.apply f = PropImpl.apply f
    member __.apply r = PropImpl.applyResult r
    member __.apply b = PropImpl.applyBool b
    member __.map f g = PropImpl.map f g
    member __.bind f g = PropImpl.bind f g
    member __.combine f g1 g2 = PropImpl.combine f g1 g2
    member __.provedToTrue r = PropImpl.provedToTrue r
    member __.undecided = PropImpl.undecided
    member __.falsified = PropImpl.falsified
    member __.proved = PropImpl.proved
    member __.passed = PropImpl.passed
    member __.exn e = PropImpl.exn e
    member __.sizedProp f = PropImpl.sizedProp f
    member __.all ps = PropImpl.all ps
    member __.atLeastOne ps = PropImpl.atLeastOne ps
    member __.secure p = PropImpl.secure p
    member inline this.forAllNoShrink(g1, g2) = fun f ->
      this.forAllNoShrink g1 (fun t -> this.forAllNoShrink g2 (f t))
    member inline this.forAllNoShrink(g1, g2, g3) = fun f ->
      this.forAllNoShrink g1 (fun t -> this.forAllNoShrink (g2, g3) (f t))
    member inline this.forAllNoShrink(g1, g2, g3, g4) = fun f ->
      this.forAllNoShrink g1 (fun t -> this.forAllNoShrink (g2, g3, g4) (f t))
    member inline this.forAllNoShrink(g1, g2, g3, g4, g5) = fun f ->
      this.forAllNoShrink g1 (fun t -> this.forAllNoShrink (g2, g3, g4, g5) (f t))
    member inline this.forAllNoShrink(g1, g2, g3, g4, g5, g6) = fun f ->
      this.forAllNoShrink g1 (fun t -> this.forAllNoShrink (g2, g3, g4, g5, g6) (f t))
    member inline this.forAllNoShrink(g1, g2, g3, g4, g5, g6, g7) = fun f ->
      this.forAllNoShrink g1 (fun t -> this.forAllNoShrink (g2, g3, g4, g5, g6, g7) (f t))
    member inline this.forAll(a1, a2) = fun f ->
      this.forAll a1 (fun t -> this.forAll a2 (f t))
    member inline this.forAll(a1, a2, a3) = fun f ->
      this.forAll a1 (fun t -> this.forAll (a2, a3) (f t))
    member inline this.forAll(a1, a2, a3, a4) = fun f ->
      this.forAll a1 (fun t -> this.forAll (a2, a3, a4) (f t))
    member inline this.forAll(a1, a2, a3, a4, a5) = fun f ->
      this.forAll a1 (fun t -> this.forAll (a2, a3, a4, a5) (f t))
    member inline this.forAll(a1, a2, a3, a4, a5, a6) = fun f ->
      this.forAll a1 (fun t -> this.forAll (a2, a3, a4, a5, a6) (f t))
    member inline this.forAll(a1, a2, a3, a4, a5, a6, a7) = fun f ->
      this.forAll a1 (fun t -> this.forAll (a2, a3, a4, a5, a6, a7) (f t))
    member __.someFailing gs = PropImpl.someFailing gs
    member __.noneFailing gs = PropImpl.noneFailing gs
    member __.raises<'T, 'U when 'T :> exn> x = PropImpl.raises<'T, 'U> x
    member __.within maximumMs wrappedProp = PropImpl.within maximumMs wrappedProp
    member __.collect(f: _ -> #Prop) = fun t -> PropImpl.collectF f t
    member __.collect(t) = fun prop -> PropImpl.collect t prop
    member __.classify(c, ifTrue) = fun prop -> PropImpl.classify c ifTrue prop
    member __.classify(c, ifTrue, ifFalse) = fun prop -> PropImpl.classifyF c ifTrue ifFalse prop

  let Prop = PropModule()

  open PropTypeClass
  open PropResult

  let inline (&&&) p1 p2 =
    Prop.combine (&&&) (instance PropApply p1) (Prop.secure (fun () -> instance PropApply p2))
  let inline  (|||) p1 p2 =
    Prop.combine (|||) (instance PropApply p1) (Prop.secure (fun () -> instance PropApply p2))
  let inline (++) p1 p2 =
    Prop.combine (++) (instance PropApply p1) (Prop.secure (fun () -> instance PropApply p2))
  let inline (==>) p1 (Lazy p2) =
    let p2 = instance PropApply p2
    instance PropApply p1
    |> Prop.bind (fun r1 ->
      if PropResult.isProved r1 then p2 |> Prop.map (fun r2 -> PropResult.merge r1 r2 r2.Status)
      elif not <| PropResult.isSuccess r1 then Prop.apply { r1 with Status = Undecided }
      else p2 |> Prop.map (fun r2 -> Prop.provedToTrue (PropResult.merge r1 r2 r2.Status)))
  let inline (==) p1 p =
    instance PropApply p1
    |> Prop.bind (fun r1 ->
      instance PropApply p
      |> Prop.map (fun r2 ->
        PropResult.merge r1 r2 (if r1.Status = r2.Status then True else False)))
  let inline (@|) s p = instance PropApply p |> Prop.map (fun r -> { r with Labels = Set.add s r.Labels })
  let inline (|@) p s = s @| p

  let property name = PropertyBuilder(name)

  module UseTestNameByReflection =
    let property = PropertyBuilder()
