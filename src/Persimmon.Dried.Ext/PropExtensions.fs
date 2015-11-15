namespace Persimmon.Dried

open System
open System.Runtime.CompilerServices

[<Extension>]
type PropExtensions () =

  [<Extension>]
  static member Or(p1: Prop, p2: Func<Prop>) = p1 .|. Lazy(p2)
  [<Extension>]
  static member Or(p1: Prop, p2: Func<bool>) = p1 .|. Lazy(p2)
  [<Extension>]
  static member And(p1: Prop, p2: Func<Prop>) = p1 .&. Lazy(p2)
  [<Extension>]
  static member And(p1: Prop, p2: Func<bool>) = p1 .&. Lazy(p2)
  [<Extension>]
  static member Append(p1: Prop, p2: Func<Prop>) = p1 ++ Lazy(p2)
  [<Extension>]
  static member Append(p1: bool, p2: Func<Prop>) = p1 ++ Lazy(p2)
  [<Extension>]
  static member When(body: Lazy<Prop>, pred: bool) = pred ==> body
  [<Extension>]
  static member When(body: Lazy<bool>, pred: bool) = pred ==> body
  [<Extension>]
  static member When(body: Lazy<Prop>, pred: Prop) = pred ==> body
  [<Extension>]
  static member When(body: Lazy<bool>, pred: Prop) = pred ==> body
  [<Extension>]
  static member Label(prop: Prop, label: string) = prop |@ label
  [<Extension>]
  static member Label(prop: bool, label: string) = prop |@ label
  [<Extension>]
  static member Classify(prop: Prop, c, ifTrue) = Prop.classify(c, ifTrue) prop
  [<Extension>]
  static member Classify(prop: bool, c, ifTrue) = Prop.classify(c, ifTrue) prop
  [<Extension>]
  static member Collect(prop, t) = Prop.collect(t, prop)
  [<Extension>]
  static member Run(prop) = Runner.run "" Runner.Parameters.Default prop
  [<Extension>]
  static member Run(prop, config: Configuration) =
    Runner.run config.Name config.Parameter prop
  [<Extension>]
  static member Skip(_: Prop, reason) = Prop.skip reason

[<Extension>]
type PropModuleExtensions () =

  [<Extension>]
  static member ForAllNoShrink(prop: PropModule, g, f: Func<_, bool>) =
    prop.forAllNoShrink g (fun t -> f.Invoke(t))
  [<Extension>]
  static member ForAllNoShrink(prop: PropModule, g1, g2, f: Func<_, _, bool>) =
    prop.forAllNoShrink g1 (fun t1 -> prop.forAllNoShrink g2 (fun t2 -> f.Invoke(t1, t2)))
  [<Extension>]
  static member ForAllNoShrink
    (prop: PropModule, g1, g2, g3: NonShrinkerArbitrary<_>, f: Func<_, _, _, bool>) =
    prop.forAllNoShrink g1 (fun t1 -> prop.forAllNoShrink (g2, g3) (fun t2 t3 -> f.Invoke(t1, t2, t3)))
  [<Extension>]
  static member ForAllNoShrink
    (prop: PropModule, g1, g2, g3, g4: NonShrinkerArbitrary<_>, f: Func<_, _, _, _, bool>) =
    prop.forAllNoShrink g1 (fun t1 ->
      prop.forAllNoShrink (g2, g3, g4) (fun t2 t3 t4 -> f.Invoke(t1, t2, t3, t4)))
  [<Extension>]
  static member ForAllNoShrink
    (prop: PropModule, g1, g2, g3, g4, g5: NonShrinkerArbitrary<_>, f: Func<_, _, _, _, _, bool>) =
    prop.forAllNoShrink g1 (fun t1 ->
      prop.forAllNoShrink (g2, g3, g4, g5) (fun t2 t3 t4 t5 -> f.Invoke(t1, t2, t3, t4, t5)))
  [<Extension>]
  static member ForAllNoShrink
    (prop: PropModule, g1, g2, g3, g4, g5, g6: NonShrinkerArbitrary<_>, f: Func<_, _, _, _, _, _, bool>) =
    prop.forAllNoShrink g1 (fun t1 ->
      prop.forAllNoShrink (g2, g3, g4, g5, g6) (fun t2 t3 t4 t5 t6 -> f.Invoke(t1, t2, t3, t4, t5, t6)))
  [<Extension>]
  static member ForAllNoShrink
    (prop: PropModule, g1, g2, g3, g4, g5, g6, g7: NonShrinkerArbitrary<_>, f: Func<_, _, _, _, _, _, _, bool>) =
    prop.forAllNoShrink g1 (fun t1 ->
      prop.forAllNoShrink (g2, g3, g4, g5, g6, g7) (fun t2 t3 t4 t5 t6 t7 ->
        f.Invoke(t1, t2, t3, t4, t5, t6, t7)))
  [<Extension>]
  static member ForAll(prop: PropModule, a, f: Func<_, bool>) =
    prop.forAll a (fun t -> f.Invoke(t))
  [<Extension>]
  static member ForAll(prop: PropModule, a1, a2, f: Func<_, _, bool>) =
    prop.forAll a1 (fun t1 -> prop.forAll a2 (fun t2 -> f.Invoke(t1, t2)))
  [<Extension>]
  static member ForAll(prop: PropModule, a1, a2, a3: IArbitrary<_>, f: Func<_, _, _, bool>) =
    prop.forAll a1 (fun t1 -> prop.forAll (a2, a3) (fun t2 t3 -> f.Invoke(t1, t2, t3)))
  [<Extension>]
  static member ForAll(prop: PropModule, a1, a2, a3, a4: IArbitrary<_>, f: Func<_, _, _, _, bool>) =
    prop.forAll a1 (fun t1 -> prop.forAll (a2, a3, a4) (fun t2 t3 t4 -> f.Invoke(t1, t2, t3, t4)))
  [<Extension>]
  static member ForAll(prop: PropModule, a1, a2, a3, a4, a5: IArbitrary<_>, f: Func<_, _, _, _, _, bool>) =
    prop.forAll a1 (fun t1 ->prop.forAll (a2, a3, a4, a5) (fun t2 t3 t4 t5 ->
      f.Invoke(t1, t2, t3, t4, t5)))
  [<Extension>]
  static member ForAll
    (prop: PropModule, a1, a2, a3, a4, a5, a6: IArbitrary<_>, f: Func<_, _, _, _, _, _, bool>) =
    prop.forAll a1 (fun t1 -> prop.forAll (a2, a3, a4, a5, a6) (fun t2 t3 t4 t5 t6 ->
      f.Invoke(t1, t2, t3, t4, t5, t6)))
  [<Extension>]
  static member ForAll
    (prop: PropModule, a1, a2, a3, a4, a5, a6, a7: IArbitrary<_>, f: Func<_, _, _, _, _, _, _, bool>) =
    prop.forAll a1 (fun t1 -> prop.forAll (a2, a3, a4, a5, a6, a7) (fun t2 t3 t4 t5 t6 t7 ->
      f.Invoke(t1, t2, t3, t4, t5,t6, t7)))
  [<Extension>]
  static member Collect(prop: PropModule, f: Func<_, #Prop>, t: Prop) = prop.collect f t
  [<Extension>]
  static member Collect(prop: PropModule, f: Func<_, #Prop>, t: bool) = prop.collect f t
  [<Extension>]
  static member Secure(prop: PropModule, p: Func<_>) = prop.secure (fun () -> p.Invoke())
  [<Extension>]
  static member SizedProp(prop: PropModule, f: Func<_, _>) = prop.sizedProp (fun i -> f.Invoke(i))
  [<Extension>]
  static member ForAllNoShrink(prop: PropModule, g, f: Func<_, Prop>) =
    prop.forAllNoShrink g (fun t -> f.Invoke(t))
  [<Extension>]
  static member ForAllNoShrink(prop: PropModule, g1, g2, f: Func<_, _, Prop>) =
    prop.forAllNoShrink g1 (fun t1 -> prop.forAllNoShrink g2 (fun t2 -> f.Invoke(t1, t2)))
  [<Extension>]
  static member ForAllNoShrink
    (prop: PropModule, g1, g2, g3: NonShrinkerArbitrary<_>, f: Func<_, _, _, Prop>) =
    prop.forAllNoShrink g1 (fun t1 -> prop.forAllNoShrink (g2, g3) (fun t2 t3 -> f.Invoke(t1, t2, t3)))
  [<Extension>]
  static member ForAllNoShrink
    (prop: PropModule, g1, g2, g3, g4: NonShrinkerArbitrary<_>, f: Func<_, _, _, _, Prop>) =
    prop.forAllNoShrink g1 (fun t1 ->
      prop.forAllNoShrink (g2, g3, g4) (fun t2 t3 t4 -> f.Invoke(t1, t2, t3, t4)))
  [<Extension>]
  static member ForAllNoShrink
    (prop: PropModule, g1, g2, g3, g4, g5: NonShrinkerArbitrary<_>, f: Func<_, _, _, _, _, Prop>) =
    prop.forAllNoShrink g1 (fun t1 ->
      prop.forAllNoShrink (g2, g3, g4, g5) (fun t2 t3 t4 t5 -> f.Invoke(t1, t2, t3, t4, t5)))
  [<Extension>]
  static member ForAllNoShrink
    (prop: PropModule, g1, g2, g3, g4, g5, g6: NonShrinkerArbitrary<_>, f: Func<_, _, _, _, _, _, Prop>) =
    prop.forAllNoShrink g1 (fun t1 ->
      prop.forAllNoShrink (g2, g3, g4, g5, g6) (fun t2 t3 t4 t5 t6 -> f.Invoke(t1, t2, t3, t4, t5, t6)))
  [<Extension>]
  static member ForAllNoShrink
    (prop: PropModule, g1, g2, g3, g4, g5, g6, g7: NonShrinkerArbitrary<_>, f: Func<_, _, _, _, _, _, _, Prop>) =
    prop.forAllNoShrink g1 (fun t1 ->
      prop.forAllNoShrink (g2, g3, g4, g5, g6, g7) (fun t2 t3 t4 t5 t6 t7 ->
        f.Invoke(t1, t2, t3, t4, t5, t6, t7)))
  [<Extension>]
  static member ForAll(prop: PropModule, a, f: Func<_, Prop>) =
    prop.forAll a (fun t -> f.Invoke(t))
  [<Extension>]
  static member ForAll(prop: PropModule, a1, a2, f: Func<_, _, Prop>) =
    prop.forAll a1 (fun t1 -> prop.forAll a2 (fun t2 -> f.Invoke(t1, t2)))
  [<Extension>]
  static member ForAll(prop: PropModule, a1, a2, a3: IArbitrary<_>, f: Func<_, _, _, Prop>) =
    prop.forAll a1 (fun t1 -> prop.forAll (a2, a3) (fun t2 t3 -> f.Invoke(t1, t2, t3)))
  [<Extension>]
  static member ForAll(prop: PropModule, a1, a2, a3, a4: IArbitrary<_>, f: Func<_, _, _, _, Prop>) =
    prop.forAll a1 (fun t1 -> prop.forAll (a2, a3, a4) (fun t2 t3 t4 -> f.Invoke(t1, t2, t3, t4)))
  [<Extension>]
  static member ForAll(prop: PropModule, a1, a2, a3, a4, a5: IArbitrary<_>, f: Func<_, _, _, _, _, Prop>) =
    prop.forAll a1 (fun t1 ->prop.forAll (a2, a3, a4, a5) (fun t2 t3 t4 t5 ->
      f.Invoke(t1, t2, t3, t4, t5)))
  [<Extension>]
  static member ForAll
    (prop: PropModule, a1, a2, a3, a4, a5, a6: IArbitrary<_>, f: Func<_, _, _, _, _, _, Prop>) =
    prop.forAll a1 (fun t1 -> prop.forAll (a2, a3, a4, a5, a6) (fun t2 t3 t4 t5 t6 ->
      f.Invoke(t1, t2, t3, t4, t5, t6)))
  [<Extension>]
  static member ForAll
    (prop: PropModule, a1, a2, a3, a4, a5, a6, a7: IArbitrary<_>, f: Func<_, _, _, _, _, _, _, Prop>) =
    prop.forAll a1 (fun t1 -> prop.forAll (a2, a3, a4, a5, a6, a7) (fun t2 t3 t4 t5 t6 t7 ->
      f.Invoke(t1, t2, t3, t4, t5,t6, t7)))
