namespace Persimmon.Dried

open System
open System.Runtime.CompilerServices

[<Extension>]
type PropExtensions () =

  [<Extension>]
  static member inline Or(p1: _, p2: Func<_>) = p1 .|. Lazy(p2)
  [<Extension>]
  static member inline And(p1: _, p2: Func<_>) = p1 .&. Lazy(p2)
  [<Extension>]
  static member inline Append(p1: _, p2: Func<_>) = p1 ++ Lazy(p2)
  [<Extension>]
  static member inline When(body: Lazy<_>, pred: _) = pred ==> body
  [<Extension>]
  static member inline Label(prop: _, label: string) = prop |@ label

[<Extension>]
type PropModuleExtensions () =

  [<Extension>]
  static member inline forAllNoShrink(prop: PropModule, g, f: Func<_, _>) =
    prop.forAllNoShrink g (fun t -> f.Invoke(t))
  [<Extension>]
  static member inline forAllNoShrink(prop: PropModule, g1, g2, f: Func<_, _, _>) =
    prop.forAllNoShrink g1 (fun t1 -> prop.forAllNoShrink g2 (fun t2 -> f.Invoke(t1, t2)))
  [<Extension>]
  static member inline forAllNoShrink
    (prop: PropModule, g1, g2, g3: NonShrinkerArbitrary<_>, f: Func<_, _, _, _>) =
    prop.forAllNoShrink g1 (fun t1 -> prop.forAllNoShrink (g2, g3) (fun t2 t3 -> f.Invoke(t1, t2, t3)))
  [<Extension>]
  static member inline forAllNoShrink
    (prop: PropModule, g1, g2, g3, g4: NonShrinkerArbitrary<_>, f: Func<_, _, _, _, _>) =
    prop.forAllNoShrink g1 (fun t1 ->
      prop.forAllNoShrink (g2, g3, g4) (fun t2 t3 t4 -> f.Invoke(t1, t2, t3, t4)))
  [<Extension>]
  static member inline forAllNoShrink
    (prop: PropModule, g1, g2, g3, g4, g5: NonShrinkerArbitrary<_>, f: Func<_, _, _, _, _, _>) =
    prop.forAllNoShrink g1 (fun t1 ->
      prop.forAllNoShrink (g2, g3, g4, g5) (fun t2 t3 t4 t5 -> f.Invoke(t1, t2, t3, t4, t5)))
  [<Extension>]
  static member inline forAllNoShrink
    (prop: PropModule, g1, g2, g3, g4, g5, g6: NonShrinkerArbitrary<_>, f: Func<_, _, _, _, _, _, _>) =
    prop.forAllNoShrink g1 (fun t1 ->
      prop.forAllNoShrink (g2, g3, g4, g5, g6) (fun t2 t3 t4 t5 t6 -> f.Invoke(t1, t2, t3, t4, t5, t6)))
  [<Extension>]
  static member inline forAllNoShrink
    (prop: PropModule, g1, g2, g3, g4, g5, g6, g7: NonShrinkerArbitrary<_>, f: Func<_, _, _, _, _, _, _, _>) =
    prop.forAllNoShrink g1 (fun t1 ->
      prop.forAllNoShrink (g2, g3, g4, g5, g6, g7) (fun t2 t3 t4 t5 t6 t7 ->
        f.Invoke(t1, t2, t3, t4, t5, t6, t7)))
  [<Extension>]
  static member inline forAll(prop: PropModule, a, f: Func<_, _>) =
    prop.forAll a (fun t -> f.Invoke(t))
  [<Extension>]
  static member inline forAll(prop: PropModule, a1, a2, f: Func<_, _, _>) =
    prop.forAll a1 (fun t1 -> prop.forAll a2 (fun t2 -> f.Invoke(t1, t2)))
  [<Extension>]
  static member inline forAll(prop: PropModule, a1, a2, a3: Arbitrary<_>, f: Func<_, _, _, _>) =
    prop.forAll a1 (fun t1 -> prop.forAll (a2, a3) (fun t2 t3 -> f.Invoke(t1, t2, t3)))
  [<Extension>]
  static member inline forAll(prop: PropModule, a1, a2, a3, a4: Arbitrary<_>, f: Func<_, _, _, _, _>) =
    prop.forAll a1 (fun t1 -> prop.forAll (a2, a3, a4) (fun t2 t3 t4 -> f.Invoke(t1, t2, t3, t4)))
  [<Extension>]
  static member inline forAll(prop: PropModule, a1, a2, a3, a4, a5: Arbitrary<_>, f: Func<_, _, _, _, _, _>) =
    prop.forAll a1 (fun t1 ->prop.forAll (a2, a3, a4, a5) (fun t2 t3 t4 t5 ->
      f.Invoke(t1, t2, t3, t4, t5)))
  [<Extension>]
  static member inline forAll
    (prop: PropModule, a1, a2, a3, a4, a5, a6: Arbitrary<_>, f: Func<_, _, _, _, _, _, _>) =
    prop.forAll a1 (fun t1 -> prop.forAll (a2, a3, a4, a5, a6) (fun t2 t3 t4 t5 t6 ->
      f.Invoke(t1, t2, t3, t4, t5, t6)))
  [<Extension>]
  static member inline forAll
    (prop: PropModule, a1, a2, a3, a4, a5, a6, a7: Arbitrary<_>, f: Func<_, _, _, _, _, _, _, _>) =
    prop.forAll a1 (fun t1 -> prop.forAll (a2, a3, a4, a5, a6, a7) (fun t2 t3 t4 t5 t6 t7 ->
      f.Invoke(t1, t2, t3, t4, t5,t6, t7)))
  [<Extension>]
  static member inline collect(prop: PropModule, f: Func<_, #Prop>, t) = prop.collect f t
  [<Extension>]
  static member secure(prop: PropModule, p: Func<_>) = prop.secure (fun () -> p.Invoke())
  [<Extension>]
  static member sizedProp(prop: PropModule, f: Func<_, _>) = prop.sizedProp (fun i -> f.Invoke(i))
