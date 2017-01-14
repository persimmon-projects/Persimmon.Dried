namespace Persimmon.Dried.Ext

open System
open System.ComponentModel
open System.Runtime.CompilerServices
open Persimmon.Dried

[<Extension>]
type GenExtensions () =

  [<Extension>]
  static member inline Resize(g, size) = Gen.resize size g

  [<Extension>]
  static member inline Where(g, pred: Func<_, _>) = Gen.suchThat (fun x -> pred.Invoke(x)) g

  [<Extension>]
  static member inline Select(g, f: Func<_, _>) = Gen.map (fun x -> f.Invoke(x)) g

  [<Extension>]
  static member inline SelectMany(g, f: Func<_, _>) = Gen.bind (fun x -> f.Invoke(x)) g

  [<Extension>]
  static member SelectMany(self, f: Func<_, Gen<_>>, g: Func<_, _, _>) =
    self >>= (fun x -> f.Invoke(x) |> Gen.map (fun y -> g.Invoke(x, y)))

  [<Extension>]
  static member inline RetryUntil(g, pred: Func<_, _>) = Gen.retryUntil (fun x -> pred.Invoke(x)) g

  [<Extension>]
  static member inline Variant(g, n) = Gen.variant n g

module Gen =

  let private gen (f: GenParameters -> 'T) = { new Gen<'T> with member  __.Apply(p) = f p }

  [<CompiledName("Sized")>]
  let inline sized (f: Func<int, Gen<'T>>) = Gen.sized (fun i -> f.Invoke(i))

  [<CompiledName("Sequence")>]
  let sequence gs = gs |> Seq.toList |> Gen.sequence |> Gen.map List.toSeq

  [<CompiledName("ArrayOfLength")>]
  let arrayOfLength (g, n) = Gen.arrayOfLength n g

  [<CompiledName("IEnumerableOfLength")>]
  let seqOfLength (g, n) = Gen.seqOfLength n g

  [<CompiledName("Promote")>]
  let promote (f: Func<_, Gen<_>>) =
    gen (fun p a -> (f.Invoke(a)).Apply(p))
    |> Gen.map (fun f -> Func<_, _>(f))

  [<CompiledName("Pick")>]
  let inline pick (n, xs) = Gen.pick n xs

  [<CompiledName("OneOf")>]
  let inline oneOf ([<ParamArrayAttribute>] gens: _ []) = Gen.oneOf gens

  [<CompiledName("Frequency")>]
  let inline frequency ([<ParamArrayAttribute>] xs: _ []) = Gen.frequency xs
