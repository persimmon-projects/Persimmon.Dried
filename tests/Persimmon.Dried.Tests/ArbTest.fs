namespace Persimmon.Dried.Tests

open Persimmon.Dried
open UseTestNameByReflection

module ArbTest =

  let ``func`` = property {
    apply (Prop.forAll (Arb.func CoArb.int Arb.int, Arb.nonEmptyList Arb.int) (fun f xs ->
      List.map f xs |> List.isEmpty |> not))
  }

  let ``non null`` = property {
    apply (Prop.forAll Arb.string.NonNull ((<>) null))
  }
