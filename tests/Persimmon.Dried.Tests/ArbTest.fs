namespace Persimmon.Dried.Tests

open Persimmon.Dried
open UseTestNameByReflection

module ArbTest =

  let ``func`` = property {
    apply (Prop.forAll (Arb.func CoArbitrary.int Arb.int, Arb.nonEmptyList Arb.int) (fun f xs ->
      List.map f xs |> List.isEmpty |> not))
  }

  let ``func and fail`` =
    let a = { Arb.nonEmptyList Arb.int with Gen = Gen.fail }
    property {
      apply (Prop.forAll (Arb.func CoArbitrary.int Arb.int, a) (fun f xs ->
        List.map f xs |> List.isEmpty |> not) == Prop.undecided.Value)
    }
