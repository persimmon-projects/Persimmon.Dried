namespace Persimmon.Dried.Tests

open System
open Persimmon.Dried
open UseTestNameByReflection

module ArbTest =

  let ``func`` = property {
    apply (Prop.forAll (Arb.func CoArb.int Arb.int, Arb.list Arb.int |> Arb.nonEmpty) (fun f xs ->
      List.map f xs |> List.isEmpty |> not))
  }

  let ``non null`` = property {
    apply (Prop.forAll Arb.string.NonNull ((<>) null))
  }

  let ``nullable`` =
    let nullable = Arb.nullable Arb.int
    let nullOnly = { nullable with Gen = nullable.Gen |> Gen.suchThat (fun x -> not <| x.HasValue) }
    property {
      apply (Prop.forAll nullOnly ((=) (Nullable())))
    }
