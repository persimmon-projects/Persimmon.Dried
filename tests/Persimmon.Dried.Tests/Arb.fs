namespace Persimmon.Dried.Tests

open Persimmon.Dried

module Arb =

  let prop =
    let undecidedOrPassed = Prop.forAll Arb.bool (fun b -> b ==> lazy true)
    {
      Gen =
        Gen.frequency [
          (4, Gen.constant Prop.falsified.Value)
          (4, Gen.constant Prop.passed.Value)
          (3, Gen.constant Prop.proved.Value)
          (3, Gen.constant undecidedOrPassed)
          (2, Gen.constant Prop.undecided.Value)
          (1, Gen.constant (Prop.skip ""))
          (1, Gen.constant (Prop.exn null))
        ]
      Shrinker = Shrink.shrinkAny
      PrettyPrinter = Pretty.prettyAny
    }

  let genParameters = {
    Gen =
      Arb.int.Gen
      |> Gen.suchThat (fun x -> x > 0)
      |> Gen.map (fun sz -> { Gen.Parameters.Default with Size = sz })
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyAny
  }

  let exn = {
    Gen = Gen.constant (exn ())
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyExn
  }
