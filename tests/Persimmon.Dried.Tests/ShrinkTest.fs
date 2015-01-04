namespace Persimmon.Dried.Tests

open Persimmon
open Persimmon.Dried

module ShrinkTest =

  let ``int`` = property "int" {
    apply (Prop.forAll Arb.int (fun n ->
      Shrink.shrink Arb.int.Shrinker n
      |> Seq.forall ((<>) n)))
  }
