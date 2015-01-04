namespace Persimmon.Dried.Tests

open Persimmon
open Persimmon.Dried

module ShrinkTest =

  let ``int`` = property "int" {
    apply (Prop.forAll Arb.int (fun n ->
      Shrink.shrink Arb.int.Shrinker n
      |> Seq.forall ((<>) n)))
  }

  // very slowly...
  let ``list`` = property "list" {
    minSize 10
    maxSize 10
    apply (Prop.forAll (Arb.list Arb.int) (fun l ->
      Shrink.shrink (Arb.list Arb.int).Shrinker l
      |> Seq.forall ((<>) l)))
  }

  let rec shrinkClosure s x =
    let xs = Shrink.shrink s x
    if Seq.isEmpty xs then xs
    else Seq.append xs (xs |> Seq.take 1 |> Seq.collect (shrinkClosure s))

  let ``non zero int`` = property "non zero int" {
    apply (Prop.forAll Arb.int (fun n ->
      n <> 0 ==> (lazy (shrinkClosure Arb.int.Shrinker n |> Seq.exists ((=) 0)))))
  }

  let ``non empty list`` = property "non empty list" {
    minSize 10
    maxSize 10
    apply (Prop.forAll (Arb.nonEmptyList Arb.int) (fun l ->
        let ls = shrinkClosure (Arb.list Arb.int).Shrinker l
        sprintf "%A" l @| (Seq.exists (List.isEmpty) ls && Seq.exists ((=) [0]) ls)))
  }
