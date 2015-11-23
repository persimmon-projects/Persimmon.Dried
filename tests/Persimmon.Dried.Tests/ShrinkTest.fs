namespace Persimmon.Dried.Tests

open Persimmon
open Persimmon.Dried
open UseTestNameByReflection

module ShrinkTest =

  let ``int`` = property {
    apply (Prop.forAll Arb.int (fun n ->
      Shrink.shrink Arb.int.Shrinker n
      |> Seq.forall ((<>) n)))
  }

  // very slowly...
  let ``list`` = property {
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

  let ``non zero int`` = property {
    apply (Prop.forAll Arb.int (fun n ->
      n <> 0 ==> (lazy (shrinkClosure Arb.int.Shrinker n |> Seq.exists ((=) 0)))))
  }

  let ``non empty list`` = property {
    minSize 10
    maxSize 10
    apply (Prop.forAll (Arb.list Arb.int |> Arb.nonEmpty) (fun l ->
        let ls = shrinkClosure (Arb.list Arb.int).Shrinker l
        sprintf "%A" l @| (Seq.exists (List.isEmpty) ls && Seq.exists ((=) [0]) ls)))
  }

  let ``choice shrinks`` = property {
    apply (Prop.forAll (Arb.choice Arb.int Arb.int) (fun e ->
      Shrink.shrink (Shrink.shrinkChoice Shrink.shrinkInt Shrink.shrinkInt) e
      |> Seq.exists ((=) e)
      |> not))
  }

  let ``choice 1 of 2`` = property {
    apply (Prop.forAll Arb.int (fun i ->
      let e = Choice1Of2 i
      Shrink.shrink (Shrink.shrinkChoice Shrink.shrinkInt Shrink.shrinkInt) e
      |> Seq.forall (function | Choice1Of2 _ -> true | _ -> false)))
  }

  let ``choice 2 of 2`` = property {
    apply (Prop.forAll Arb.int (fun i ->
      let e = Choice2Of2 i
      Shrink.shrink (Shrink.shrinkChoice Shrink.shrinkInt Shrink.shrinkInt) e
      |> Seq.forall (function | Choice2Of2 _ -> true | _ -> false)))
  }
