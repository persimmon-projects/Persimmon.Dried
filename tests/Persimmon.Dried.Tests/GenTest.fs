namespace Persimmon.Dried.Tests

open System
open Persimmon
open Persimmon.Dried
open UseTestNameByReflection
open Gen
open PropImpl.Gen
open FsRandom

module GenTest =

  let `` sequence `` =
    let arb = {
      Gen = Gen.listOf (Gen.constant Arb.int.Gen)
      Shrinker = Shrink.shrinkAny
      PrettyPrinter = Pretty.prettyAny
    }
    let la (l: Gen<int> list) = {
      Gen = sequence l
      Shrinker = Shrink.shrinkAny
      PrettyPrinter = Pretty.prettyAny
    }
    property {
      apply (Prop.forAll (arb) (fun l ->
        Prop.forAll (la l) (fun x ->  List.length x = List.length l))
      )
    }

  let `` frequency `` =
    let g =
      frequency [
        (10, Gen.constant 0)
        (5, Gen.constant 1)
      ]
    let arb = {
      Gen = g
      Shrinker = Shrink.shrinkInt
      PrettyPrinter = Pretty.prettyAny
    }
    property {
      apply (Prop.forAll (arb) (fun _ -> true))
    }

  let `` retryUntil `` = property {
    apply (Prop.forAll (Arb.gen Arb.int) (fun g ->
      Gen.retryUntil (fun _ -> true) g == g))
  }

  let ``choose int`` = property {
    apply (Prop.forAll (Arb.DoNotSize.int, Arb.DoNotSize.int) (fun l h ->
      l <= h ==> lazy
        let g = Gen.chooseInt32 l h
        let x = Gen.sample g
        x >= l && x <= h
    ))
  }

  module DoNotSize =

    let ``choose int`` = property {
      apply (Prop.forAll (Arb.int, Arb.int) (fun l h ->
        l <= h ==> lazy
          let arb = {
            Gen = choose (Statistics.uniformDiscrete (l, h))
            Shrinker = Shrink.shrinkInt
            PrettyPrinter = Pretty.prettyAny
          }
          Prop.forAll arb (fun x -> x >= l && x <= h )))
    }

  let `` sized `` = property {
    apply (Prop.forAll (Arb.gen Arb.int) (fun g ->
      Gen.sized (fun _ -> g) == g))
  }

  let ``oneOf n`` = property {
    apply (Prop.forAll (Arb.list Arb.int) (fun l ->
      l <> [] ==> lazy
        let oneOf = Gen.oneOf (List.map Gen.constant l)
        Prop.forAll { Arb.int with Gen = oneOf } (fun x -> List.exists ((=) x) l)))
  }

  let ``oneOf 2`` = property {
    apply (Prop.forAll (Arb.int, Arb.int) (fun n1 n2 ->
      Prop.forAll { Arb.int with Gen = oneOf [ Gen.constant n1; Gen.constant n2] } (fun n -> n = n1 || n = n2)))
  }

  let `` listOf `` = property {
    apply (Prop.sizedProp (fun sz ->
      Prop.forAll (Arb.list Arb.int) (fun l ->
        List.length l <= sz)))
  }

  let `` nonEmptyListOf `` = property {
    apply (Prop.sizedProp (fun sz ->
      Prop.forAll (Arb.list Arb.int |> Arb.nonEmpty) (fun l ->
        List.length l <= max 1 sz)))
  }

  let `` listOfLength `` =
    let arb = {
      Gen = choose (Statistics.uniformDiscrete (0, 100))
      Shrinker = Shrink.shrinkInt
      PrettyPrinter = Pretty.prettyAny
    }
    property {
      apply (Prop.forAll arb (fun n ->
        let arb = {
          Gen = Gen.listOfLength n Arb.int.Gen
          Shrinker = Shrink.shrinkList Arb.int.Shrinker
          PrettyPrinter = Pretty.prettyList
        }
        Prop.forAll arb (List.length >> ((=) n))))
    }

  let `` pick `` = property {
    apply (Prop.forAll (Arb.list Arb.int) (fun l ->
      let arb = {
        Gen = choose (Statistics.uniformDiscrete (-1, 2 * List.length l))
        Shrinker = Shrink.shrinkInt
        PrettyPrinter = Pretty.prettyAny
      }
      Prop.forAll arb (fun n ->
        (n >= 0 && n <= List.length l) ==> lazy
          let arb = {
            Gen = pick n l
            Shrinker = Shrink.shrinkAny
            PrettyPrinter = Pretty.prettyAny
          }
          Prop.forAll arb (fun m -> Seq.length m = n && Seq.forall (fun x -> List.exists ((=) x) l) m)
      )))
  }

  let `` numChar `` =
    let arb = {
      Gen = numChar
      Shrinker = Shrink.shrinkAny
      PrettyPrinter = Pretty.prettyAny
    }
    property {
      apply (Prop.forAll arb (Char.IsDigit))
    }

  let `` alphaUpperChar `` =
    let arb = {
      Gen = alphaUpperChar
      Shrinker = Shrink.shrinkAny
      PrettyPrinter = Pretty.prettyAny
    }
    property {
      apply (Prop.forAll arb (fun c -> Char.IsLetter c && Char.IsUpper c))
    }

  let `` alphaLowerChar `` =
    let arb = {
      Gen = alphaLowerChar
      Shrinker = Shrink.shrinkAny
      PrettyPrinter = Pretty.prettyAny
    }
    property {
      apply (Prop.forAll arb (fun c -> Char.IsLetter c && Char.IsLower c))
    }

  let `` alphaChar `` =
    let arb = {
      Gen = alphaChar
      Shrinker = Shrink.shrinkAny
      PrettyPrinter = Pretty.prettyAny
    }
    property {
      apply (Prop.forAll arb (Char.IsLetter))
    }

  let `` identifier `` =
    let arb = {
      Gen = identifier
      Shrinker = Shrink.shrinkString
      PrettyPrinter = Pretty.prettyString
    }
    property {
      apply (Prop.forAll arb (fun s ->
        s.Length > 0 && Char.IsLetter s.[0] && Char.IsLower s.[0] && String.forall Char.IsLetterOrDigit s))
    }

  let `` option `` = property {
    apply (Prop.forAll Arb.int (fun n ->
      let arb = {
        Gen = option (constant n)
        Shrinker = Shrink.shrinkOption Arb.int.Shrinker
        PrettyPrinter = Pretty.prettyAny
      }
      Prop.forAll arb (function
      | Some m when m = n -> true
      | None -> true
      | _ -> false)))
  }

  let suchThatArb = {
    Gen = Arb.string.NonNull.Gen |> suchThat ((<>) "") |> suchThat (fun s -> not <| s.Contains(","))
    Shrinker = Shrink.shrinkString
    PrettyPrinter = Pretty.prettyString
  }

  let ``suchThat combined`` = property {
    apply (Prop.forAll suchThatArb (fun str ->
      not (str = "" || str.Contains(","))))
  }

  let ``suchThat 1`` = property {
    apply (Prop.forAll suchThatArb ((<>) ""))
  }

  let ``suchThat 2`` = property {
    apply (Prop.forAll suchThatArb (fun str ->
      not (str.Contains(","))))
  }

  let ``monad laws`` = property {
    apply ("first law" @|
      Prop.forAll (Arb.func CoArb.int (Arb.gen Arb.int), Arb.int) (fun f x ->
        (constant x) >>= f == f x))
    apply ("second law" @| Prop.forAll (Arb.gen Arb.int) (fun x -> x >>= constant == x))
    apply ("third law" @|
      Prop.forAll (Arb.func CoArb.int (Arb.gen Arb.int), Arb.func CoArb.int (Arb.gen Arb.int), Arb.gen Arb.int) (fun f g m ->
        (m >>= f) >>= g == (m >>= (fun x -> f x >>= g))))
  }

  let ``return sample`` = test {
    let revrevOrig (xs: int list) = xs |> List.rev |> List.rev = xs
    let! xs = property {
      applyReturn (Prop.forAll (Arb.list Arb.int) revrevOrig)
    }
    do! assertPred (revrevOrig xs)
  }

   type Tree =
     | Node of Tree * Tree
     | Leaf of int

  let ``avoid stack overflow`` =

    let tree, treeRef = createGenForwardedToRef ()
    let treeArb = {
      Gen = tree
      Shrinker = Shrink.shrinkAny
      PrettyPrinter = Pretty.prettyAny
    }

    let leaf = Arb.int.Gen |> Gen.map Leaf
    let node =
      gen {
        let! l = tree
        let! r = tree
        return Node(l, r)
      }

    treeRef := Gen.oneOf [ leaf; node ]

    property {
      apply (Prop.forAll treeArb (fun _ -> true))
    }

  let `` listOfMaxLength `` =
    let arb = {
      Gen = choose (Statistics.uniformDiscrete (0, 100))
      Shrinker = Shrink.shrinkInt
      PrettyPrinter = Pretty.prettyAny
    }
    property {
      apply (Prop.forAll arb (fun n ->
        let arb = {
          Gen = Gen.listOfMaxLength n Arb.int.Gen
          Shrinker = Shrink.shrinkList Arb.int.Shrinker
          PrettyPrinter = Pretty.prettyList
        }
        Prop.forAll arb (fun ls ->
          let len = List.length ls
          n >= len
        )
      ))
    }

  let `` listOfMinLength `` =
    let arb = {
      Gen = choose (Statistics.uniformDiscrete (0, 100))
      Shrinker = Shrink.shrinkInt
      PrettyPrinter = Pretty.prettyAny
    }
    property {
      apply (Prop.forAll arb (fun n ->
        let arb = {
          Gen = Gen.listOfMinLength n Arb.int.Gen
          Shrinker = Shrink.shrinkList Arb.int.Shrinker
          PrettyPrinter = Pretty.prettyList
        }
        Prop.forAll arb (fun ls ->
          let len = List.length ls
          n <= len
        )
      ))
    }
