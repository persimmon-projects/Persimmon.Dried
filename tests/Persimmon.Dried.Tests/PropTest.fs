namespace Persimmon.Dried.Tests

open Persimmon
open Persimmon.Dried
open UseTestNameByReflection

module PropTest =

  let ``==> undecided`` = property {
    apply (Prop.forAll Arb.prop (fun p1 ->
      let g = [ Prop.falsified.Value; Prop.undecided.Value ] |> List.map Gen.constant |> Gen.oneOf
      let a = { Arb.prop with Gen = g }
      Prop.forAll a (fun p2 ->
        let p3 = p2 ==> lazy p1
        p3 == Prop.undecided.Value
        .|. lazy (p3 == Prop.exnNull.Value .&. lazy p1 == Prop.exnNull.Value))))
  }

  let ``==> true`` =
    let g1 =
      [ Prop.proved; Prop.falsified; Prop.undecided; Prop.exnNull ]
      |> List.map Gen.constant |> Gen.oneOf
    let g2 =
      [ Prop.passed.Value; Prop.proved.Value ]
      |> List.map Gen.constant |> Gen.oneOf
    let a1 = { Gen = g1; Shrinker = Shrink.shrinkAny; PrettyPrinter = Pretty.prettyAny }
    let a2 = { Arb.prop with Gen = g2 }
    property {
      apply (Prop.forAll (a1, a2) (fun p1 p2 ->
        let p = p2 ==> p1
        p == p1.Value
        .|. lazy ((p2 == Prop.passed.Value)
        .&. lazy (p1.Value == Prop.proved.Value) .&. lazy (p == Prop.passed.Value))))
    }

  let ``==> short circuit`` =
    let rec loopForever =
      lazy (printfn "looping"; loopForever.Value)
    let positiveDomain n =
      match n with
      | n when n > 0 -> true
      | n when (n &&& 1) = 0 -> failwith "exception"
      | _ -> loopForever.Value
    property {
      apply (Prop.forAll Arb.int (fun n ->
        n > 0 ==> lazy (positiveDomain n)))
    }

  let propException (): Prop = failwith "exception"

  let ``==> exception`` = property {
    apply (Prop.passed.Value ==> lazy (propException ()) == Prop.exnNull.Value)
  }

  let ``==> skip`` = property {
    apply (Prop.passed.Value ==> lazy (Prop.skip "==> skip test") == Prop.skipWithoutMessage.Value)
  }

  let ``.&. commutativity`` =
    let g =
      [
        Prop.proved.Value
        Prop.passed.Value
        Prop.falsified.Value
        Prop.undecided.Value
        Prop.exnNull.Value
      ]
      |> List.map Gen.constant |> Gen.oneOf
    let a = { Arb.prop with Gen = g }
    property {
      apply (Prop.forAll (a, a) (fun p1 p2 -> (p1 .&. lazy p2) == (p2 .&. lazy p1)))
    }

  let ``.&. exception`` = property {
    apply (Prop.forAll Arb.nonSkippedProp (fun p ->
      p .&. lazy (propException ()) == Prop.exnNull.Value))
  }

  let ``.&. exception2`` = property {
    apply (Prop.passed.Value .&. lazy (propException ()) == Prop.exnNull.Value)
  }

  let ``.&. skip`` = property {
    apply (Prop.forAll Arb.prop (fun p ->
      p .&. lazy (Prop.skip ".&. skip test") == Prop.skipWithoutMessage.Value))
  }

  let ``.&. skip 2`` = property {
    apply (Prop.passed.Value .&. lazy (Prop.skip ".&. skip test 2") == Prop.skipWithoutMessage.Value)
  }

  let ``.&. identity`` =
    let g =
      [
        Prop.proved.Value
        Prop.passed.Value
        Prop.skipWithoutMessage.Value
        Prop.falsified.Value
        Prop.undecided.Value
        Prop.exnNull.Value
      ]
      |> List.map Gen.constant |> Gen.oneOf
    let a = { Arb.prop with Gen = g }
    property {
      apply (Prop.forAll a (fun p -> (p .&. Prop.proved) == p))
    }

  let ``.&. false`` =
    let g =
      [
        Prop.proved.Value
        Prop.passed.Value
        Prop.falsified.Value
        Prop.undecided.Value
        Prop.exnNull.Value
      ]
      |> List.map Gen.constant |> Gen.oneOf
    let a = { Arb.prop with Gen = g }
    property {
      apply (Prop.forAll a (fun p ->
        let q = p .&. Prop.falsified
        (q == Prop.falsified.Value) .|. lazy ((q == Prop.exnNull.Value) .&. lazy (p == Prop.exnNull.Value))))
    }

  let ``.&. undecided`` =
    let g = [ Prop.proved.Value; Prop.passed.Value; Prop.undecided.Value ] |> List.map Gen.constant |> Gen.oneOf
    let a = { Arb.prop with Gen = g }
    property {
      apply (Prop.forAll a (fun p -> (p .&. Prop.undecided) == Prop.undecided.Value))
    }

  let ``.&. right prio`` = property {
    apply (Prop.forAll (Arb.int, Arb.genParameters) (fun _ prms ->
      let p =
        (Prop.proved.Value |> Prop.map (PropResult.label "RHS"))
        .&. Prop.proved |> Prop.map(PropResult.label "LHS")
      p.Apply(prms).Labels |> Set.exists ((=) "RHS")))
  }

  let ``.|. commutativity`` =
    let g =
      [
        Prop.proved.Value
        Prop.passed.Value
        Prop.skipWithoutMessage.Value
        Prop.falsified.Value
        Prop.undecided.Value
        Prop.exnNull.Value
      ]
      |> List.map Gen.constant |> Gen.oneOf
    let a = { Arb.prop with Gen = g }
    property {
      apply (Prop.forAll (a, a) (fun p1 p2 -> (p1 .|. lazy p2) == (p2 .|. lazy p1)))
    }

  let ``.|. exception`` = property {
    apply (Prop.forAll Arb.nonSkippedProp (fun p ->
      p .|. lazy (propException ()) == Prop.exnNull.Value))
  }

  let ``.|. skip`` = property {
    apply (Prop.forAll Arb.prop (fun p ->
      p .|. lazy (Prop.skip ".|. skip test") == Prop.skipWithoutMessage.Value))
  }

  let ``.|. identity`` =
    let g =
      [
        Prop.proved.Value
        Prop.passed.Value
        Prop.skipWithoutMessage.Value
        Prop.falsified.Value
        Prop.undecided.Value
        Prop.exnNull.Value
      ]
      |> List.map Gen.constant |> Gen.oneOf
    let a = { Arb.prop with Gen = g }
    property {
      apply (Prop.forAll a (fun p -> (p .|. Prop.falsified) == p))
    }

  let ``.|. true`` =
    let g =
      [
        Prop.proved.Value
        Prop.passed.Value
        Prop.falsified.Value
        Prop.undecided.Value
      ]
      |> List.map Gen.constant |> Gen.oneOf
    let a = { Arb.prop with Gen = g }
    property {
      apply (Prop.forAll a (fun p -> (p .|. Prop.falsified) == p))
    }

  let ``++ commutativity`` =
    let g =
      [
        Prop.proved.Value
        Prop.passed.Value
        Prop.skipWithoutMessage.Value
        Prop.falsified.Value
        Prop.undecided.Value
        Prop.exnNull.Value
      ]
      |> List.map Gen.constant |> Gen.oneOf
    let a = { Arb.prop with Gen = g }
    property {
      apply (Prop.forAll (a, a) (fun p1 p2 -> (p1 ++ lazy p2) == (p2 ++ lazy p1)))
    }

  let ``++ exception`` = property {
    apply (Prop.forAll Arb.nonSkippedProp (fun p ->
      p ++ lazy (propException ()) == Prop.exnNull.Value))
  }

  let ``++ skip`` = property {
    apply (Prop.forAll Arb.prop (fun p ->
      p ++ lazy (Prop.skip "++ skip test") == Prop.skipWithoutMessage.Value))
  }

  let ``++ identity 1`` =
    let g =
      [
        Prop.proved.Value
        Prop.passed.Value
        Prop.skipWithoutMessage.Value
        Prop.falsified.Value
        Prop.exnNull.Value
      ]
      |> List.map Gen.constant |> Gen.oneOf
    let a = { Arb.prop with Gen = g }
    property {
      apply (Prop.forAll a (fun p -> (p ++ Prop.proved) == p))
    }

  let ``++ identity 2`` =
    let g =
      [
        Prop.proved.Value
        Prop.passed.Value
        Prop.skipWithoutMessage.Value
        Prop.falsified.Value
        Prop.exnNull.Value
      ]
      |> List.map Gen.constant |> Gen.oneOf
    let a = { Arb.prop with Gen = g }
    property {
      apply (Prop.forAll a (fun p -> (p ++ Prop.undecided) == p))
    }

  let ``++ false`` =
    let g =
      [
        Prop.proved.Value
        Prop.passed.Value
        Prop.falsified.Value
        Prop.undecided.Value
      ]
      |> List.map Gen.constant |> Gen.oneOf
    let a = { Arb.prop with Gen = g }
    property {
    apply (Prop.forAll a (fun p -> p ++ Prop.falsified == Prop.falsified.Value))
  }

  module Result =

    let undecided = property {
      apply (Prop.forAll Arb.genParameters (fun prms ->
        Prop.undecided.Value.Apply(prms).Status = Undecided))
    }

    let falsified = property {
      apply (Prop.forAll Arb.genParameters (fun prms ->
        Prop.falsified.Value.Apply(prms).Status = False))
    }

    let proved = property {
      apply (Prop.forAll Arb.genParameters (fun prms ->
        Prop.proved.Value.Apply(prms).Status = Proof))
    }

    let passed = property {
      apply (Prop.forAll Arb.genParameters (fun prms ->
        Prop.passed.Value.Apply(prms).Status = True))
    }

    let exn = property {
      apply (Prop.forAll (Arb.genParameters, Arb.exn) (fun prms e ->
        (Prop.exn e).Apply(prms).Status = Exception e))
    }

    let skipped = property {
      apply (Prop.forAll Arb.genParameters (fun prms ->
        (Prop.skip "skipped test").Apply(prms).Status = Skipped "skipped test"))
    }

  let all =
    let a =
      Arb.list { Gen = Gen.constant Prop.proved.Value; Shrinker = Shrink.shrinkAny; PrettyPrinter = Pretty.prettyAny }
      |> Arb.nonEmpty
    property {
      apply (Prop.forAll a Prop.all)
    }

  let atLeastOne =
    let a =
      Arb.list { Gen = Gen.constant Prop.proved.Value; Shrinker = Shrink.shrinkAny; PrettyPrinter = Pretty.prettyAny }
      |> Arb.nonEmpty
    property {
      apply (Prop.forAll a Prop.atLeastOne)
    }

  let raises =
    let raiseExn =
      Prop.raises<exn, _> (
        lazy
          let s: string = null
          s.Length)
      |> Prop.apply
    property {
      apply raiseExn
    }

  let sizedProp =
    let g =
      [
        Prop.passed.Value
        Prop.skipWithoutMessage.Value
        Prop.falsified.Value
        Prop.undecided.Value
        Prop.exnNull.Value
      ]
      |> List.map Gen.constant |> Gen.oneOf
    let a = { Arb.prop with Gen = g }
    property {
    apply (Prop.forAll a (fun p -> p == Prop.sizedProp (fun _ -> p)))
  }

  let ``chek some prop`` = property {
    apply (Prop.forAll (Arb.int, Arb.int) (fun a b -> a + b = b + a))
    apply (Prop.forAll (Arb.int, Arb.int) (fun a b -> a * b = b * a))
  }
