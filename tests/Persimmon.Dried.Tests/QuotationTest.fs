module Persimmon.Dried.Tests.QuotationTest

open System
open Persimmon
open Persimmon.Dried
open UseTestNameByReflection

let ``number is zero`` = Prop.forAll Arb.int ((=) 0)
let ``lazy value`` = lazy Prop.forAll Arb.int (fun _ -> false)
let ``ref value`` = ref <| Prop.forAll Arb.int (fun _ -> false)
let ``call function`` () = Prop.forAll Arb.int (fun _ -> false)

type PropertiesBuilder with
  [<CustomOperation("test")>]
  member __.Test(s: PropertiesState<_>, f) =
    TestCase<_>(None, [], [], fun tc -> async {
      let p = Prop.all s.Properties
      let r = Runner.check s.RunnerParams p
      let pr = p.Apply(Gen.Parameters.Default)
      return
        match assertPred <| f r pr with
        | Passed () -> Done(tc, NonEmptyList.singleton (Passed ()), TimeSpan.Zero)
        | NotPassed cause -> Done(tc, NonEmptyList.singleton (NotPassed cause), TimeSpan.Zero)
    })
  member __.Run(f: unit -> TestCase<_>) =
    try f ()
    with e -> TestCase.makeError None [] [] e

module ReturnValue =

  let ``record variable name and return value`` = property {
    applyReturn ``number is zero``
    test (fun r pr -> not <| Runner.Result.isPassed r && pr.Labels |> Seq.exists ((=) "number is zero"))
  }

  let ``value does not have label`` = property {
    applyReturn (Prop.forAll Arb.int ((=) 0))
    test (fun r pr -> not <| Runner.Result.isPassed r && Set.isEmpty pr.Labels)
  }

  let ``record local variable name`` =
    let ``number is one`` = Prop.forAll Arb.int ((=) 1)
    property {
      applyReturn ``number is one``
      test (fun r pr -> not <| Runner.Result.isPassed r && pr.Labels |> Seq.exists ((=) "number is one"))
    }

  let ``apply lazy value`` = property {
    applyReturn ``lazy value``.Value
    test (fun r pr -> not <| Runner.Result.isPassed r && pr.Labels |> Seq.exists ((=) "lazy value"))
  }

  let ``apply ref value`` = property {
    applyReturn !``ref value``
    test (fun r pr -> not <| Runner.Result.isPassed r && pr.Labels |> Seq.exists ((=) "ref value"))
  }

  let ``local lazy value`` =
    let ``local value`` = lazy Prop.forAll Arb.int (fun _ -> false)
    property {
      applyReturn ``local value``.Value
      test (fun _ pr -> pr.Labels |> Seq.exists ((=) "local value"))
  }

  let ``local ref value`` =
    let ``local value`` = ref <| Prop.forAll Arb.int (fun _ -> false)
    property {
      applyReturn !``local value``
      test (fun _ pr -> pr.Labels |> Seq.exists ((=) "local value"))
  }

  let ``call function and apply`` = property {
    applyReturn (``call function`` ())
    test (fun _ pr -> pr.Labels |> Seq.exists ((=) "call function"))
  }

  let ``call local function`` =
    let ``local function`` () = Prop.forAll Arb.int (fun _ -> false)
    property {
      applyReturn (``local function`` ())
      test (fun _ pr -> pr.Labels |> Seq.exists ((=) "local function"))
  }

module NonReturnValue =

  let ``record variable name`` = property {
    apply ``number is zero``
    test (fun r pr -> not <| Runner.Result.isPassed r && pr.Labels |> Seq.exists ((=) "number is zero"))
  }

  let ``value does not have label`` = property {
    apply (Prop.forAll Arb.int ((=) 0))
    test (fun r pr -> not <| Runner.Result.isPassed r && Set.isEmpty pr.Labels)
  }

  let ``record local variable name`` =
    let ``number is one`` = Prop.forAll Arb.int ((=) 1)
    property {
      apply ``number is one``
      test (fun r pr -> not <| Runner.Result.isPassed r && pr.Labels |> Seq.exists ((=) "number is one"))
    }

  let ``apply lazy value`` = property {
    apply ``lazy value``.Value
    test (fun r pr -> not <| Runner.Result.isPassed r && pr.Labels |> Seq.exists ((=) "lazy value"))
  }

  let ``apply ref value`` = property {
    apply !``ref value``
    test (fun r pr -> not <| Runner.Result.isPassed r && pr.Labels |> Seq.exists ((=) "ref value"))
  }

  let ``local lazy value`` =
    let ``local value`` = lazy Prop.forAll Arb.int (fun _ -> false)
    property {
      apply ``local value``.Value
      test (fun _ pr -> pr.Labels |> Seq.exists ((=) "local value"))
  }

  let ``local ref value`` =
    let ``local value`` = ref <| Prop.forAll Arb.int (fun _ -> false)
    property {
      apply !``local value``
      test (fun _ pr -> pr.Labels |> Seq.exists ((=) "local value"))
  }

  let ``call function and apply`` = property {
    apply (``call function`` ())
    test (fun _ pr -> pr.Labels |> Seq.exists ((=) "call function"))
  }

  let ``call local function`` =
    let ``local function`` () = Prop.forAll Arb.int (fun _ -> false)
    property {
      apply (``local function`` ())
      test (fun _ pr -> pr.Labels |> Seq.exists ((=) "local function"))
  }
  