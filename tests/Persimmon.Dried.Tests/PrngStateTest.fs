namespace Persimmon.Dried.Tests

open Nessos.FsPickler
open Persimmon.Dried
open UseTestNameByReflection

module SeedTest =

  let arb =

    {
      Gen = Gen.oneOf [ Gen.constant Gen.Parameters.Default.PrngState ]
      Shrinker = Shrink.shrinkAny
      PrettyPrinter = fun s -> Pretty(fun _ ->
        let xml = FsPickler.CreateXmlSerializer()
        xml.PickleToString(s))
    }
    
  open Runner

  let toPropResult (x: Runner.Result) =
    let status =
      match x.Status with
      | Exhausted -> Undecided
      | Passed -> True
      | Proved _ -> Proof
      | Skipped s -> PropStatus.Skipped s
      | Failed _ -> False
      | PropException(_, e, _) -> Exception e
    { Status = status; Args = []; Labels = Set.empty; Collected = [] }

  let prop s =
    let prms = { Runner.Parameters.Default with PrngState = s }
    Prop.forAll Arb.int (fun i -> i >= 0)
    |> Runner.check prms
    |> toPropResult
    |> Prop.apply

  let ``serialize and deserialize`` = property {
    apply (Prop.forAll arb (fun s ->
      prop (PrngState.toBinary s |> PrngState.ofBinary) == prop s))
  }

  let ``serialize and deserialize string`` = property {
    apply (Prop.forAll arb (fun s ->
      prop (PrngState.toBinaryString s |> PrngState.ofBinaryString) == prop s))
  }

  let state =
    [
      "00-CE-91-F5-78-05-E9-FD-00-00-01-28-46-73-52-61-6E-"
      "64-6F-6D-2E-52-61-6E-64-6F-6D-4E-75-6D-62-65-72-47-65-6E-65-72-61-74-6F-7"
      "2-2B-50-72-6E-67-53-74-61-74-65-02-CE-91-F5-00-CE-91-F5-05-00-00-00-00-CE"
      "-91-F5-00-00-00-00-00-2D-46-73-52-61-6E-64-6F-6D-2E-52-61-6E-64-6F-6D-4E-"
      "75-6D-62-65-72-47-65-6E-65-72-61-74-6F-72-2B-43-72-65-61-74-65-53-74-61-7"
      "4-65-40-31-31-00-CE-91-F5-00-08-46-73-52-61-6E-64-6F-6D-00-07-31-2E-33-2E"
      "-33-2E-30-00-07-6E-65-75-74-72-61-6C-00-10-35-66-37-32-66-36-65-32-33-63-"
      "36-39-32-30-64-34-00-CE-91-F5-01-00-00-00-10-CE-91-F5-00-CE-91-F5-05-00-0"
      "0-00-00-CE-91-F5-00-00-00-00-00-0E-53-79-73-74-65-6D-2E-54-75-70-6C-65-60"
      "-34-00-CE-91-F5-00-08-6D-73-63-6F-72-6C-69-62-00-07-34-2E-30-2E-30-2E-30-"
      "00-07-6E-65-75-74-72-61-6C-00-10-62-37-37-61-35-63-35-36-31-39-33-34-65-3"
      "0-38-39-00-CE-91-F5-04-00-00-00-10-CE-91-F5-00-CE-91-F5-00-00-00-00-00-0D"
      "-53-79-73-74-65-6D-2E-55-49-6E-74-33-32-04-CE-91-F5-07-00-00-00-00-00-00-"
      "00-04-CE-91-F5-09-00-00-00-00-00-00-00-04-CE-91-F5-09-00-00-00-00-00-00-0"
      "0-04-CE-91-F5-09-00-00-00-00-00-00-00-00-CE-91-F5-02-CE-91-F5-00-CE-91-F5"
      "-00-00-00-00-00-2E-46-73-52-61-6E-64-6F-6D-2E-52-61-6E-64-6F-6D-4E-75-6D-"
      "62-65-72-47-65-6E-65-72-61-74-6F-72-2B-58-6F-72-73-68-69-66-74-50-72-6E-6"
      "7-40-36-30-04-CE-91-F5-03-00-00-00-00-00-00-00-00-CE-91-F5-00-CE-91-F5-C1"
      "-00-04-50-F0-E4-0E-D4-B3-9B-E1-C1-A5-AF-8C-4A"
    ]
    |> String.concat ""

  let ``deserialize old format`` =
    let arb = {
      Gen = Gen.constant state
      Shrinker = Shrink.shrinkString
      PrettyPrinter = Pretty.prettyString
    }
    property {
      apply (Prop.forAll arb (fun s ->
        let p1 = PrngState.ofBinaryString s
        let p2 = p1 |> PrngState.toBinaryString |> PrngState.ofBinaryString
        prop p1 == prop p2
      ))
    }
