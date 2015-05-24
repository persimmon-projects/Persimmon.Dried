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
        let xml = FsPickler.CreateXml()
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

  let ``serialize and deserialize`` =
    let p s =
      let prms = { Runner.Parameters.Default with PrngState = s }
      Prop.forAll Arb.int (fun i -> i >= 0)
      |> Runner.check prms
      |> toPropResult
      |> Prop.apply
    property {
      apply (Prop.forAll arb (fun s ->
        p (PrngState.toBinary s |> PrngState.ofBinary) == p s))
    }
