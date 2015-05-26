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
