namespace Persimmon.Dried

type NonShrinkerArbitrary<'T> = {
  Gen: Gen<'T>
  PrettyPrinter: 'T -> Pretty
}

type Arbitrary<'T> = {
  Gen: Gen<'T>
  Shrinker: Shrink<'T>
  PrettyPrinter: 'T -> Pretty
}
with
  member this.NonShrinker: NonShrinkerArbitrary<'T> = {
    Gen = this.Gen
    PrettyPrinter = this.PrettyPrinter
  }

[<RequireQualifiedAccess>]
module Arb =

  let unit = {
    Gen = Gen.constant ()
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = fun () -> Pretty(fun _ -> "unit")
  }

  let bool = {
    Gen = Gen.elements [ true; false ]
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyAny
  }

  open FsRandom
  open System

  let byte = {
    Gen = Gen.choose (Statistics.uniformDiscrete (int Byte.MinValue, int Byte.MaxValue)) |> Gen.map byte
    Shrinker = Shrink.shrinkByte
    PrettyPrinter = Pretty.prettyAny
  }

  let uint16 = {
    Gen = Gen.choose (Statistics.uniformDiscrete (int UInt16.MinValue, int UInt16.MaxValue)) |> Gen.map uint16
    Shrinker = Shrink.shrinkUInt16
    PrettyPrinter = Pretty.prettyAny
  }

  let sbyte = {
    Gen = Gen.choose (Statistics.uniformDiscrete (int SByte.MinValue, int SByte.MaxValue)) |> Gen.map sbyte
    Shrinker = Shrink.shrinkSbyte
    PrettyPrinter = Pretty.prettyAny
  }

  let int16 = {
    Gen = Gen.choose (Statistics.uniformDiscrete (int Int16.MinValue, int Int16.MaxValue)) |> Gen.map int16
    Shrinker = Shrink.shrinkInt16
    PrettyPrinter = Pretty.prettyAny
  }

  let int = {
    Gen = Gen.choose (Statistics.uniformDiscrete (Int32.MinValue, Int32.MaxValue))
    Shrinker = Shrink.shrinkInt
    PrettyPrinter = Pretty.prettyAny
  }

  let float32 = {
    Gen = gen {
      let! s = Gen.choose (Statistics.uniformDiscrete (0, 1))
      let! e = Gen.choose (Statistics.uniformDiscrete (0, 0xfe))
      let! m = Gen.choose (Statistics.uniformDiscrete (0, 0x7fffff))
      return System.Convert.ToSingle((s <<< 31) ||| (e <<< 23) ||| m)
    }
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyAny
  }

  let list a = {
    Gen = Gen.listOf a.Gen
    Shrinker = Shrink.shrinkList a.Shrinker
    PrettyPrinter = Pretty.prettyList
  }

  let nonEmptyList a = {
    Gen = Gen.nonEmptyListOf a.Gen
    Shrinker = Shrink.shrinkList a.Shrinker
    PrettyPrinter = Pretty.prettyList
  }

  let array a = {
    Gen = Gen.arrayOf a.Gen
    Shrinker = Shrink.shrinkArray a.Shrinker
    PrettyPrinter = Pretty.prettyAny
  }

  let char = {
    Gen = Gen.frequency
      [
        (0xD800 - Operators.int Char.MinValue,
          Gen.choose (Statistics.uniformDiscrete (Operators.int Char.MinValue, 0xD800 - 1)) |> Gen.map char)
        (Operators.int Char.MaxValue - 0xDFFF,
          Gen.choose (Statistics.uniformDiscrete (0xDFFF + 1, Operators.int Char.MaxValue)) |> Gen.map char)
      ]
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyAny
  }

  let string = {
    Gen = (array char).Gen |> Gen.map (fun xs -> String(xs))
    Shrinker = Shrink.shrinkString
    PrettyPrinter = Pretty.prettyString
  }

  let func (c: CoArbitrary<_>) (a: Arbitrary<_>) = {
    Gen = Gen.promote (fun x -> a.Gen |> CoArbitrary.apply x c)
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyAny
  }
