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

  [<CompiledName("Bool")>]
  let bool = {
    Gen = Gen.elements [ true; false ]
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyAny
  }

  open FsRandom
  open System

  [<CompiledName("Byte")>]
  let byte = {
    Gen = Gen.choose (Statistics.uniformDiscrete (int Byte.MinValue, int Byte.MaxValue)) |> Gen.map byte
    Shrinker = Shrink.shrinkByte
    PrettyPrinter = Pretty.prettyAny
  }

  [<CompiledName("UInt16")>]
  let uint16 = {
    Gen = Gen.choose (Statistics.uniformDiscrete (int UInt16.MinValue, int UInt16.MaxValue)) |> Gen.map uint16
    Shrinker = Shrink.shrinkUInt16
    PrettyPrinter = Pretty.prettyAny
  }

  [<CompiledName("SByte")>]
  let sbyte = {
    Gen = Gen.choose (Statistics.uniformDiscrete (int SByte.MinValue, int SByte.MaxValue)) |> Gen.map sbyte
    Shrinker = Shrink.shrinkSbyte
    PrettyPrinter = Pretty.prettyAny
  }

  [<CompiledName("Int16")>]
  let int16 = {
    Gen = Gen.choose (Statistics.uniformDiscrete (int Int16.MinValue, int Int16.MaxValue)) |> Gen.map int16
    Shrinker = Shrink.shrinkInt16
    PrettyPrinter = Pretty.prettyAny
  }

  [<CompiledName("Int")>]
  let int = {
    Gen = Gen.choose (Statistics.uniformDiscrete (Int32.MinValue, Int32.MaxValue))
    Shrinker = Shrink.shrinkInt
    PrettyPrinter = Pretty.prettyAny
  }

  [<CompiledName("Single")>]
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

  [<CompiledName("IEnumerable")>]
  let seq s = {
    Gen = Gen.seqOf s.Gen
    Shrinker = Shrink.shrinkSeq s.Shrinker
    PrettyPrinter = Pretty.prettyAny
  }

  [<CompiledName("Array")>]
  let array a = {
    Gen = Gen.arrayOf a.Gen
    Shrinker = Shrink.shrinkArray a.Shrinker
    PrettyPrinter = Pretty.prettyAny
  }

  [<CompiledName("Char")>]
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

  [<CompiledName("String")>]
  let string = {
    Gen = (array char).Gen |> Gen.map (fun xs -> String(xs))
    Shrinker = Shrink.shrinkString
    PrettyPrinter = Pretty.prettyString
  }

  [<CompiledName("DateTime")>]
  let datetime fmt = {
    Gen = gen {
      //FIXME: use uint64 generator
      let! l = int.Gen |> Gen.map int64
      let d = DateTime.MinValue
      return DateTime(d.Ticks + l)
    }
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyDateTime fmt
  }

  let func (c: CoArbitrary<_>) (a: Arbitrary<_>) = {
    Gen = Gen.promote (fun x -> CoArbitrary.apply x c a.Gen)
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyAny
  }

  [<CompiledName("Guid")>]
  let guid = {
    Gen = gen {
      let! a = int.Gen
      let! b = int16.Gen
      let! c = int16.Gen
      let! d = byte.Gen
      let! e = byte.Gen
      let! f = byte.Gen
      let! g = byte.Gen
      let! h = byte.Gen
      let! i = byte.Gen
      let! j = byte.Gen
      let! k = byte.Gen
      return Guid(a, b, c, d, e, f, g, h, i, j, k)
    }
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyGuid
  }

  let option (a: Arbitrary<_>) = {
    Gen = Gen.sized(fun n ->
      Gen.frequency [
        (n, Gen.resize (n / 2) a.Gen |> Gen.map Some)
        (1, Gen.constant None)
      ])
    Shrinker = Shrink.shrinkOption a.Shrinker
    PrettyPrinter = Pretty.prettyAny
  }

  let choice (at: Arbitrary<_>) (au: Arbitrary<_>) = {
    Gen = Gen.oneOf [ Gen.map Choice1Of2 at.Gen; Gen.map Choice2Of2 au.Gen ]
    Shrinker = Shrink.shrinkChoice at.Shrinker au.Shrinker
    PrettyPrinter = Pretty.prettyAny
  }
