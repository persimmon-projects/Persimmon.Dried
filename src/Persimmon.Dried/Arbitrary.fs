namespace Persimmon.Dried

type NonShrinkerArbitrary<'T> =
  abstract member Gen: Gen<'T>
  abstract member PrettyPrinter: ('T -> Pretty)

type private NonShrinkerArbitraryImpl<'T> = {
  Gen: Gen<'T>
  PrettyPrinter: 'T -> Pretty
}
with
  interface NonShrinkerArbitrary<'T> with
    member this.Gen = this.Gen
    member this.PrettyPrinter = this.PrettyPrinter

type IArbitrary<'T> =
  abstract member Gen: Gen<'T>
  abstract member Shrinker: Shrink<'T>
  abstract member PrettyPrinter: ('T -> Pretty)
  abstract member NonShrinker: NonShrinkerArbitrary<'T>

type Arbitrary<'T> = {
  Gen: Gen<'T>
  Shrinker: Shrink<'T>
  PrettyPrinter: 'T -> Pretty
}
with
  member this.NonShrinker: NonShrinkerArbitrary<'T> =
    {
      NonShrinkerArbitraryImpl.Gen = this.Gen
      PrettyPrinter = this.PrettyPrinter
    }
    :> NonShrinkerArbitrary<'T>
  interface IArbitrary<'T> with
    member this.Gen = this.Gen
    member this.Shrinker = this.Shrinker
    member this.PrettyPrinter = this.PrettyPrinter
    member this.NonShrinker = this.NonShrinker

[<Sealed>]
type AllowNullArbitrary<'T, 'U when 'T : null and 'U :> IArbitrary<'T>>(arb: 'U) =
  member __.NonNull = arb
  member __.Gen =
    Gen.frequency [
      (9, arb.Gen)
      (1, Gen.constant null)
    ]
  member __.Shrinker = arb.Shrinker
  member __.PrettyPrinter = arb.PrettyPrinter
  member __.NonShrinker = arb.NonShrinker
  interface IArbitrary<'T> with
    member this.Gen = this.Gen
    member this.Shrinker = this.Shrinker
    member this.PrettyPrinter = this.PrettyPrinter
    member this.NonShrinker = this.NonShrinker

[<Sealed>]
type CollectionArbitrary<'T, 'U, 'V when 'U :> 'T seq and 'V :> IArbitrary<'U>>(nonEmpty: 'V, collection: Gen<'U>) =
  new (nonEmpty: 'V, empty: 'U) =
    let gen =
      Gen.frequency [
        (9, nonEmpty.Gen)
        (1, Gen.constant empty)
      ]
    CollectionArbitrary(nonEmpty, gen)
  member __.NonEmpty = nonEmpty
  member __.Gen = collection
  member __.Shrinker = nonEmpty.Shrinker
  member __.PrettyPrinter = nonEmpty.PrettyPrinter
  member __.NonShrinker = nonEmpty.NonShrinker
  interface IArbitrary<'U> with
    member this.Gen = this.Gen
    member this.Shrinker = this.Shrinker
    member this.PrettyPrinter = this.PrettyPrinter
    member this.NonShrinker = this.NonShrinker

[<AutoOpen>]
module ArbitrarySyntax =

  type GenBuilder with
    member inline __.Source(arb: IArbitrary<_>) = arb.Gen
    member inline __.Source(arb: NonShrinkerArbitrary<_>) = arb.Gen
    member inline __.Source(gen: Gen<_>) = gen

[<RequireQualifiedAccess>]
module Arb =

  [<CompiledName("NonNull")>]
  let inline nonNull (a: AllowNullArbitrary<_, _>) = a.NonNull

  [<CompiledName("NonEmpty")>]
  let inline nonEmpty (a: CollectionArbitrary<_, _, _>) = a.NonEmpty

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
    Gen = Gen.choose (ruint8)
    Shrinker = Shrink.shrinkByte
    PrettyPrinter = Pretty.prettyAny
  }

  [<CompiledName("UInt16")>]
  let uint16 = {
    Gen = Gen.choose (ruint16)
    Shrinker = Shrink.shrinkUInt16
    PrettyPrinter = Pretty.prettyAny
  }

  [<CompiledName("UInt32")>]
  let uint32 = {
    Gen = Gen.choose (ruint32)
    Shrinker = Shrink.shrinkUInt32
    PrettyPrinter = Pretty.prettyAny
  }

  [<CompiledName("UInt64")>]
  let uint64 = {
    Gen = Gen.choose (ruint64)
    Shrinker = Shrink.shrinkUInt64
    PrettyPrinter = Pretty.prettyAny
  }

  [<CompiledName("SByte")>]
  let sbyte = {
    Gen = Gen.choose rint8
    Shrinker = Shrink.shrinkSbyte
    PrettyPrinter = Pretty.prettyAny
  }

  [<CompiledName("Int16")>]
  let int16 = {
    Gen = Gen.choose rint16
    Shrinker = Shrink.shrinkInt16
    PrettyPrinter = Pretty.prettyAny
  }

  [<CompiledName("Int")>]
  let int = {
    Gen = Gen.choose rint32
    Shrinker = Shrink.shrinkInt
    PrettyPrinter = Pretty.prettyAny
  }

  [<CompiledName("Int64")>]
  let int64 = {
    Gen = Gen.choose rint64
    Shrinker = Shrink.shrinkInt64
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

  let list (a: IArbitrary<_>) =
    CollectionArbitrary(
      {
        Gen = Gen.nonEmptyListOf a.Gen
        Shrinker = Shrink.shrinkList a.Shrinker
        PrettyPrinter = Pretty.prettyList
      },
      Gen.listOf a.Gen
    )

  [<Obsolete("use Arb.list.NonEmpty")>]
  let nonEmptyList (a: IArbitrary<_>) = {
    Gen = Gen.nonEmptyListOf a.Gen
    Shrinker = Shrink.shrinkList a.Shrinker
    PrettyPrinter = Pretty.prettyList
  }

  [<CompiledName("IEnumerable")>]
  let seq (s: IArbitrary<_>) =
    AllowNullArbitrary(
      CollectionArbitrary(
        {
          Gen = Gen.nonEmptySeqOf s.Gen
          Shrinker = Shrink.shrinkSeq s.Shrinker
          PrettyPrinter = Pretty.prettyAny
        },
        Gen.seqOf s.Gen
      )
    )

  [<CompiledName("Array")>]
  let array (a: IArbitrary<_>) =
    AllowNullArbitrary(
      CollectionArbitrary(
        {
          Gen = Gen.nonEmptyArrayOf a.Gen
          Shrinker = Shrink.shrinkArray a.Shrinker
          PrettyPrinter = Pretty.prettyAny
        },
        Gen.arrayOf a.Gen
      )
    )

  let set (s: IArbitrary<_>) =
    CollectionArbitrary(
      {
        Gen = Gen.nonEmptyListOf s.Gen |> Gen.map Set.ofList
        Shrinker = Shrink.shrinkAny
        PrettyPrinter = Pretty.prettyAny
      },
      Gen.listOf s.Gen |> Gen.map Set.ofList
    )

  let map (key: IArbitrary<_>) (value: IArbitrary<_>) =
    CollectionArbitrary(
      {
        Gen =
          Gen.sized (fun n ->
            Statistics.uniformDiscrete (1, max 1 n)
            |> Gen.choose)
          >>= (fun n ->
            Gen.listOfLength n key.Gen
            >>= (fun k ->
              Gen.listOfLength n value.Gen
              |> Gen.map (fun v -> List.zip k v |> Map.ofList)))
        Shrinker = Shrink.shrinkAny
        PrettyPrinter = Pretty.prettyAny
      },
      Map.empty
    )

  open System.Linq
  open System.Collections.Generic

  [<CompiledName("List")>]
  let resizeArray (xs: IArbitrary<_>) =
    let a = list xs
    AllowNullArbitrary(
      CollectionArbitrary(
        {
          Gen = a.NonEmpty.Gen |> Gen.map Enumerable.ToList
          Shrinker = Shrink.shrinkAny
          PrettyPrinter = Pretty.prettyAny
        },
        a.Gen |> Gen.map Enumerable.ToList
      )
    )

  [<CompiledName("ICollection")>]
  let icollection (cs: IArbitrary<_>) =
    let a = resizeArray cs
    AllowNullArbitrary(
      CollectionArbitrary(
        {
          Gen = a.NonNull.NonEmpty.Gen |> Gen.map (fun xs -> xs :> ICollection<_>)
          Shrinker = Shrink.shrinkAny
          PrettyPrinter = Pretty.prettyAny
        },
        a.NonNull.Gen |> Gen.map (fun xs -> xs :> ICollection<_>)
      )
    )

  [<CompiledName("Dictionary")>]
  let dict (key: IArbitrary<_>, value: IArbitrary<_>) =
    let a = map key value
    AllowNullArbitrary(
      CollectionArbitrary(
        {
          Gen = a.NonEmpty.Gen |> Gen.map (fun m -> Dictionary<_,_>(m))
          Shrinker = Shrink.shrinkAny
          PrettyPrinter = Pretty.prettyAny
        },
        a.Gen |> Gen.map (fun m -> Dictionary<_,_>(m))
      )
    )

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
  let string =
    AllowNullArbitrary(
      {
        Gen = (array char).Gen |> Gen.map (fun xs -> String(xs))
        Shrinker = Shrink.shrinkString
        PrettyPrinter = Pretty.prettyString
      }
    )

  [<CompiledName("DateTime")>]
  let datetime fmt = {
    Gen = gen {
      let! l = int64
      let d = DateTime.MinValue
      return DateTime(d.Ticks + l)
    }
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyDateTime fmt
  }

  let func (c: CoArbitrary<_>) (a: IArbitrary<_>) = {
    Gen = Gen.promote (fun x -> CoArb.apply x c a.Gen)
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyAny
  }

  [<CompiledName("Func")>]
  let systemFunc (c, a) =
    let arb = func c a
    {
      Gen = arb.Gen |> Gen.map (fun f -> Func<_, _>(f))
      Shrinker = Shrink.shrinkAny
      PrettyPrinter = Pretty.prettyAny
    }

  [<CompiledName("Guid")>]
  let guid = {
    Gen = gen {
      let! a = int
      let! b = int16
      let! c = int16
      let! d = byte
      let! e = byte
      let! f = byte
      let! g = byte
      let! h = byte
      let! i = byte
      let! j = byte
      let! k = byte
      return Guid(a, b, c, d, e, f, g, h, i, j, k)
    }
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyGuid
  }

  let option (a: IArbitrary<_>) = {
    Gen = Gen.sized(fun n ->
      Gen.frequency [
        (n, Gen.resize (n / 2) a.Gen |> Gen.map Some)
        (1, Gen.constant None)
      ])
    Shrinker = Shrink.shrinkOption a.Shrinker
    PrettyPrinter = Pretty.prettyAny
  }

  let choice (at: IArbitrary<_>) (au: IArbitrary<_>) = {
    Gen = Gen.oneOf [ Gen.map Choice1Of2 at.Gen; Gen.map Choice2Of2 au.Gen ]
    Shrinker = Shrink.shrinkChoice at.Shrinker au.Shrinker
    PrettyPrinter = Pretty.prettyAny
  }

  [<CompiledName("Nullable")>]
  let nullable (a: Arbitrary<_>) = {
    Gen =
      Gen.frequency [
        (9, a.Gen |> Gen.map (fun x -> Nullable(x)))
        (1, Gen.constant (Nullable()))
      ]
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyAny
  }

  // port from FsCheck

(*--------------------------------------------------------------------------*\
**  FsCheck                                                                 **
**  Copyright (c) 2008-2015 Kurt Schelfthout and contributors.              **
**  All rights reserved.                                                    **
**  https://github.com/kurtschelfthout/FsCheck                              **
**                                                                          **
**  This software is released under the terms of the Revised BSD License.   **
**  See the file License.txt for the full text.                             **
\*--------------------------------------------------------------------------*)

  let private fraction (a:int) (b:int) (c:int) =
    double a + double b / (abs (double c) + 1.0)

  [<CompiledName("Double")>]
  let float = {
    Gen =
      Gen.frequency [
        (6, gen {
          let! a = int
          let! b = int
          let! c = int
          return fraction a b c
        })
        (1, Gen.elements [ Double.NaN; Double.NegativeInfinity; Double.PositiveInfinity])
        (1, Gen.elements [ Double.MaxValue; Double.MinValue; Double.Epsilon])]
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyAny
  }
