namespace Persimmon.Dried

type CoArbitrary<'T> =
  abstract member Apply : 'T -> (Gen<'U> -> Gen<'U>)

[<RequireQualifiedAccess>]
module CoArb =

  let apply a (c: CoArbitrary<_>) = c.Apply(a)

  let contramap (f: 'U -> 'T) (c: CoArbitrary<'T>) =
    { new CoArbitrary<'U> with
      member __.Apply(a) = c.Apply(f a)
    }

  let unit = { new CoArbitrary<unit> with
    member __.Apply(_) = id
  }

  [<CompiledName("Bool")>]
  let bool = { new CoArbitrary<bool> with
    member __.Apply(b) = Gen.variant <| if b then 1L else 0L
  }

  [<CompiledName("Char")>]
  let char = { new CoArbitrary<char> with
    member __.Apply(c) = Gen.variant (int64 c <<< 1)
  }

  let option c = { new CoArbitrary<_ option> with
    member __.Apply(o) =
      match o with
      | None -> Gen.variant 0L
      | Some x -> Gen.variant -1L << apply x c
  }

  [<CompiledName("Byte")>]
  let byte = { new CoArbitrary<byte> with
    member __.Apply(n) = Gen.variant (int64 n)
  }

  [<CompiledName("UInt16")>]
  let uint16 = { new CoArbitrary<uint16> with
    member __.Apply(n) = Gen.variant (int64 n)
  }

  [<CompiledName("UInt32")>]
  let uint32 = { new CoArbitrary<uint32> with
    member __.Apply(n) = Gen.variant (int64 n)
  }

  [<CompiledName("UInt64")>]
  let uint64 = { new CoArbitrary<uint64> with
    member __.Apply(n) = Gen.variant (int64 n)
  }

  [<CompiledName("Single")>]
  let float32 = { new CoArbitrary<float32> with
    member __.Apply(n) = Gen.variant (int64 n)
  }

  [<CompiledName("Double")>]
  let float = { new CoArbitrary<float> with
    member __.Apply(n) = Gen.variant (int64 n)
  }

  [<CompiledName("SByte")>]
  let sbyte = { new CoArbitrary<sbyte> with
    member __.Apply(n) = Gen.variant (int64 n)
  }

  [<CompiledName("Int16")>]
  let int16 = { new CoArbitrary<int16> with
    member __.Apply(n) = Gen.variant (int64 n)
  }

  [<CompiledName("Int")>]
  let int = { new CoArbitrary<int> with
    member __.Apply(n) = Gen.variant (int64 n)
  }

  [<CompiledName("Int64")>]
  let int64 = { new CoArbitrary<int64> with
    member __.Apply(n) = Gen.variant n
  }

  [<CompiledName("Tuple")>]
  let tuple2 xa yb = { new CoArbitrary<_ * _> with
    member __.Apply((x, y)) = apply x xa << apply y yb
  }

  [<CompiledName("Tuple")>]
  let tuple3 xa yb zc = { new CoArbitrary<_ * _ * _> with
    member __.Apply((x, y, z)) = apply x xa << apply y yb << apply z zc
  }

  let list l = { new CoArbitrary<_ list> with
    member this.Apply(c) =
      match c with
      | [] -> Gen.variant 0L
      | x::xs -> Gen.variant 1L << apply x l << (this.Apply(xs)) }

  [<CompiledName("Array")>]
  let array a = list a |> contramap Array.toList

  [<CompiledName("IEnumerable")>]
  let seq a = list a |> contramap Seq.toList

  [<CompiledName("List")>]
  let resizeArray a : CoArbitrary<ResizeArray<_>> = list a |> contramap Seq.toList

  let set a = list a |> contramap Set.toList

  let map a b = tuple2 a b |> list |> contramap (Map.fold (fun xs k v -> (k, v) :: xs) [])

  let choice l r = { new CoArbitrary<Choice<_, _>> with
    member __.Apply(c) =
      match c with
      | Choice1Of2 x -> Gen.variant 0L << apply x l
      | Choice2Of2 y -> Gen.variant 1L << apply y r }

  [<CompiledName("String")>]
  let string = { new CoArbitrary<string> with
    member __.Apply(s) =
      (array char).Apply(s.ToCharArray())
  }

  open System
  open System.Collections.Generic
  open System.Linq

  [<CompiledName("Dictionary")>]
  let dict a b : CoArbitrary<Dictionary<_, _>> =
    let f = Func<_, _, _>(fun xs (KeyValue(k, v)) -> (k, v) :: xs)
    tuple2 a b
    |> list
    |> contramap (fun d -> d.Aggregate([], f))
