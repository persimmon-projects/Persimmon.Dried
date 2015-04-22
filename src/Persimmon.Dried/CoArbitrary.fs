namespace Persimmon.Dried

type CoArbitrary<'T> =
  abstract member Apply : 'T -> (Gen<'U> -> Gen<'U>)

[<RequireQualifiedAccess>]
module CoArbitrary =

  let apply a (c: CoArbitrary<_>) = c.Apply(a)

  let unit = { new CoArbitrary<unit> with
    member __.Apply(_) = id
  }

  [<CompiledName("Bool")>]
  let bool = { new CoArbitrary<bool> with
    member __.Apply(b) = if b then Gen.variant -1 else Gen.variant 0
  }

  let option c = { new CoArbitrary<_ option> with
    member __.Apply(o) =
      match o with
      | None -> Gen.variant 0
      | Some x -> Gen.variant -1 << apply x c
  }

  [<CompiledName("Int")>]
  let int = { new CoArbitrary<int> with
    member __.Apply(n) = Gen.variant n
  }
