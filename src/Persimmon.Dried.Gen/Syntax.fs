[<AutoOpen>]
module Persimmon.Dried.GenSyntax

let gen = GenBuilder()

let (<*>) (f:Gen<_ -> _>) (g:Gen<_>) = gen {
  let! f = f
  let! g = g
  return f g
}

let (<!>) f a = Gen.constant f <*> a
let inline (>>=) m f = Gen.bind f m

open Persimmon

type ParameterizeBuilder with
  [<CustomOperation("sample")>]
  member inline __.Sample(source, gen: Gen<_>) = seq { yield! source; yield Gen.sample gen }
