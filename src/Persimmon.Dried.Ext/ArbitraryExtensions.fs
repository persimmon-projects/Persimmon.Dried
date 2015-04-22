namespace Persimmon.Dried

open System

module Arbitrary =

  [<CompiledName("Create")>]
  let create (gen, shrinker, pretty: Func<_, _>) = {
    Gen = gen
    Shrinker = shrinker
    PrettyPrinter = fun x -> pretty.Invoke(x)
  }

