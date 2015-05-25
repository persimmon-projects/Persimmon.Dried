namespace Persimmon.Dried

type ToFunc<'T> =
  abstract member ToFunc: ('T -> 'U) -> CFunc<'T, 'U>

module ToFunc =

  let functionMap (g: 'T -> 'U) (h: 'U -> 'T) (f: 'T -> 'V) (func: ToFunc<'U>) =
    FMap(g, h, func.ToFunc(h >> f)) :> CFunc<_, _>

  let xmap f g func =
    { new ToFunc<_> with
      member __.ToFunc(h) = functionMap g f h func
    }

  let unit = { new ToFunc<_> with
    member __.ToFunc(f) = Single(f ()) :> CFunc<_, _> }

  let bool func =
    let g = function | false -> Choice1Of2 () | true -> Choice2Of2 ()
    let h = function | Choice1Of2 _ -> false | Choice2Of2 _ -> true
    { new ToFunc<_> with
      member __.ToFunc(f) = functionMap g h f func
    }

  let option func =
    let g = function | None -> Choice1Of2 () | Some x -> Choice2Of2 x
    let h = function | Choice1Of2 _ -> None | Choice2Of2 x -> Some x
    { new ToFunc<_> with
      member __.ToFunc(f) = functionMap g h f func
    }

