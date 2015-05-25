namespace Persimmon.Dried

type CFunc<'T, 'U> =
  abstract member Map: ('U -> 'V) -> CFunc<'T, 'V>
  abstract member Abstract: 'U -> ('T -> 'U)
  abstract member Table: ('T * 'U) list

type Pair<'T, 'U, 'V>(a: CFunc<'T, CFunc<'U, 'V>>) =
  interface CFunc<'T * 'U, 'V> with
    member __.Map(f) = Pair(a.Map(fun x -> x.Map(f))) :> CFunc<_, _>
    member __.Abstract(d) = fun (x, y) ->
      a.Map(fun c -> c.Abstract(d) y).Abstract(d) x
    member __.Table =
      a.Table
      |> List.collect (fun (x, q) ->
        q.Table
        |> List.map (fun (y, c) ->
          ((x, y), c)
        )
      )

type Sum<'T, 'U, 'V>(a: CFunc<'T, 'V>, b: CFunc<'U, 'V>) =
  interface CFunc<Choice<'T, 'U>, 'V> with
    member __.Map(f) = Sum(a.Map(f), b.Map(f)) :> CFunc<_, _>
    member __.Abstract(d) = function
    | Choice1Of2 x -> a.Abstract(d) x
    | Choice2Of2 y -> b.Abstract(d) y
    member __.Table =
      let x = a.Table |> List.map (fun (x, c) -> (Choice1Of2 x, c))
      let y = b.Table |> List.map (fun (y, c) -> (Choice2Of2 y, c))
      List.append x y

type Single<'T>(a: 'T) =
  interface CFunc<unit, 'T> with
    member __.Map(f) = Single(f a) :> CFunc<_, _>
    member __.Abstract(_) = fun _ -> a
    member __.Table = [ ((), a) ]

type Nil<'T, 'U>() =
  interface CFunc<'T, 'U> with
    member __.Map(_) = Nil() :> CFunc<_, _>
    member __.Abstract(d) = fun _ -> d
    member __.Table = []

type Table<'T, 'U when 'T : equality>(a: ('T * 'U) list) =
  interface CFunc<'T, 'U> with
    member __.Map(f) = Table(a |> List.map (fun (x, y) -> (x, f y))) :> CFunc<_, _>
    member __.Abstract(d) = fun aa ->
      match a |> List.choose (fun (x, y) -> if aa = x then Some y else None) with
      | x::_ -> x
      | [] -> d
    member __.Table = a

type FMap<'T, 'U, 'V>(a: 'T -> 'U, b: 'U -> 'T, c: CFunc<'U, 'V>) =
  interface CFunc<'T, 'V> with
    member __.Map(f) = FMap(a, b, c.Map(f)) :> CFunc<_, _>
    member __.Abstract(d) = a >> (c.Abstract(d))
    member __.Table =
      c.Table
      |> List.map (fun (x, c) -> (b x, c))

