namespace Persimmon.Dried

open System
open FsRandom

type GenParameters = {
  Size: int
  PrngState: PrngState
}

[<AbstractClass>]
type internal R<'T>(labels: Set<string>, result: 'T option) =
  
  new(result: 'T option) = R(Set.empty, result)

  member __.Labels = labels
  member __.Result = result

  abstract member Sieve: 'U -> bool

  member this.Retrieve =
    match result with
    | Some(v) as r when this.Sieve(v) -> r
    | f -> f

  member this.Copy(r: Option<'U>, ?l: Set<string>, ?s: 'U -> bool) =
    let l = defaultArg l labels
    let s = defaultArg s this.Sieve
    { new R<'U>(l, r) with
      member __.Sieve(x) =
        match box x with
        | :? 'U as x -> s x
        | _ -> false }

  member this.Map(f: 'T -> 'U) = this.Copy(this.Retrieve |> Option.map f, labels)

  member this.Bind(f: 'T -> R<'U>) =
    match this.Retrieve with
    | None -> this.Copy(None, labels)
    | Some t ->
      let r = f t
      r.Copy(r.Result, labels + r.Labels)

[<AbstractClass>]
type internal IGen<'T>() =
  abstract member SieveCopy: obj -> bool
  default __.SieveCopy(_) = true
  abstract member DoApply: GenParameters -> R<'T>

type Gen<'T> internal (igen: IGen<'T>) =
  
  member internal __.Gen = igen

  member this.SuchThat(f: 'T -> bool) =
    let gen = { new IGen<'T>() with
      member __.DoApply(p) =
        let res = igen.DoApply(p)
        res.Copy(res.Result, s = fun (x: 'T) -> res.Sieve(x) && f x)
      override __.SieveCopy(x) =
        match x with
        | :? 'T as x -> igen.SieveCopy(x)
        | _ -> false
    }
    Gen(gen)

  member __.Apply(p) = igen.DoApply(p).Retrieve

module Gen =

  module Parameters =

    let Default = {
      Size = 100
      PrngState = Utility.createRandomState ()
    }

  let private gen (f: GenParameters -> R<'T>) =
    Gen({ new IGen<'T>() with member  __.DoApply(p) = f p })

  let private r x = { new R<_>(x) with member __.Sieve(_) = true }

  let inline suchThat p (g: Gen<_>) = g.SuchThat(p)
  let map f (g: Gen<_>) = gen (fun p -> g.Gen.DoApply(p).Map(f))
  let bind (f: 'T -> Gen<'U>) (g: Gen<'T>) =
    gen (fun p -> g.Gen.DoApply(p).Bind(fun t -> (f t).Gen.DoApply(p)))
  let filter pred (g: Gen<_>) = suchThat pred g

  let constant x = gen (fun _ -> r (Some x)) //|> suchThat ((=) x)
  let fail<'T> = gen (fun _ -> r (None: 'T option)) |> suchThat (fun (_: 'T) -> false)

  let rec retryUntil (p: 'T -> bool) gen =
    bind (fun t -> if p t then constant t |> suchThat p else retryUntil p gen) gen

  let sample (g: Gen<_>) = g.Gen.DoApply(Parameters.Default).Retrieve

  let label l (g: Gen<_>) =
    let gen = { new IGen<_>() with
      member __.DoApply(p) =
        let r = g.Gen.DoApply(p)
        r.Copy(r.Result, Set.add l r.Labels)
      override __.SieveCopy(x) = g.Gen.SieveCopy(x)
    }
    Gen(gen)

  let choose f = gen (fun p -> r (Random.next f p.PrngState |> fst |> Some))

  let sized (f: int -> Gen<'T>) = gen (fun p -> (f p.Size).Gen.DoApply(p))
  let size = sized constant

  let resize s (g: Gen<_>) = gen (fun p -> g.Gen.DoApply({ p with Size = s }))

  let elements xs =
    Statistics.uniformDiscrete(0, Seq.length xs - 1)
    |> choose
    |> map (fun n -> Seq.nth n xs)

  let oneOf gens = elements gens |> bind id

  let option (g: Gen<_>) = oneOf ([ map Some g; constant None])

  let sequence (gs: Gen<_> list) = 
    gen (fun p ->
      gs
      |> List.fold (fun rs g ->
        g.Gen.DoApply(p).Bind(fun r ->
          rs.Map(fun x -> r :: x))) (r (Some [])))
    |> map List.rev

  // frequency function is a port of https://github.com/fsharp/FsCheck/blob/f90b83ee2396d00a21b507ee6a09b72ff62f75f1/src/FsCheck/Gen.fs#L142
  // FsCheck is released under the terms of the Revised BSD License.
  // Copyright (c) 2008-2015 Kurt Schelfthout. All rights reserved.
  let frequency xs = 
    let rec pick n xs =
      if Seq.isEmpty xs then fail
      else
        let (k, x), xs = Seq.head xs, Seq.skip 1 xs
        if n <= k then x
        else pick (n - k) xs
    let tot = Seq.sumBy fst xs
    Statistics.uniformDiscrete (1, tot)
    |> choose
    |> bind (fun n -> pick n xs) 

  let tuple2 g = bind (fun x -> map (fun y -> (x, y)) g) g
  let tuple3 g = bind (fun x -> bind (fun y -> map (fun z -> (x, y, z)) g) g) g

  let listOfLength n g = List.init n (fun _ -> g) |> sequence
  let arrayOfLength n g = listOfLength n g |> map List.toArray
  let seqOfLength n g = listOfLength n g |> map List.toSeq

  let listOf g =
    sized (fun n ->
      Statistics.uniformDiscrete (0, n)
      |> choose
      |> bind (fun k -> listOfLength k g))

  let nonEmptyListOf g =
    sized (fun n ->
      Statistics.uniformDiscrete (1, max 1 n)
      |> choose
      |> bind (fun k -> listOfLength k g))

  let arrayOf g =
    sized (fun n ->
      Statistics.uniformDiscrete (0, n)
      |> choose
      |> bind (fun k -> arrayOfLength k g))

  let seqOf g =
    sized (fun n ->
      Statistics.uniformDiscrete (0, n)
      |> choose
      |> bind (fun k -> seqOfLength k g))

type GenBuilder internal () =
  member __.Return(x) = Gen.constant x
  member __.ReturnFrom(g: Gen<_>) = g
  member __.Bind(x, f) = Gen.bind f x

[<AutoOpen>]
module GenSyntax =
  let gen = GenBuilder()
  let (<*>) (f:Gen<_ -> _>) (g:Gen<_>) = gen {
    let! f = f
    let! g = g
    return f g
  }
  let (<!>) f a = Gen.constant f <*> a
