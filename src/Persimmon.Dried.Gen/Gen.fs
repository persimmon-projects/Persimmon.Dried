namespace Persimmon.Dried

open System
open FsRandom

type GenParameters = {
  Size: int
  PrngState: PrngState
}

type Gen<'T> =

  abstract member Apply: GenParameters -> 'T

module Gen =

  module Parameters =

    let Default = {
      Size = 100
      PrngState = Utility.createRandomState ()
    }

    let nextSeed prms = { prms with PrngState = prms.PrngState.Next64Bits() |> snd }

  let private gen (f: GenParameters -> 'T) =
    { new Gen<'T> with member  __.Apply(p) = f p }

  let inline apply p (gen: Gen<_>) = gen.Apply(p)

  [<CompiledName("Constant")>]
  let constant x = gen (fun _ -> x)

  let sized (f: int -> Gen<'T>) = gen (fun p -> (f p.Size).Apply(p))

  [<CompiledName("Size")>]
  let size = sized constant

  let resize s (g: Gen<_>) = gen (fun p -> g.Apply({ p with Size = s }))

  let private suchThatOption (f: _ -> bool) g =
    let rp i j g = resize (2 * i + j) g
    let inner k n =
      gen (fun p ->
        if n = 0 then None
        else
          let mutable n = n
          let mutable k = k
          let mutable g = rp k n g
          let mutable p = p
          let mutable x = apply p g
          while n <> 0 && not (f x) do
            n <- n - 1
            k <- k + 1
            g <- rp k n g
            p <- Parameters.nextSeed p
            x <- apply p g
          if n = 0 then None else Some x)
    sized(max 1 >> inner 0)

  let suchThat f (g: Gen<_>) = gen (fun p ->
    let mutable g2 = suchThatOption f g
    let mutable p = p
    let mutable r = apply p g2
    while Option.isNone r do
      g2 <- sized (fun n -> resize (n + 1) (suchThatOption f g))
      p <- Parameters.nextSeed p
      r <- apply p g2
    r.Value
  )

  let map f (g: Gen<_>) = gen (fun p -> g |> apply p |> f)

  let bind (f: 'T -> Gen<'U>) (g: Gen<'T>) =
    gen (fun p -> g |> apply p |> f |> apply (Parameters.nextSeed p))

  let inline filter pred (g: Gen<_>) = suchThat pred g

  let rec retryUntil (p: 'T -> bool) gen =
    bind (fun t -> if p t then constant t |> suchThat p else retryUntil p gen) gen

  [<CompiledName("Sample")>]
  let sample (g: Gen<_>) = g.Apply(Parameters.Default)

  [<CompiledName("Choose")>]
  let choose f = gen (fun p -> Random.next f p.PrngState |> fst)

  [<CompiledName("Elements")>]
  let elements xs =
    Statistics.uniformDiscrete(0, Seq.length xs - 1)
    |> choose
    |> map (fun n -> Seq.nth n xs)

  [<CompiledName("OneOf")>]
  let oneOf gens = elements gens |> bind id

  let option (g: Gen<_>) = oneOf ([ map Some g; constant None ])

  let sequence (gs: Gen<_> list) =
    gen (fun p ->
      (([], p), gs)
      ||> List.fold (fun (rs, p) g -> (apply p g :: rs, Parameters.nextSeed p))
      |> fst
      |> List.rev)

  // frequency function is a port of https://github.com/fsharp/FsCheck/blob/f90b83ee2396d00a21b507ee6a09b72ff62f75f1/src/FsCheck/Gen.fs#L142
  // FsCheck is released under the terms of the Revised BSD License.
  // Copyright (c) 2008-2015 Kurt Schelfthout. All rights reserved.
  [<CompiledName("Frequency")>]
  let frequency xs =
    let rec pick n xs =
      if Seq.isEmpty xs then invalidArg "xs" "Gen.frequency require non-empty list."
      else
        let (k, x), xs = Seq.head xs, Seq.skip 1 xs
        if n <= k then x
        else pick (n - k) xs
    let tot = Seq.sumBy fst xs
    Statistics.uniformDiscrete (1, tot)
    |> choose
    |> bind (fun n -> pick n xs)

  [<CompiledName("Tuple2")>]
  let tuple2 g = bind (fun x -> map (fun y -> (x, y)) g) g
  [<CompiledName("Tuple3")>]
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

  let listOfMaxLength x g =
    sized (fun n ->
      Statistics.uniformDiscrete (0, min x n)
      |> choose
      |> bind (fun k -> listOfLength k g))

  let listOfMinLength x g =
    sized (fun n ->
      Statistics.uniformDiscrete (x, max x n)
      |> choose
      |> bind (fun k -> listOfLength k g))

  [<CompiledName("ArrayOf")>]
  let arrayOf g =
    sized (fun n ->
      Statistics.uniformDiscrete (0, n)
      |> choose
      |> bind (fun k -> arrayOfLength k g))

  [<CompiledName("NonEmptyArrayOf")>]
  let nonEmptyArrayOf g =
    sized (fun n ->
      Statistics.uniformDiscrete (1, max 1 n)
      |> choose
      |> bind (fun k -> arrayOfLength k g))

  [<CompiledName("ArrayOfMaxLength")>]
  let arrayOfMaxLength x g = listOfMaxLength x g |> map List.toArray

  [<CompiledName("ArrayOfMinLength")>]
  let arrayOfMinLength x g = listOfMinLength x g |> map List.toArray

  [<CompiledName("IEnumerableOf")>]
  let seqOf g =
    sized (fun n ->
      Statistics.uniformDiscrete (0, n)
      |> choose
      |> bind (fun k -> seqOfLength k g))

  [<CompiledName("NonEmptyIEnumerableOf")>]
  let nonEmptySeqOf g =
    sized (fun n ->
      Statistics.uniformDiscrete (1, max 1 n)
      |> choose
      |> bind (fun k -> seqOfLength k g))

  [<CompiledName("IEnumerableOfMaxLength")>]
  let seqOfMaxLength x g = listOfMaxLength x g |> map List.toSeq

  [<CompiledName("IEnumerableOfMinLength")>]
  let seqOfMinLength x g = listOfMinLength x g |> map List.toSeq

  let promote (f: _ -> Gen<_>) = gen (fun p a -> (f a).Apply(p))

  module private Random =

    let boolVariant b (s: PrngState) =
      let _, x = s.Next64Bits()
      let _, y = x.Next64Bits()
      if b then y else x

    let chop n = n % 2L

    let even n = n % 2L = 0L

    let chip finished n s = boolVariant (even n) s |> boolVariant finished

    let stop n = n <= 1L

    let rec bigNatVariant n g =
      if stop n then chip true n g
      else bigNatVariant (chop n) (chip false n g)

    let natVariant n g =
      if stop n then chip true n g
      else bigNatVariant n g

    let variantState n (g: PrngState) =
      if n >= 1L then natVariant (n - 1L) (boolVariant false g)
      elif n = 0L then natVariant 0L (boolVariant true g)
      else bigNatVariant -n (boolVariant true g)

  let variant n (g: Gen<_>) =
    gen (fun p -> g.Apply({ p with PrngState = Random.variantState n p.PrngState }))

  let private chooseChar min max = choose (Statistics.uniformDiscrete (min, max)) |> map char

  [<CompiledName("NumChar")>]
  let numChar = chooseChar 48 57
  [<CompiledName("AlphaLowerChar")>]
  let alphaLowerChar = chooseChar 97 122
  [<CompiledName("AlphaUpperChar")>]
  let alphaUpperChar = chooseChar 65 90
  [<CompiledName("alphaChar")>]
  let alphaChar = frequency [ (1, alphaUpperChar); (9, alphaLowerChar) ]
  [<CompiledName("AlphaNumChar")>]
  let alphaNumChar = frequency [ (1, numChar); (9, alphaChar) ]

  [<CompiledName("Identifier")>]
  let identifier =
    alphaLowerChar
    |> bind (fun c ->
      listOf alphaNumChar
      |> map (fun cs -> c :: cs))
    |> suchThat (List.forall (fun c -> Char.IsLetter c || Char.IsDigit c))
    |> map (fun cs -> String(Array.ofList cs))

  let pick n l =
    if n > Seq.length l || n < 0 then invalidArg "n, l" "Gen.pick require non-empty list and positive number"
    else
      gen (fun p ->
        let a = ResizeArray()
        a.AddRange(l)
        let mutable p = p
        while a.Count > n do
          let v = (choose (Statistics.uniformDiscrete (0, a.Count - 1))).Apply(p)
          a.RemoveAt(v) |> ignore
          p <- Parameters.nextSeed p
        a :> _ seq
      )
      |> suchThat (Seq.forall (fun x -> l |> Seq.exists ((=) x)))

  let createGenForwardedToRef<'T> () =
    let refGen = ref (constant Unchecked.defaultof<'T>)
    let fwdGen = gen (fun p -> (!refGen).Apply(p))
    (fwdGen, refGen)

  [<CompiledName("SomeOf")>]
  let someOf l =
    choose (Statistics.uniformDiscrete (0, Seq.length l))
    |> bind (fun x -> pick x l)

  let infinite p (g: Gen<'T>) = Seq.unfold (fun p -> Some(g.Apply(p), Parameters.nextSeed p)) p

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
  let inline (>>=) m f = Gen.bind f m
