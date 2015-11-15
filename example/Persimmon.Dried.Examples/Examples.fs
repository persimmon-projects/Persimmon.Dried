
open System
open System.Threading
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Collections
open System.Collections.Generic
open FsRandom
open Persimmon
open Persimmon.Dried

// https://github.com/fsharp/FsCheck/blob/6c71e953aa2b6152cd117357a2d3430e03f51bfb/examples/FsCheck.Examples/Examples.fs

let prms = { Runner.Parameters.Default with Callback = Runner.createConsoleReporter 100 }
let run (p: #Prop) = Runner.run "" prms p

type TestEnum =
  | First = 0
  | Second = 1
  | Third = 2

let testEnumArb = {
  Gen = Gen.oneOf
    [
      Gen.constant TestEnum.First
      Gen.constant TestEnum.Second
      Gen.constant TestEnum.Third
    ]
  Shrinker = Shrink.shrinkAny
  PrettyPrinter = Pretty.prettyAny
}

let testEnum (e:TestEnum) = e = TestEnum.First
run <| Prop.forAll testEnumArb testEnum

//bug: exception escapes: fixed
let prop_EscapingException x =
    if x = 0 then failwith "nul" else true
    |@ "bla"
run <| Prop.forAll Arb.int prop_EscapingException

//more escaping exceptions?
let somefailingFunction () = failwith "escaped"

let prop_EscapingException2 x =
    x <> 0 ==> lazy (if x = 0 then false else (somefailingFunction ()))
run <| Prop.forAll Arb.int prop_EscapingException2

let prop_LabelBug x =
    if x = 0 then failwith "null" else true
    |@ "bla"
run <| Prop.forAll Arb.int prop_LabelBug

//smart shrinking
[<StructuredFormatDisplay("{Display}")>]
type Smart<'a> = | Smart of int * 'a with
  override x.ToString() = match x with Smart(_, a) -> sprintf "%A" a
  member x.Display = x.ToString()

let smartArb a = {
  Gen = a.Gen |> Gen.map (fun arb -> Smart(0, arb))
  Shrinker = Shrink.apply (fun (Smart(i, x)) ->
    let ys = Seq.zip {0..Int32.MaxValue} (Shrink.shrink a.Shrinker x) |> Seq.map Smart
    let i' = Math.Max(0,i-2)
    let rec interleave left right =
      match (left,right) with
      | ([],rs) -> rs
      | (ls,[]) -> ls
      | (l::ls,r::rs) -> l::r::(interleave ls rs)
    interleave (Seq.take i' ys |> Seq.toList) (Seq.skip i' ys |> Seq.toList) |> List.toSeq
  )
  PrettyPrinter = Pretty.prettyAny
}

let smartShrink (Smart(_, i)) = i < 20
run <| Prop.forAll (smartArb Arb.int) smartShrink

//-------------examples from QuickCheck paper-------------
let prop_RevUnit (x:char) = List.rev [x] = [x]

let inline trivial b = Prop.classify(b, "trivial")

let prop_RevApp (x:string) xs =
  List.rev (x::xs) = List.rev xs @ [x]
  |> trivial (xs = [])
  |> trivial (xs.Length = 1)

let prop_MaxLe (x:float) y = (x <= y) ==> (lazy (max  x y = y))

//----------various examples-------------------------------

//convoluted property, but shows the power of the combinators: it's no problem to return
//functions that return properties.
//Prop.forAll (Arb.bool, Arb.int, Arb.char, Arb.int)
//  (fun b y x z -> if b then (fun q -> y + 1 = z + int q) else (fun q -> q = 10.0)
//  |> Prop.forAll Arb.float32)
//|> run

//arrays
let prop_RevRevArr (xs: int[]) = Array.rev (Array.rev xs) = xs
run <| Prop.forAll (Arb.array Arb.int) prop_RevRevArr

let prop_RevRevArr2 (xs: int[][]) = xs.Rank = 1
run <| Prop.forAll (Arb.array (Arb.array Arb.int)) prop_RevRevArr2

run <| Prop.forAll (Arb.array Arb.int) (fun arr -> Array.rev arr = arr)

type ARecord = {
  XPos : int
  YPos : int
  Name: string
}

let aRecordArb = {
  Gen = gen {
    let! x = Arb.int
    let! y = Arb.int
    let! name = Arb.string
    return { XPos = x; YPos = y; Name = name }
  }
  Shrinker = Shrink.shrinkAny
  PrettyPrinter = Pretty.prettyAny
}

run <| Prop.forAll aRecordArb (fun record -> (record.XPos > 0 && record.YPos > 0) ==> lazy (record.XPos * record.YPos > 0))

let tuple3 = {
  Gen = Gen.tuple3 Arb.int.Gen
  Shrinker = Shrink.shrinkAny
  PrettyPrinter = Pretty.prettyAny
}
run <| Prop.forAll (Arb.int, Arb.int, Arb.int, Arb.int, Arb.int, Arb.int, tuple3)
  (fun a b c d e f (g, h, i) -> a > b && b > c && d > e && f > g && e > f && h > i && a > i)

type ADisc =
  | First of int
  | Second of char
  | Third of ADisc
  | Fourth of ADisc []

let adiscGen, adiscGenRef = Gen.createGenForwardedToRef<ADisc> ()
let aDiscArb =
  let first = Arb.int.Gen |> Gen.map First
  let second = Arb.char.Gen |> Gen.map Second
  let third = adiscGen |> Gen.map Third
  let fourth = Gen.arrayOf adiscGen |> Gen.map Fourth
  adiscGenRef := Gen.oneOf [ first; second; third; fourth ]
  {
    Gen = adiscGen
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyAny
  }

run <| Prop.forAll aDiscArb (fun d ->
  match d with
  | First i -> i = 2
  | Second _ -> true
  | Third _ -> true
  | Fourth _ -> raise <| InvalidOperationException())

//-----------ReflectArbitrary tests------------------------
//a record type containing an array type
type List<'a> = { list : 'a [] }

//a recursive union type containing a record type
type Tree<'a> =
  | Leaf of string
  | Branch of List<Tree<'a>>

let rec xmlSafeTree (x : Tree<string>) =
  match x with
  | Leaf x -> not (x.StartsWith " " && x.EndsWith " ")
  | Branch xs -> Array.forall xmlSafeTree xs.list

let product x y = (x > 0 && y > 0) ==> lazy (x * y > 0)

let revString (x : string) =
  let cs = x.ToCharArray()
  Array.Reverse cs
  new String(cs)

let revRevString x = revString (revString x) = x

let private idempotent f x = let y = f x in f y = y
run <| Prop.forAll Arb.string (idempotent (fun (x : string) -> x.ToUpper()))

//-----property combinators------------------
let inline private withPositiveInteger (p : int -> 'a) =
  fun n -> n <> 0 ==> lazy (p (abs n))

let testProp = withPositiveInteger (fun x -> x > 0 |> Prop.classify(true, "bla") |> Prop.generic)
run <| Prop.forAll Arb.int testProp

let testProp2 =
  withPositiveInteger (fun x ->
    Prop.forAll Arb.int <| withPositiveInteger (fun y -> Prop.apply (x + y > 0) |> Prop.generic))
run <| Prop.forAll Arb.int testProp2

let blah (s:string) = if s = "" then raise (new System.Exception("foo")) else s.Length > 3

let inline private withNonEmptyString p =
  let arb = {
    Gen = Gen.elements [ "A"; "AA"; "AAA" ]
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyAny
  }
  Prop.forAll arb p

run (withNonEmptyString blah)

let prop_Exc =
  let arb (a: IArbitrary<_>) = {
    Gen = Gen.resize 100 a.Gen
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyAny
  }
  Prop.forAll (arb Arb.string) (fun _-> failwith "error"; true)
Runner.run "prop_Exc" prms prop_Exc


//-----------------test reflective shrinking--------
type RecordStuff<'a> = {
  Yes: bool
  Name: 'a
  NogIets: list<int * char>
}

let bigSize = { prms with MinSize = 100; MaxSize = 100 }

let recordStuffArb (a: IArbitrary<_>) = {
  Gen = gen {
    let! yes = Arb.bool
    let! name = a
    let! nogIets =
      Arb.int.Gen
      |> Gen.bind (fun n ->
        Arb.char.Gen
        |> Gen.map (fun c -> (n, c)))
      |> Gen.listOf
    return { Yes = yes; Name = name; NogIets = nogIets }
  }
  Shrinker = Shrink.shrinkAny
  PrettyPrinter = Pretty.prettyAny
}
Runner.run "" bigSize (Prop.forAll (recordStuffArb Arb.string) (fun s -> s.Yes))

type Recursive<'a> = Void | Leaf of 'a | Branch of Recursive<'a> * 'a * Recursive<'a>
let recursiveGen, recursiveGenRef = Gen.createGenForwardedToRef<Recursive<_>> ()
let rec recursiveArb (a: IArbitrary<_>): Arbitrary<_> =
  let v = Gen.constant Void
  let leaf = a.Gen |> Gen.map Leaf
  let branch = gen {
    let! l = recursiveGen
    let! c = a
    let! r = recursiveGen
    return Branch(l, c, r)
  }
  recursiveGenRef := Gen.oneOf [ v; leaf; branch ]
  {
    Gen = recursiveGen
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyAny
  }

Runner.run "" bigSize (Prop.forAll (recursiveArb Arb.string) (fun s ->
  match s with
  | Branch _ -> false
  | _ -> true))

type Simple = Void | Void2 | Void3 | Leaf of int | Leaf2 of string * int * char * float32
let simpleArb =
  let v = Gen.constant Void
  let v2 = Gen.constant Void
  let v3 = Gen.constant Void
  let leaf = Arb.int.Gen |> Gen.map Leaf
  let leaf2 = gen {
    let! s = Arb.string
    let! i = Arb.int
    let! c = Arb.char
    let! f = Arb.float32
    return Leaf2(s, i, c, f)
  }
  {
    Gen = Gen.oneOf [ v; v2; v3; leaf; leaf2 ]
    Shrinker = Shrink.shrinkAny
    PrettyPrinter = Pretty.prettyAny
  }

//should yield a simplified Leaf2
Runner.run "" bigSize (Prop.forAll simpleArb (fun s -> match s with Leaf2 _ -> false |  _ -> true))

//should yield a Void3
Runner.run "" bigSize (Prop.forAll simpleArb (fun s -> match s with Leaf2 _ -> false | Void3 -> false |  _ -> true))

Runner.run "" bigSize (Prop.forAll Arb.int (fun i -> (-10 < i && i < 0) || (0 < i) && (i < 10 )))
run (Prop.forAll (Arb.option Arb.bool) (fun opt -> match opt with None -> false | Some b  -> b  ))
run (Prop.forAll (Arb.option Arb.int) (fun opt ->
  match opt with
  | Some n when n < 0 -> false
  | Some n when n >= 0 -> true
  | _ -> true))

let prop_RevId' (xs:list<int>) (x:int) = if (xs.Length > 2) && (x > 10) then false else true
run (Prop.forAll (Arb.list Arb.int, Arb.int) prop_RevId')

type Ordering = | LT | EQ | GT

let arbOrdering = {
  Gen = Gen.oneOf ([LT; EQ; GT] |> List.map Gen.constant)
  Shrinker = Shrink.shrinkAny
  PrettyPrinter = Pretty.prettyAny
}

let coArbOrdering = { new CoArbitrary<Ordering> with
  member __.Apply(t) =
    match t with
    | GT -> Gen.variant 0L
    | EQ -> Gen.variant 1L
    | LT -> Gen.variant 2L
}

printfn "set example1"
(Arb.func coArbOrdering Arb.bool).Gen
|> Gen.infinite Gen.Parameters.Default
|> Seq.map (fun f -> [EQ; GT; LT] |> List.map (fun a -> (a, f a)))
|> Seq.distinct
|> Seq.take (2 * 2 * 2)
|> Seq.iter (printfn "%A")

printfn "set example1"
(Arb.func coArbOrdering arbOrdering).Gen
|> Gen.infinite Gen.Parameters.Default
|> Seq.map (fun f -> [EQ; GT; LT] |> List.map (fun a -> (a, f a)))
|> Seq.distinct
|> Seq.take (3 * 3 * 3)
|> Seq.iter (printfn "%A")

Console.ReadKey() |> ignore
