open System
open System.IO
open System.Diagnostics
open Persimmon.Dried

let score n (xs: TimeSpan seq) =
  let m, n = (n + 1) % 2 = 0, (n + 1) / 2
  let median =
    if m then Seq.nth (n - 1) xs
    else
      let a = Seq.nth (n - 1) xs
      let b = Seq.nth n xs
      TimeSpan((a.Ticks + b.Ticks) / 2L)
  (Seq.min xs, median, Seq.max xs)

let watch n check =
  let inner _ =
    let watch = Stopwatch.StartNew()
    let result = check ()
    printf "%s" (if result = 0 then "." else "x")
    watch.Stop()
    (result, watch.Elapsed)
  let result = List.init n id |> List.map inner
  printfn ""
  result
  |> Seq.map snd
  |> score n
  |||> printfn """[execution time]
min: %A
median: %A
max: %A"""
  result |> Seq.map fst |> Seq.fold (|||) 0

let target = Prop.forAll (Arb.list Arb.int) (fun l ->
  Shrink.shrink (Arb.list Arb.int).Shrinker l
  |> Seq.forall ((<>) l))

let prms =  {
  Runner.Parameters.Default with
    MinSize = 10
    MaxSize = 10
}

[<EntryPoint>]
let main args = 
  (fun () -> Runner.mainRunner prms target)
  |> watch (int args.[0])
