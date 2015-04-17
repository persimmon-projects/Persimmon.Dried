namespace Persimmon.Dried

open System.Collections.Generic

// key type of underlying is obj because 'T avoid equality constraint
[<Sealed>]
type FreqMap<'T>(underlying: Dictionary<obj, int>, total: int) =
  
  member __.Total = total
  member internal __.Underlying = underlying

  override __.ToString() = sprintf "%A" underlying

module FreqMap =

  let empty<'T> = FreqMap<'T>(Dictionary<obj, int>(), 0)

  let add t (m: FreqMap<_>) =
    let d = Dictionary(m.Underlying)
    let n =
      match m.Underlying.TryGetValue(t) with
      | true, n ->
        d.Remove(t) |> ignore
        n + 1
      | false, _ -> 1
    d.Add(t, n)
    FreqMap(d, m.Total + 1)

  let remove t (m: FreqMap<_>) =
    let u =
      match m.Underlying.TryGetValue(t) with
      | true, n ->
        let d = Dictionary(m.Underlying)
        d.Remove(t) |> ignore
        d.Add(t, n - 1)
        d
      | false, _ -> Dictionary(m.Underlying)
    FreqMap(u, m.Total + 1)

  let tryFindCount t (m: FreqMap<_>) =
    match m.Underlying.TryGetValue(t) with
    | true, v -> Some v
    | false, _ -> None

  let append (m1: FreqMap<_>) (m2: FreqMap<_>) =
    let keys = Seq.append (m2.Underlying |> Seq.map (fun (KeyValue(k, _)) -> k)) (m1.Underlying |> Seq.map (fun (KeyValue(k, _)) -> k))
    let mappings =
      keys
      |> Seq.map (fun x ->
        let n1 = match tryFindCount x m1 with | Some v -> v | None -> 0
        let n2 = match tryFindCount x m2 with | Some v -> v | None -> 0
        (x, n1 + n2))
    let d = Dictionary<obj, int>()
    mappings |> Seq.iter (fun (k, v) -> d.Add(k, v))
    FreqMap(d, m1.Total + m2.Total)

  let sub (m1: FreqMap<_>) (m2: FreqMap<_>) =
    let u =
      m2.Underlying
      |> Seq.map (fun (KeyValue(x, n)) ->
        match tryFindCount x m1 with
        | Some v -> (x, n - v)
        | None -> (x, n))
    let d = Dictionary<obj, int>()
    u |> Seq.iter (fun (k, v) -> d.Add(k, v))
    FreqMap(d, m2.Underlying |> Seq.fold (fun acc (KeyValue(_, n)) -> acc + n) 0)

  let counts (m: FreqMap<_>) = m.Underlying |> Seq.map (fun (KeyValue(k, v)) -> (k, v)) |> Seq.sortBy snd

  let tryFindRatio t (m: FreqMap<_>) = tryFindCount t m |> Option.map (fun c -> float32 c / float32 m.Total)

  let ratios (m: FreqMap<'T>) = counts m |> Seq.map (fun (t, c) -> (unbox<'T> t, float32 c / float32 m.Total))

  open Pretty
  open Helper

  let pretty (fm: FreqMap<_ list>) = Pretty(fun _ ->
    if fm.Total = 0 then ""
    else
      "> Collected test data: " -/ String.concat newLine (seq {
        for (xs, r) in ratios fm do
          if not <| List.isEmpty xs then
          let xs = xs |> List.filter ((<>) null)
            yield sprintf "%d%% %s" (r * 100.0f |> round |> int) (xs |> Seq.map (fun x -> x.ToString()) |> String.concat ", ")
      }))
