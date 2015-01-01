namespace Persimmon.Dried

type Shrink<'T> =
  abstract member Shrink: 'T -> 'T seq

module Shrink =

  let rec interleave xs ys =
    if Seq.isEmpty xs then ys
    elif Seq.isEmpty ys then xs
    else seq {
      yield Seq.head xs
      yield! seq {
        yield Seq.head ys
        yield! interleave (Seq.skip 1 xs) (Seq.skip 1 ys)
      }
    }

  let apply s = { new Shrink<_> with
    member __.Shrink(x) = s x }

  let shrink (s: Shrink<_>) x = s.Shrink(x)

  let shrinkAny<'T> = apply (fun (_: 'T) -> Seq.empty)

  let rec private removeChunks n xs =
    if Seq.isEmpty xs then Seq.empty
    elif xs |> Seq.skip 1 |> Seq.isEmpty then seq { yield Seq.empty; yield Seq.empty }
    else
      let n1 = n / 2
      let n2 = n - n1
      let xs1 = xs |> Seq.take n1
      let xs2 = xs |> Seq.skip n1
      let xs3 = seq {
        for ys1 in removeChunks n1 xs1 do
          if not <| Seq.isEmpty ys1 then yield Seq.append ys1 xs2
      }
      let xs4 = seq {
        for ys2 in removeChunks n2 xs2 do
          if not <| Seq.isEmpty ys2 then yield Seq.append xs1 ys2
      }
      seq {
        yield xs1
        yield! seq {
          yield xs2
          yield! interleave xs3 xs4
        }
      }

  let rec private shrinkOne s zs =
    if Seq.isEmpty zs then Seq.empty
    else
      let x = Seq.head zs
      let xs = Seq.skip 1 zs
      let a = shrink x s |> Seq.map (fun x -> seq { yield x; yield! xs })
      Seq.append a (shrinkOne s xs |> Seq.map (fun xs -> seq { yield x; yield! xs }))
