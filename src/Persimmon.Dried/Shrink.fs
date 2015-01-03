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

  let rec private shrinkOne (s: Shrink<_>) zs =
    if Seq.isEmpty zs then Seq.empty
    else
      let x = Seq.head zs
      let xs = Seq.skip 1 zs
      let a = shrink s x |> Seq.map (fun x -> seq { yield x; yield! xs })
      Seq.append a (shrinkOne s xs |> Seq.map (fun xs -> seq { yield x; yield! xs }))

  let private shrinkContainer (s: Shrink<'T>) v = apply (fun xs ->
    Seq.append (removeChunks (Seq.length xs) xs) (shrinkOne s xs)
    |> Seq.map v)

  let shrinkSeq s = shrinkContainer s id
  let shrinkList s = shrinkContainer s Seq.toList
  let shrinkArray s = shrinkContainer s Seq.toArray

  // FIXME
  let private shrinkUInt' f g n =

    let n = f n

    let rec halfs n =
      if n = 0UL then Seq.empty else seq { yield n; yield! halfs (n / 2UL) }

    if n = 0UL then Seq.empty
    else
      let ns = halfs (n / 2UL) |> Seq.map (fun x -> n - x)
      seq { yield 0UL; yield! interleave ns ns }
    |> Seq.map g

  let private shrinkInt' f g n =
    
    let n = f n

    let rec halfs n =
      if n = 0L then Seq.empty else seq { yield n; yield! halfs (n / 2L) }

    if n = 0L then Seq.empty
    else
      let ns = halfs (n / 2L) |> Seq.map (fun x -> n - x)
      seq { yield 0L; yield! interleave ns (ns |> Seq.map (fun x -> -1L * x)) }
    |> Seq.map g

  let shrinkByte = apply (shrinkUInt' uint64 byte)
  let shrinkUInt16 = apply (shrinkUInt' uint64 uint16)
  let shrinkUInt32 = apply (shrinkUInt' uint64 uint32)
  let shrinkUInt64 = apply (shrinkUInt' uint64 id)
  let shrinkSbyte = apply (shrinkInt' int64 sbyte)
  let shrinkInt16 = apply (shrinkInt' int64 int16)
  let shrinkInt = apply (shrinkInt' int64 int)
  let shrinkInt64 = apply (shrinkInt' int64 id)

  let shrinkString = apply (fun (s: string) ->
    shrink (shrinkArray shrinkAny) (s.ToCharArray())
    |> Seq.map(fun a -> System.String(a))
  )

  let shrinkOption s = apply (function
    | None -> Seq.empty
    | Some x -> seq {
        yield None
        for y in shrink s x -> Some(y)
      })

  let shrinkTuple2 s1 s2 = apply (fun (t1, t2) ->
      let xs = shrink s1 t1 |> Seq.map (fun x -> (x, t2))
      let ys = shrink s2 t2 |> Seq.map (fun x -> (t1, x))
      Seq.append xs ys)

  let shrinkTuple3 s1 s2 s3 = apply (fun (t1, t2, t3) ->
      let xs = shrink s1 t1 |> Seq.map (fun x -> (x, t2, t3))
      let ys = shrink s2 t2 |> Seq.map (fun x -> (t1, x, t3))
      let zs = shrink s3 t3 |> Seq.map (fun x -> (t1, t2, x))
      Seq.append (Seq.append xs ys) zs)

  let xmap st (from: _ -> _) (to': _ -> _) = apply (fun u -> shrink (to' u) st |> Seq.map from)
