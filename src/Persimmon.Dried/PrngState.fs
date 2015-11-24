module Persimmon.Dried.PrngState

open System
open FsRandom
open Nessos.FsPickler

let ofBinary bin =
  let binary = FsPickler.CreateBinarySerializer()
  binary.UnPickle<PrngState>(bin)

let ofBinaryString (bin: string) =
  let ofBinaryStringForOldFormat () =
    bin.Split([|'-'; ' '|])
    |> Array.map (fun x -> Convert.ToByte(x, 16))

  let bin =
    if bin.Contains("-") then
      ofBinaryStringForOldFormat ()
    else
      Convert.FromBase64String(bin)
  ofBinary bin

let toBinary (state: PrngState) =
  let binary = FsPickler.CreateBinarySerializer()
  binary.Pickle state

let toBinaryString (state: PrngState) =
  Convert.ToBase64String(toBinary state)
