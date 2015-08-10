module Persimmon.Dried.PrngState

open System
open FsRandom
open Nessos.FsPickler

let ofBinary bin =
  let binary = FsPickler.CreateBinarySerializer()
  binary.UnPickle<PrngState>(bin)

let ofBinaryString (bin: string) =
  bin.Split([|'-'; ' '|])
  |> Array.map (fun x -> Convert.ToByte(x, 16))
  |> ofBinary

let toBinary (state: PrngState) =
  let binary = FsPickler.CreateBinarySerializer()
  binary.Pickle state

let toBinaryString (state: PrngState) =
  BitConverter.ToString(toBinary state)
