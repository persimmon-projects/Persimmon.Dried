module Persimmon.Dried.PrngState

open System
open FsRandom
open Nessos.FsPickler

let ofBinary bin =
  let binary = FsPickler.CreateBinary()
  binary.UnPickle<PrngState>(bin)

let ofBinaryString (bin: string) =
  Text.Encoding.ASCII.GetBytes(bin)
  |> ofBinary

let toBinary (state: PrngState) =
  let binary = FsPickler.CreateBinary()
  binary.Pickle state

let toBinaryString (state: PrngState) =
  BitConverter.ToString(toBinary state)
