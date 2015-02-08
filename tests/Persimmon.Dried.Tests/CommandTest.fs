namespace Persimmon.Dried.Tests

open System.Threading
open FsRandom
open Persimmon
open Persimmon.Dried
open UseTestNameByReflection

type Counter(n0: int) =
  let n = ref n0
  member __.Inc() =
    Monitor.Enter(n)
    try
      incr n
      !n
    finally
      Monitor.Exit(n) |> ignore
  member __.Dec() =
    Monitor.Enter(n)
    try
      decr n
      !n
    finally
      Monitor.Exit(n) |> ignore
  member __.Value = !n

type Get private () =
  inherit SuccessCommand<Counter, int, int>()
  override __.StructuredFormatDisplay = "Get"
  override __.Run(sut) = sut.Value
  override __.NextState(state) = state
  override __.PreCondition(state) = true
  override __.PostCondition(state, result) =
    let r = result = state
    Prop.apply r
  static member Instance = Get() :> Command<_, _, _>

type Inc private () =
  inherit SuccessCommand<Counter, int, int>()
  override __.StructuredFormatDisplay = "Inc"
  override __.Run(sut: Counter) = sut.Inc()
  override __.NextState(state) = state + 1
  override __.PreCondition(state) = true
  override __.PostCondition(state, result) =
    let r = result = state + 1
    Prop.apply r
  static member Instance = Inc() :> Command<_, _, _>

type Dec private () =
  inherit SuccessCommand<Counter, int, int>()
  override __.StructuredFormatDisplay = "Dec"
  override __.Run(sut: Counter) = sut.Dec()
  override __.NextState(state) = state - 1
  override __.PreCondition(_) = true
  override __.PostCondition(state, result) =
    let r = result = state - 1
    Prop.apply r
  static member Instance = Dec() :> Command<_, _, _>

type TestCommands private () =
  static member Instance = TestCommands()
  interface Commands<Counter, int> with
    member x.CanCreateNewSut(_, _, _) = true
    member x.DestroySut(_) = ()
    member x.GenCommand(_) =
      [ Get.Instance; Inc.Instance; Dec.Instance ]
      |> List.map (Command.boxResult >> Gen.constant)
      |> Gen.oneOf
    member x.GenInitialState = Gen.choose (Statistics.uniformDiscrete (0, 100))
    member x.InitialPreCondition(_) = true
    member x.NewSut(state) = Counter(state)

module CommandTest =

  let commands = property {
    apply (Commands.property 4 1000000 TestCommands.Instance)
  }
