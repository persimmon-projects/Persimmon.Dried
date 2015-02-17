namespace Commands.Redis

open Persimmon
open Persimmon.Dried
open StackExchange.Redis
open CloudStructures
open System.Runtime.CompilerServices

[<Extension>]
type AsyncBuilderExtensions() =
  [<Extension>]
  static member Source(_: AsyncBuilder, t) = Async.AwaitTask t
  [<Extension>]
  static member Source(_: AsyncBuilder, s) = Async.Parallel(Seq.map Async.AwaitTask s)

type State = {
  Contents: Map<string, string>
  Deleted: Set<string>
  Connected: bool
}

type Client(host: string, port: int, options: string) =
  let connectionString = sprintf "%s:%d,%s" host port options
  let mutable settings = RedisSettings(connectionString)
  member __.IsConnected = settings.GetConnection().IsConnected
  member __.Get<'T>(key: string) =
    async {
      let! v = RedisString<'T>(settings, RedisKey.op_Implicit(key)).Get()
      return if v.HasValue then Some v.Value else None
    }
    |> Async.RunSynchronously
  member __.FlushDb() =
    settings.GetConnection().GetServer(host, port).FlushDatabase()
  member __.Quit() =
    __.ReConnect()
    while not __.IsConnected do Async.Sleep(200) |> Async.RunSynchronously
    settings.GetConnection().Dispose()
  member __.ReConnect() =
    if not <| settings.GetConnection().IsConnected then settings <- RedisSettings(connectionString)
  member __.Del<'T>(keys: string []) =
    async {
      let! results = keys |> Array.map (fun key -> RedisString<_>(settings, RedisKey.op_Implicit(key)).Delete())
      return Array.sumBy (fun x -> System.Convert.ToInt64(x: bool)) results
    }
    |> Async.RunSynchronously
  member __.Set(key: string, value: 'T) =
    async {
      return! RedisString<'T>(settings, RedisKey.op_Implicit(key)).Set(value)
    }
    |> Async.RunSynchronously
  member __.DbSize = settings.GetConnection().GetServer(host, port).DatabaseSize()

type ToggleConnected private () =
  member __.StructuredFormatDisplay = "ToggleConnected"
  static member Instance =
    Gen.constant(ToggleConnected() :> Command<_, _, _>) |> Gen.map Command.boxResult
  interface Command<Client, State, bool> with
    member this.StructuredFormatDisplay = this.StructuredFormatDisplay
    member __.NextState(state) = { state with Connected = not state.Connected }
    member __.PostCondition(_, result) =
      match result with
      | Choice1Of2 true -> true
      | _ -> false
      |> Prop.apply
    member __.PreCondition(_) = true
    member __.Run(sut) =
      if sut.IsConnected then sut.Quit()
      else sut.ReConnect()
      true

type Get = {
  Key: string
}
with
  member this.StructuredFormatDisplay = sprintf "Get(%s)" this.Key
  interface Command<Client, State, string option> with
    member this.StructuredFormatDisplay = this.StructuredFormatDisplay
    member __.NextState(state) = state
    member this.PostCondition(state, result) =
      match result with
      | Choice1Of2 v -> Map.tryFind this.Key state.Contents = v
      | _ -> false
      |> Prop.apply
    member __.PreCondition(state) = state.Connected
    member this.Run(sut) = sut.Get<string>(this.Key)

type Del = {
  Keys: string seq
}
with
  member this.StructuredFormatDisplay = sprintf "Del %A" this.Keys
  interface Command<Client, State, int64 option> with
    member this.StructuredFormatDisplay = this.StructuredFormatDisplay
    member this.NextState(state) = {
      state with
        Contents = state.Contents |> Map.filter (fun x _ -> Seq.exists ((<>) x) this.Keys)
        Deleted = Set.union state.Deleted (Set.ofSeq this.Keys)
    }
    member this.PostCondition(state, result) =
      match result with
      | Choice1Of2 v ->
        let l = int64 (state.Contents |> Map.filter (fun x _ -> Seq.exists ((=) x) this.Keys)).Count
        v |> Option.exists ((=) l)
      | _ -> false
      |> Prop.apply
    member __.PreCondition(state) = state.Connected
    member this.Run(sut) =
      if Seq.isEmpty this.Keys then Some 0L
      else
        Some (sut.Del(Array.ofSeq this.Keys))

type Set = {
  Key: string
  Value: string
}
with
  member this.StructuredFormatDisplay = sprintf "Set(%s, %s)" this.Key this.Value
  interface Command<Client, State, bool> with
    member this.StructuredFormatDisplay = this.StructuredFormatDisplay
    member this.NextState(state) = {
      state with
        Contents = state.Contents |> Map.add this.Key this.Value
        Deleted = Set.filter ((<>) this.Key) state.Deleted
    }
    member __.PostCondition(_, result) =
      match result with
      | Choice1Of2 true -> true
      | _ -> false
      |> Prop.apply
    member __.PreCondition(state) = state.Connected
    member this.Run(sut) = sut.Set(this.Key, this.Value)

type FlushDB private () =
  static member Instance =
    Gen.constant(FlushDB() :> Command<_, _, _>) |> Gen.map Command.boxResult
  member __.StructuredFormatDisplay = "FlushDB"
  interface Command<Client, State, bool> with
    member this.StructuredFormatDisplay = this.StructuredFormatDisplay
    member __.NextState(state) = { state with Contents = Map.empty }
    member __.PostCondition(_, result) =
      match result with
      | Choice1Of2 true -> true
      | _ -> false
      |> Prop.apply
    member __.PreCondition(state) = state.Connected
    member __.Run(sut) =
      sut.FlushDb()
      true

type DBSize private () =
  static member Instance =
    Gen.constant(DBSize() :> Command<_, _, _>) |> Gen.map Command.boxResult
  member __.StructuredFormatDisplay = "DBSize"
  interface Command<Client, State, int64 option> with
    member this.StructuredFormatDisplay = this.StructuredFormatDisplay
    member __.NextState(state) = state
    member __.PostCondition(state, result) =
      match result with
      | Choice1Of2 s -> s |> Option.exists ((=) (int64 state.Contents.Count))
      | _ -> false
      |> Prop.apply
    member __.PreCondition(state) = state.Connected
    member __.Run(sut) = Some sut.DbSize

module Commands =

  let genKey = Gen.identifier
  let genValue = Gen.identifier

  let genSet = gen {
    let! key = genKey
    let! value = genValue
    return { Key = key; Value = value }
  }

  let genGet = genKey |> Gen.map (fun k -> { Get.Key = k })

  let genDel = Gen.nonEmptyListOf genKey |> Gen.map (fun ks -> { Keys = ks })

  let genDelExisting state =
    if Map.isEmpty state.Contents then genDel
    else
      state.Contents
      |> Map.toSeq
      |> Seq.map fst
      |> Gen.someOf
      |> Gen.map (fun ks -> { Keys = ks })

  let genSetExisting state =
    if Map.isEmpty state.Contents then genSet
    else gen {
      let! key = Gen.oneOf (state.Contents |> Map.toSeq |> Seq.map (fst >> Gen.constant))
      let! value = Gen.oneOf [ genValue; Gen.constant (state.Contents.[key]) ]
      return { Key = key; Value = value }
    }

  let genGetExisting state =
    if Map.isEmpty state.Contents then genGet
    else gen {
      let! key = Gen.oneOf (state.Contents |> Map.toSeq |> Seq.map (fst >> Gen.constant))
      return { Get.Key = key }
    }

  let genGetDeleted state =
    if Set.isEmpty state.Deleted then genGet
    else gen {
      let! key = Gen.oneOf (state.Deleted |> Set.toSeq |> Seq.map Gen.constant)
      return { Get.Key = key }
    }

open Commands

type CommandsRedis() =
  interface Commands<Client, State> with
    member __.CanCreateNewSut(_, initSuts, runningSuts) =
      Seq.isEmpty initSuts && Seq.isEmpty runningSuts
    member __.DestroySut(sut) =
      sut.FlushDb()
      sut.Quit()
    member __.GenCommand(state) =
      if not state.Connected then ToggleConnected.Instance
      else
        Gen.frequency [
          (20, genDel |> Gen.map Command.boxResult)
          (10, genDelExisting state |> Gen.map Command.boxResult)
          (50, genSet |> Gen.map Command.boxResult)
          (10, genSetExisting state |> Gen.map Command.boxResult)
          (20, genGet |> Gen.map Command.boxResult)
          (20, genGetExisting state |> Gen.map Command.boxResult)
          (20, genGetDeleted state |> Gen.map Command.boxResult)
          (20, DBSize.Instance)
          (1, FlushDB.Instance)
          (3, ToggleConnected.Instance)
        ]
    member __.GenInitialState = Gen.constant {
      Contents = Map.empty
      Deleted = Set.empty
      Connected = true
    }
    member __.InitialPreCondition(state) = state.Connected
    member __.NewSut(_) = Client("localhost", 6379, "allowAdmin=true")

module CommandsRedisTest =

  open UseTestNameByReflection

  let redisspec = property {
    apply (Commands.property 1 1000000 (CommandsRedis()))
  }
