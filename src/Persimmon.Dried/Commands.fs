namespace Persimmon.Dried

type Command<'Sut, 'State, 'Result> =
  abstract member Run: 'Sut -> 'Result
  abstract member NextState: 'State -> 'State
  abstract member PreCondition: 'State -> bool
  abstract member PostCondition: 'State * Choice<'Result, exn> -> Prop
  abstract member StructuredFormatDisplay: string

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
type private BoxedCommand<'Sut, 'State, 'Result>(command: Command<'Sut, 'State, 'Result>) =
  member __.StructuredFormatDisplay = command.StructuredFormatDisplay
  interface Command<'Sut, 'State, obj> with
    member this.StructuredFormatDisplay = this.StructuredFormatDisplay
    member __.Run(sut) = box(command.Run(sut))
    member __.NextState(state) = command.NextState(state)
    member __.PreCondition(state) = command.PreCondition(state)
    member __.PostCondition(state, result) =
      let result =
        match result with
        | Choice1Of2 r -> Choice1Of2 (unbox<'Result> r)
        | Choice2Of2 e -> Choice2Of2 e
      command.PostCondition(state, result)

[<AutoOpen>]
module CommandSyntax =

  type Command<'Sut, 'State, 'Result> with
    member this.Boxed = BoxedCommand(this) :> Command<'Sut, 'State, obj>

module Command =

  let run sut (command: Command<_, _, _>) = command.Run(sut)
  let nextState state (command: Command<_, _, _>) = command.NextState(state)
  let preCondition state (command: Command<_, _, _>) = command.PreCondition(state)
  let postCondition state result (command: Command<_, _, _>) = command.PostCondition(state, result)
  
  let internal runPC sut (command: Command<'Sut, 'State, 'Result>) =
    let r =
      try
        Choice1Of2(command.Run(sut))
      with e ->
        Choice2Of2 e
    let r1 = match r with | Choice1Of2 r -> Choice1Of2(sprintf "%A" r) | Choice2Of2 e -> Choice2Of2 e
    (r1, fun s -> (preCondition s command) ==> lazy (postCondition s r command))

  type Unit = Unit

type Commands<'Sut, 'State> =
  abstract member CanCreateNewSut: 'State * 'State seq * 'Sut seq -> bool
  abstract member NewSut: 'State -> 'Sut
  abstract member DestroySut: 'Sut -> unit
  abstract member InitialPreCondition: 'State -> bool
  abstract member GenInitialState: Gen<'State>
  abstract member GenCommand: 'State -> Gen<Command<'Sut, 'State, obj>>

[<AbstractClass>]
type SuccessCommand<'Sut, 'State, 'Result>() =
  abstract member PostCondition: 'State * 'Result -> Prop
  abstract member Run: 'Sut -> 'Result
  abstract member NextState: 'State -> 'State
  abstract member PreCondition: 'State -> bool
  abstract member StructuredFormatDisplay: string
  default this.StructuredFormatDisplay = this.ToString()
  interface Command<'Sut, 'State, 'Result> with
    member this.PostCondition(state, result) =
      match result with
      | Choice1Of2 result -> this.PostCondition(state, result)
      | Choice2Of2 e -> Prop.exn e
    member this.Run(sut) = this.Run(sut)
    member this.NextState(state) = this.NextState(state)
    member this.PreCondition(state) = this.PreCondition(state)
    member this.StructuredFormatDisplay = this.StructuredFormatDisplay

[<AbstractClass>]
type UnitCommand<'Sut, 'State>() =
  abstract member PostCondition: 'State * bool -> Prop
  abstract member Run: 'Sut -> Command.Unit
  abstract member NextState: 'State -> 'State
  abstract member PreCondition: 'State -> bool
  abstract member StructuredFormatDisplay: string
  default this.StructuredFormatDisplay = this.ToString()
  // compile error
  //interface Command<'Sut, 'State, unit> with
  interface Command<'Sut, 'State, Command.Unit> with
    member this.PostCondition(state, result) =
      this.PostCondition(state, match result with | Choice1Of2 _ -> true | Choice2Of2 _ -> false)
    member this.Run(sut) = this.Run(sut)
    member this.NextState(state) = this.NextState(state)
    member this.PreCondition(state) = this.PreCondition(state)
    member this.StructuredFormatDisplay = this.StructuredFormatDisplay

type NoOp<'Sut, 'State> = | NoOp
  with
    interface Command<'Sut, 'State, obj> with
      member __.Run(_) = null
      member __.NextState(state) = state
      member __.PreCondition(_) = true
      member __.PostCondition(_, _) = Prop.apply(true)
      member this.StructuredFormatDisplay = sprintf "%A" NoOp

module Commands =

  open System.Collections.Generic

  type Commands<'Sut, 'State, 'Result> = Command<'Sut, 'State, 'Result> list

  type Actions<'Sut, 'State, 'Result> = {
    S: 'State
    SeqCmds: Commands<'Sut, 'State, 'Result>
    ParCmds: Commands<'Sut, 'State, 'Result> list
  }

  let shrinkActions<'Sut, 'State, 'Result> = Shrink.apply(fun (ac: Actions<'Sut, 'State, 'Result>) ->
    Shrink.shrink Shrink.shrinkAny ac.ParCmds
    |> Seq.map(fun cs -> { ac with ParCmds = cs })
    |> Seq.append (Shrink.shrink Shrink.shrinkAny ac.SeqCmds |> Seq.map (fun cs -> { ac with SeqCmds = cs }))
  )

  let runSeqCmds (sut: 'Sut) (s0: 'State) (cs: Commands<'Sut, 'State, 'Result>) =
    ((Prop.proved.Value, s0, []), cs)
    ||> List.fold (fun (p, s, rs) c ->
      let r, pf = Command.runPC sut c
      (p .&. lazy (pf s), c.NextState(s), r :: rs))
    |> fun (p, s, rs) -> (p, s, List.rev rs)

  let rec private scan (f: _ -> _ -> _) = function
  | [] -> []
  | y::ys -> f y ys :: scan (fun x xs -> f x (y::xs)) ys

  let runParCmds (sut: 'Sut) (s: 'State) (pcmds: Commands<_, _, 'Result> list) =
    let memo = Dictionary<'State * Commands<'Sut, 'State, 'Result> list, 'State list>()

    let rec endStates (scss: 'State * Commands<_, _, _> list) =
      let s, css = (fst scss, snd scss |> List.filter (not << List.isEmpty))
      match memo.TryGetValue((s, css)), css with
      | ((true, states), _) -> states
      | (_, []) -> [s]
      | (_, cs :: []) ->
        [
          cs
          |> List.rev
          |> Seq.skip 1
          |> Seq.toList
          |> List.rev
          |> List.fold (fun  s0 c -> c.NextState(s0)) s
        ]
      | _ ->
        let inits = css |> scan (fun cs x -> ((List.head cs).NextState(s), (List.tail cs) :: x))
        let states =
          inits
          |> Seq.distinct
          |> Seq.collect endStates
          |> Seq.distinct
          |> Seq.toList
        memo.Add((s,css), states)
        states

    let run (endStates: 'State list) (cs: Commands<_, _, _>) = async {
      return
        if List.isEmpty cs then (Prop.proved.Value, [])
        else
          let rs =
            cs
            |> List.rev
            |> Seq.skip 1
            |> Seq.toList
            |> List.rev
            |> List.map (Command.runPC sut >> fst)
          let r, pf = Command.runPC sut (Seq.last cs)
          (Prop.atLeastOne (endStates |> List.map pf), List.zip cs (List.rev (r :: List.rev rs)))
    }

    async {
      let! l = pcmds |> List.map (run (endStates (s, pcmds))) |> Async.Parallel
      let ps, rs = Array.unzip l
      return (Prop.atLeastOne ps, List.ofArray rs)
    }
    |> Async.RunSynchronously

  let private prettyCmdsRes (rs: (Command<_, _, _> * Choice<string, exn>) list) =
    let cs =
      rs
      |> List.map (function
      | (c, Choice1Of2 "()") -> sprintf "%A" c
      | (c, Choice1Of2 r) -> sprintf "%A => %s" c r
      | (c, r) -> sprintf "%A => %A" c r
    )
    "(" + List.fold (fun acc c -> if System.String.IsNullOrEmpty(acc) then c else acc + "; " + c) "" cs + ")"

  let private propAnd p1 p2 =
    p1 |> Prop.bind (fun r -> if PropResult.isSuccess r then Prop.secure p2 else Prop.apply(fun _ -> r))

  let private runActions sut (ac: Actions<_, _, _>) =
    let (p1, s, rs1) = runSeqCmds sut ac.S ac.SeqCmds
    let l1 = sprintf "Initialstate = %A\nSeqcmds = %s" ac.S (prettyCmdsRes (List.zip ac.SeqCmds rs1))
    if List.isEmpty ac.ParCmds then p1 |@ l1
    else
      let f () =
        let (p2, rs2) = runParCmds sut s ac.ParCmds
        let l2 = rs2 |> List.map prettyCmdsRes
        let l2 = "(" + List.fold (fun acc c -> acc + "\n" + c) "" l2 + ")"
        p2 |@ l1 |@ sprintf "Parcmds = (state = %A) %s" s l2
      propAnd (p1 |@ l1) f

  let private action threadCount maxParComb (commands: Commands<_, _>) =

    let sizedCmds (s: 'State) (sz: int) =
      let l = List.init sz (fun _ -> ())
      (Gen.constant (s, []), l)
      ||> List.fold (fun g () -> gen {
        let! s0, cs = g
        let! c = commands.GenCommand(s0).SuchThat(fun x -> x.PreCondition(s0))
        return (c.NextState(s0), List.rev (c :: List.rev cs))
      })

    let rec cmdsPrecond (s: 'State) (cmds: Commands<_, _, _>) =
      match cmds with
      | [] -> (s, true)
      | c::cs when c.PreCondition(s) -> cmdsPrecond (c.NextState(s)) cs
      | _ -> (s, false)
      
    let actionsPrecond (ac: Actions<_, _, _>) =
      List.length ac.ParCmds <> 1 && ac.ParCmds |> List.forall(not << List.isEmpty) &&
      commands.InitialPreCondition(ac.S) &&
        (match cmdsPrecond ac.S ac.SeqCmds with
        | (s, true) -> ac.ParCmds |> List.forall (cmdsPrecond s >> snd)
        | _ -> false)

    let parSz =
      let rec seqs n m = if n = 1 then 1.0 else round (pown (float n) m) * seqs (n - 1) m
      if threadCount < 2 then 0
      else
        let parSz = ref 1
        while int (seqs threadCount !parSz) < maxParComb do incr parSz
        !parSz

    let g = gen {
      let! s0 = commands.GenInitialState
      let! s1, seqCmds = Gen.sized (sizedCmds s0)
      let! parCmds =
        if parSz <= 0 then Gen.constant []
        else Gen.listOfLength threadCount (sizedCmds s1 parSz |> Gen.map snd)
      return { S = s0; SeqCmds = seqCmds; ParCmds = parCmds }
    }

    g.SuchThat(actionsPrecond)

  let property threadCount maxParComb (commands: Commands<'Sut, 'State>) =
    let suts = Dictionary<obj, 'State * 'Sut option>()
    let arb = {
      Gen = action threadCount maxParComb commands
      Shrinker = shrinkActions
      PrettyPrinter = Pretty.prettyAny
    }
    Prop.forAll arb (fun ac ->
      try
        let sutId =
          lock(suts) (fun () ->
            let initSuts = [ for (state, o) in suts.Values do match o with None -> yield state | _ -> () ]
            let runningSuts = [ for (_, o) in suts.Values do match o with Some sut -> yield sut | None -> () ]
            if commands.CanCreateNewSut(ac.S, initSuts, runningSuts) then
              let sutId = obj()
              suts.Add(sutId, (ac.S, None))
              Some sutId
            else None)
        match sutId with
        | Some id ->
          let sut = commands.NewSut(ac.S)
          let doRun =
            lock(suts) (fun () ->
              if suts.ContainsKey(id) then
                suts.[id] <- (ac.S, Some sut)
                true
              else false)
          try
            if doRun then runActions sut ac else Prop.undecided.Value
          finally
            lock(suts) (fun () ->
            suts.Remove(id) |> ignore
            commands.DestroySut(sut))
        | None ->
          //printfn "NOT IMPL"
          Prop.undecided.Value
      with e ->
        suts.Clear()
        reraise ()
    )

  let propertyWithDefault commands = property 1 1000000 commands
