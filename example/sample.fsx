
#I "../src/Persimmon.Dried/bin/Debug"
#r "Persimmon"
#r "Persimmon.Dried"

open Persimmon
open Persimmon.Dried

let p1 = Prop.forAll (Arb.list Arb.int) (fun xs -> xs |> List.rev |> List.rev = xs)

let prms = { Runner.Parameters.Default with Callback = Runner.createConsoleReporter 1 }

Runner.run prms p1
