namespace Persimmon.Dried

open Persimmon
open UseTestNameByReflection

type Unit private () =
  static member Value = Unit()

type Property private (state: PropertiesState) =
  let property = property
  static member Default = Property(property.Yield())
  member __.Verbosity(value) = Property(property.Verbosity(state, value))
  member __.MinSuccessfulTests(value) = Property(property.MinSuccessfulTests(state, value))
  member __.MinSize(size) = Property(property.MinSize(state, size))
  member __.MaxSize(size) = Property(property.MaxSize(state, size))
  member __.PrngState(prngState) = Property(property.PrngState(state, prngState))
  member __.Workers(count) = Property(property.Workers(state, count))
  member __.Callback(callback) = Property(property.Callback(state, callback))
  member __.MaxDiscardRatio(ratio) = Property(property.MaxDiscardRatio(state, ratio))
  member __.Add(prop) = Property(property.Apply(state, prop))
  member __.Run() =
    test {
      do! property.Run(fun () -> state)
      return Unit.Value
    }
  static member op_Implicit(prop: Property) = prop.Run()
