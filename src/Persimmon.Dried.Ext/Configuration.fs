namespace Persimmon.Dried

open Runner
open Parameters

[<Sealed>]
type Configuration () =

  let mutable name = ""
  let mutable minSuccessfulTests = Default.MinSuccessfulTests
  let mutable minSize = Default.MinSize
  let mutable maxSize = Default.MaxSize
  let mutable prngState = Default.PrngState
  let mutable workers = Default.Workers
  let mutable callback = Default.Callback
  let mutable maxDiscardRatio = Default.MaxDiscardRatio

  member __.Name with get() = name and set(v) = name <- v
  member __.MinSuccessfulTests with get() = minSuccessfulTests and set(v) = minSuccessfulTests <- v
  member __.MinSize with get() = minSize and set(v) = minSize <- v
  member __.MaxSize with get() = maxSize and set(v) = maxSize <- v
  member __.PrngState with get() = prngState and set(v) = prngState <- v
  member __.Workers with get() = workers and set(v) = workers <- v
  member __.Callback with get() = callback and set(v) = callback <- v
  member __.MaxDiscardRatio with get() = maxDiscardRatio and set(v) = maxDiscardRatio <- v

  member internal __.Parameter = {
    MinSuccessfulTests = minSuccessfulTests
    MinSize = minSize
    MaxSize = maxSize
    PrngState = prngState
    Workers = workers
    Callback = callback
    MaxDiscardRatio = maxDiscardRatio
  }
