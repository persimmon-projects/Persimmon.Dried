namespace Persimmon.Dried

type NonShrinkerArbitrary<'T> = {
  Gen: Gen<'T>
  PrettyPrinter: 'T -> Pretty
}

type Arbitrary<'T> = {
  Gen: Gen<'T>
  Shrinker: Shrink<'T>
  PrettyPrinter: 'T -> Pretty
}
with
  member this.NonShrinker: NonShrinkerArbitrary<'T> = {
    Gen = this.Gen
    PrettyPrinter = this.PrettyPrinter
  }
