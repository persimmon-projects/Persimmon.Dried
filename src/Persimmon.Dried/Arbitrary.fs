namespace Persimmon.Dried

type Arbitrary<'T> = {
  Gen: Gen<'T>
  Shrinker: Shrink<'T>
}
