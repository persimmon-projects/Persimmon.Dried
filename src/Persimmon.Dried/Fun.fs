namespace Persimmon.Dried

type Fun<'T, 'U> = {
  F: CFunc<'T, 'U>
  U: 'U
  G: 'T -> 'U
}

module Fun =

  let apply f = f.G

