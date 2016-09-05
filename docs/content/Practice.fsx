(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/Persimmon.Dried.Gen"
#I "../../bin/Persimmon.Dried"
#r "FsRandom"
#r "Persimmon"
#r "Persimmon.Dried.Gen"
#r "Persimmon.Dried"

open Persimmon
open Persimmon.Dried

let prms = { Runner.Parameters.Default with Callback = Runner.createConsoleReporter 1 }

// port from http://gab-km.bitbucket.org/blog/html/2015/12/10/practical_persimmon_dried_en.html

(**
<div class="blog-post">

# Practical Persimmon.Dried

## Arbitray

You can use arbitraries flexibly: there are various arbitraries for basic types like `Arb.int`, `Arb.byte` and `Arb.string`, and for collection types like `Arb.list Arb.int`, `Arb.array Arb.float` and `Arb.map Arb.int Arb.string`.

You can also use arbitraries for functions or `System.Func`:

*)

// generate F# function (int -> int)
let ``function as parameter`` = Prop.forAll(Arb.func CoArb.int Arb.int)(fun f ->
  f (f 0) = (f >> f) 0
)

// generate System.Func<int, string>
let ``System.Func as parameter`` = Prop.forAll(Arb.systemFunc(CoArb.int, Arb.string))(fun f ->
  f.Invoke(0) = "0"
)

(**

A module `CoArb` represents the module of “arbitraries for functions (which are arbitraries)”.

If you want to treat arbitraries of collection types as non-empty or non-null, you can:

*)

// non empty int list
let ``non empty list`` = Prop.forAll(Arb.nonEmpty(Arb.list Arb.int), Arb.int)(fun xs i ->
  List.head xs = i
)

// non null string []
let ``non null array`` = Prop.forAll(Arb.nonNull(Arb.array Arb.string))(fun xs ->
  Array.length xs >= 0
)

(**

## Arbitrary and Gen

When you use Persimmon.Dried, you may want to generate arguments as well as you think. You can make another arbitrary from the present one.

*)

// generate int list arguments whose length are no more than 3
let arbLs = {
  Gen = Gen.listOfMaxLength 3 <| Arb.int.Gen
  Shrinker = Shrink.shrinkList <| Arb.int.Shrinker
  PrettyPrinter = Pretty.prettyList
}

let ``list of max length 3`` = Prop.forAll(arbLs)(fun xs ->
  List.length xs <= 3
)

(**

The basic arbitraries are F# records which have 3 values (labels), `Gen`, `Shrinker` and `PrettyPrinter`. In these labels, `Gen` : `Gen<'T>` takes charge of generating values, so controling it makes the result better.

`Gen` module has a lot of useful functions fo *Gen<'T>* values. There are examples in the following subsections.

### oneOf

`oneOf` function takes a sequence of *Gen<'T>* then returns one of elements in it.

*)

// use one of Gen element in the sequence
let arbOneOf = {
  Arb.int with Gen = Gen.oneOf <| List.map Gen.constant [2; 3; 5; 7]
}

let ``get one of prime numbers`` = Prop.forAll(arbOneOf)(fun i ->
  let contains v = List.exists (fun x -> x = v)
  contains i [1 .. 10]    // [2; 3; 5; 7] ⊂ [1 .. 10]
)

(**

### suchThat

`suchThat` function takes *Gen<'T>* value and a condition function, then returns *Gen<'T>* to generate values which satisfy the condition.

*)

// generate values satisfying the condition
let arbEven = {
  Arb.int with Gen = Gen.suchThat (fun i -> i % 2 = 0) Arb.int.Gen  // filter only even number
}

let ``only even number`` = Prop.forAll(arbEven)(fun i ->
  (i * i) % 4 = 0
)

(**

### listOfLength

`listOfLength` function takes a length number *N* and *Gen<'T>*, then returns *Gen<'T list>* which generates N-length ‘T list.

*)

// generate int lists whose length is 3
let arbList3 = {
  Gen = Gen.listOfLength 3 <| Arb.int.Gen
  Shrinker = Shrink.shrinkList <| Arb.int.Shrinker
  PrettyPrinter = Pretty.prettyList
}

// generate int lists whose length is 5
let arbList5 = {
  arbList3 with Gen = Gen.listOfLength 5 <| Arb.int.Gen
}

let ``list of length`` = Prop.forAll(arbList3, arbList5)(fun xs3 xs5 ->
  List.length xs3 < List.length xs5
)

(**

There are similar functions such as `listOfMaxLength` which generates lists less than or equal to length N, and `listOfMinLength` which generates lists greater than or equal to length N.

If you want to know more about Gen, let’s read [the source code](https://github.com/persimmon-projects/Persimmon.Dried/blob/b056641423471b990e385b50db9e50589bfdff4e/src/Persimmon.Dried/Gen.fs).

## Properties

In `Prop` module, there are useful functions and operators to write properties.

### And / Or operator

The And operator `.&.` succeeds if both of properties succeed, otherwise fails.

*)

// lhs .&. lazy rhs succeeds both of lhs and rhs
let ``and operator`` = Prop.forAll(Arb.int, Arb.int)(fun x y ->
  x + 1 > x |@ "x + 1 is greater than x" .&.
  lazy (y - 1 < y |@ "y - 1 is less than y") .&.
  lazy (x * y > x + y |@ "x * y is greather than x + y")  // This property may fail
)

(**

On the other hand, the Or operator `.|.` succeeds if either property succeeds, fails if both fail.

*)

// lhs .|. lazy rhs succeeds either of lhs and rhs
let ``or operator`` = Prop.forAll(Arb.int, Arb.int)(fun x y ->
  "x + 1 is greater than x" @| (x + 1 > x) .|.
  lazy ("y - 1 is less than y" @| (y - 1 < y)) .|.
  lazy ("x * y is greather than x + y" @| (x * y > x + y))  // This property may fail
)

(**

If you execute each 2 properties, *and operator* will fail and *or operator* will succeed.

`|@` and `@|` you see in the code snipets above are the labeling operators. You can use them as p |@ s or s @| p, then label p s.

### Conditional property

`==>` operator checks a right hand side property only if a left hand side property succeeds.

*)

// lhs ==> lazy rhs checks rhs if lhs succeeds.
let ``conditional property`` = Prop.forAll(Arb.int, Arb.list Arb.int)(fun i ls ->
  let ils = List.Cons(i, ls)
  List.length ls > 5  ==> lazy (List.length ils > 6)
)

(**

This snipet checks whether the length of *i :: ls* is greater than 6 or not if the input list *ls* has greater length than 5.

Note that the binary operators which take both properties, same as `.&.` or `.|.`, must take its right hand side property as `lazy`. This restriction is for the reason that F# mainly uses eager evaluation. In the case of rasing an exception when the right property is evaluated, the exception is occurred at the time of passing both properties to the operator, then the check fails.

```fsharp
// What if we could...
let ``right hand side executes... what?`` = Prop.forAll(Arb.int)(fun i ->
  (i + 1 = i) ==> (i / 0 = 0)  // write like this?
)
```

### Classify properties

`classify` function is similar to the labeling operators in the way of naming properties. Moreover, it can classify them by any conditions.

*)

let ``classifying test case`` = Prop.forAll(Arb.int, Arb.int)(fun x y ->
  x + y > x
  |> Prop.classify(x < 0, "x is negative")
  |> Prop.classify((x = 0), "x is zero")
  |> Prop.classify(y < 0, "y is negative")
  |> Prop.classify((y = 0), "y is zero")
)

(**

When you execute the snipet above, it may show you as follows:

*)

(*** define-output: classify ***)
Runner.run "" prms ``classifying test case``

(*** include-output: classify ***)

(**

You can get the ratio of failing arguments from here.

</div>
*)
