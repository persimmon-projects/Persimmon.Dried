(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#I "../../src/Persimmon.Dried/bin/Release"
#r "FsRandom"
#r "Persimmon"
#r "Persimmon.Dried"

open Persimmon
open Persimmon.Dried

let prms = { Runner.Parameters.Default with Callback = Runner.createConsoleReporter 1 }

// port from http://gab-km.bitbucket.org/blog/html/2015/12/10/practical_persimmon_dried_en.html

(**
<div class="blog-post">

# 実践Persimmon.Dried

## Arbitrary

Persimmon.Driedでは、引数候補はかなり柔軟に指定することができます。 `Arb.int` や `Arb.byte`、 `Arb.string` などの基本型や、 `Arb.list Arb.int`、 `Arb.array Arb.float`、 `Arb.map Arb.int Arb.string` のようなコレクション型も指定できます。

関数や `System.Func` を引数候補にすることもできます：

*)

// F# の関数(int -> int)を生成する
let ``function as parameter`` = Prop.forAll(Arb.func CoArb.int Arb.int)(fun f ->
  f (f 0) = (f >> f) 0
)

// System.Func<int, string> を生成する
let ``System.Func as parameter`` = Prop.forAll(Arb.systemFunc(CoArb.int, Arb.string))(fun f ->
  f.Invoke(0) = "0"
)

(**

ここに出てくる `CoArb` というモジュールですが、「(引数候補である)関数の引数候補」を表すもので、Arbitrary と同じような雰囲気で使えばいいと思います。

また、コレクション型を引数候補にする際、空であったり null であったりしてほしくない場合があると思いますが、それも柔軟に指定可能です：

*)

// 空でないリスト
let ``non empty list`` = Prop.forAll(Arb.nonEmpty(Arb.list Arb.int), Arb.int)(fun xs i ->
  List.head xs = i
)

// null でない配列
let ``non null array`` = Prop.forAll(Arb.nonNull(Arb.array Arb.string))(fun xs ->
  Array.length xs >= 0
)

(**

## Arbtrary と Gen

Persimmon.Dried を使っていく中で、もうちょっときめ細かく引数候補を生成したくなる場面が出てくるかもしれません。その場合、既存の Arbitrary を少しカスタマイズした Arbitrary を利用することができます。

*)

// 最大で要素が3つの int list を生成する
let arbLs = {
  Gen = Gen.listOfMaxLength 3 <| Arb.int.Gen
  Shrinker = Shrink.shrinkList <| Arb.int.Shrinker
  PrettyPrinter = Pretty.prettyList
}

let ``list of max length 3`` = Prop.forAll(arbLs)(fun xs ->
  List.length xs <= 3
)

(**

基本的な Arbitrary は `Gen`、`Shrinker`、`PrettyPrinter` の3つの値(ラベル)を持つレコード型です。このうち `Gen` : `Gen<'T>` が値の生成を担っている箇所なので、ここをうまく調節することで期待する結果を導きやすくなります。

Gen モジュールには、Gen<’T> 型の値を扱う便利な関数がたくさん定義されています。以下に一例を挙げてみます。

### oneOf

`oneOf` 関数は、*Gen<'T>* 型のシーケンスを受け取り、そのうちのいずれかの *Gen<'T>* の要素を返します。

*)

// 指定した Gen シーケンスについて、要素のうち1つを使う
let arbOneOf = {
  Arb.int with Gen = Gen.oneOf <| List.map Gen.constant [2; 3; 5; 7]
}

let ``get one of prime numbers`` = Prop.forAll(arbOneOf)(fun i ->
  let contains v = List.exists (fun x -> x = v)
  contains i [1 .. 10]    // [2; 3; 5; 7] ⊂ [1 .. 10]
)

(**

### suchThat

`suchThat` 関数は、ある *Gen<'T>* 型の値に対し、指定した条件にあう結果を生成するような *Gen<'T>* を返します。

*)

// 条件にマッチする要素だけ生成する
let arbEven = {
  Arb.int with Gen = Gen.suchThat (fun i -> i % 2 = 0) Arb.int.Gen  // filter only even number
}

let ``only even number`` = Prop.forAll(arbEven)(fun i ->
  (i * i) % 4 = 0
)

(**

### listOfLength

`listOfLength` 関数は、引数に長さと *Gen<'T>* 型の値を受け取り、指定した長さである *'T* 型のリストを生成する *Gen<'T list>* 型の値を返します。

*)

// 要素が3つの int list を生成する
let arbList3 = {
  Gen = Gen.listOfLength 3 <| Arb.int.Gen
  Shrinker = Shrink.shrinkList <| Arb.int.Shrinker
  PrettyPrinter = Pretty.prettyList
}

// 要素が5つの int list を生成する
let arbList5 = {
  arbList3 with Gen = Gen.listOfLength 5 <| Arb.int.Gen
}

let ``list of length`` = Prop.forAll(arbList3, arbList5)(fun xs3 xs5 ->
  List.length xs3 < List.length xs5
)

(**

類似の関数に、指定した長さ以下のリストを作る `listOfMaxLength`、指定した長さ以上のリストを作る `listOfMinLength` などがあります。

他にも有用な関数がありますので、Gen モジュールの中を Intellisense で眺めてみたり、ソースコードを紐解いてみることをオススメします。

## 様々な性質の書き方

`Prop` モジュールには、性質を書く上で便利な関数や演算子が定義されています。

### And / Or 演算子

And 演算子 `.&.` は、両辺がともに成り立つ場合に全体として性質が成り立ちます。

*)

// lhs .&. lazy rhs で、lhs、rhs ともに成り立つ場合に成り立つ
let ``and operator`` = Prop.forAll(Arb.int, Arb.int)(fun x y ->
  x + 1 > x |@ "x + 1 is greater than x" .&.
  lazy (y - 1 < y |@ "y - 1 is less than y") .&.
  lazy (x * y > x + y |@ "x * y is greather than x + y")  // This property may fail
)

(**

一方、Or 演算子 `.|.` は、両辺のいずれかが成り立つ場合に全体として性質が成り立ちます。

*)

// lhs .|. lazy rhs で、lhs、rhs いずれか成り立つ場合に成り立つ
let ``or operator`` = Prop.forAll(Arb.int, Arb.int)(fun x y ->
  "x + 1 is greater than x" @| (x + 1 > x) .|.
  lazy ("y - 1 is less than y" @| (y - 1 < y)) .|.
  lazy ("x * y is greather than x + y" @| (x * y > x + y))  // This property may fail
)

(**

上記2つの性質を実行すると、前者の *and operator* は失敗しますが、後者の *or operator* は成功します。

なお、ここで使った `|@` および `@|` はラベル演算子で、それぞれ `p |@ s`、`s @| p` のように使い、性質 *p* にラベル *s* を付けることができます。

### 条件付き性質

`==>` 演算子は、左辺の性質が成り立つ場合にのみ、右辺の性質を確認します。

*)

// lhs ==> lazy rhs で、lhs が成り立つ場合に rhs を確認する
let ``conditional property`` = Prop.forAll(Arb.int, Arb.list Arb.int)(fun i ls ->
  let ils = List.Cons(i, ls)
  List.length ls > 5  ==> lazy (List.length ils > 6)
)

(**

上記例の場合、入力されたリスト *ls* の長さが5よりも大きい場合に、*i :: ls* の長さが6より大きいか確認します。

前述の `.&.`、`.|.` 演算子も同様に、両側に性質を取るような演算子では、右辺の性質は `lazy` で包んだ値でなければなりません。これは F# が正格評価を主とする言語であることに伴う制約です。右辺の性質を評価した場合に例外が発生するようなケースでは、演算子に両辺の性質を渡すタイミングで例外が発生し、失敗扱いとなってしまうからです。

```fsharp
// 仮にこう書けていたとしたら
let ``right hand side executes... what?`` = Prop.forAll(Arb.int)(fun i ->
  (i + 1 = i) ==> (i / 0 = 0)  // どうなるか分かりますか？
)
```

### 分類する

`classify` 関数は、性質に名前を付けられる点ではラベル演算子のようですが、さらに任意の条件で分類することができます。

*)

let ``classifying test case`` = Prop.forAll(Arb.int, Arb.int)(fun x y ->
  x + y > x
  |> Prop.classify(x < 0, "x is negative")
  |> Prop.classify((x = 0), "x is zero")
  |> Prop.classify(y < 0, "y is negative")
  |> Prop.classify((y = 0), "y is zero")
)

(**

これを実行すると、次のように表示されるかもしれません：

*)

(*** define-output: classify ***)
Runner.run "" prms ``classifying test case``

(*** include-output: classify ***)

(**

ここから、テストに失敗した時の引数の分布が分かります。

</div>
*)
