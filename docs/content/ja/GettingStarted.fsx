(*** hide ***)
// このコードブロックは生成された HTML ドキュメントでは省略されます。ドキュメントで
// 見せたくない補助的なものを定義するために使います。
#I "../../../src/Persimmon.Dried/bin/Release"

(**
<div class="blog-post">

はじめに
========

Persimmon.Dried の概要、ダウンロードや使い方について。

Persimmon.Dried の入手
----------------------

以下のコマンドを実行しましょう。

    [lang=powershell]
    .\.nuget\NuGet.exe Install Persimmon.Dried -OutputDirectory tools -ExcludeVersion

はじめの一歩
------------

fsx ファイルを開き、次のように始めます。

*)

#r "Persimmon"
#r "Persimmon.Dried"

open Persimmon
open Persimmon.Dried

(**

`Prop.forAll` 関数、 `Arbitrary` インスタンス、関数を使って性質を書けます。

*)

let ``reverse and reverse is original`` = Prop.forAll (Arb.list Arb.int) (fun xs ->
  List.rev (List.rev xs) = xs
)

(**

性質の確認
----------

F# インタラクティブ上にこの定義をロードして、起動してみましょう。

*)

let prms = { Runner.Parameters.Default with Callback = Runner.createConsoleReporter 1 }

(*** define-output: revRevIsOriginal ***)
Runner.run prms ``reverse and reverse is original`` 

(*** include-output: revRevIsOriginal ***)

(**

性質が失敗したとき、 Persimmon.Dried は判例を表示します。
例えば, 

*)

let ``failure example`` = Prop.forAll (Arb.list Arb.int) (fun xs ->
  List.rev xs = xs
)

(**

を定義し、性質を確認してみましょう。

*)

(*** define-output: failureExample ***)
Runner.run prms ``failure example`` 

(*** include-output: failureExample ***)

(**

Persimmon.Console を使って性質を確認する
----------------------------------------

プロジェクトを作成し、`property` コンピュテーション式を使ってテストを書きます。

*)

let ``binding example`` = property "binding example" {
  apply (``reverse and reverse is original``)
}

(**

以下のコマンドを実行しましょう。

    [lang=powershell]
    .\.nuget\NuGet.exe Install Persimmon.Console -OutputDirectory packages -ExcludeVersion
    .\packages\Persimmon.Console\tools\Persimmon.Console.exe 'input file path'

</div>
*)