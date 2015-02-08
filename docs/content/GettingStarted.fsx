(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#I "../../src/Persimmon.Dried/bin/Release"

(**
<div class="blog-post">

Getting Started
===============

An overview of Persimmon.Dried, how to download and use.

Getting Persimmon.Dried
-----------------------

Run the command below:

    [lang=powershell]
    .\.nuget\NuGet.exe Install Persimmon.Dried -OutputDirectory tools -ExcludeVersion

The first step
--------------

open an fsx file and start with:

*)

#r "Persimmon"
#r "Persimmon.Dried"

open Persimmon
open Persimmon.Dried

(**

You can write the properties by using `Prop.forAll`, `Arbitrary` instances and function:

*)

let ``reverse and reverse is original`` = Prop.forAll (Arb.list Arb.int) (fun xs ->
  List.rev (List.rev xs) = xs
)

(**

Checking property
-----------------

We load this definition in F# interactive and then invoke.

*)

let prms = { Runner.Parameters.Default with Callback = Runner.createConsoleReporter 1 }

(*** define-output: revRevIsOriginal ***)
Runner.run prms ``reverse and reverse is original`` 

(*** include-output: revRevIsOriginal ***)

(**

When a property fails, Persimmon.Dried displays a counterexample.
For example, You define

*)

let ``failure example`` = Prop.forAll (Arb.list Arb.int) (fun xs ->
  List.rev xs = xs
)

(**

and check property

*)

(*** define-output: failureExample ***)
Runner.run prms ``failure example`` 

(*** include-output: failureExample ***)

(**

Checking properties by using Persimmon.Console
----------------------------------------------

Now create a project and write the test by using `property` computation expression.

*)

let ``binding example`` = property "binding example" {
  apply (``reverse and reverse is original``)
}

(**

Run the command below:

    [lang=powershell]
    .\.nuget\NuGet.exe Install Persimmon.Console -OutputDirectory packages -ExcludeVersion
    .\packages\Persimmon.Console\tools\Persimmon.Console.exe 'input file path'

</div>
*)
