let referenceBinaries = [ "Persimmon.Dried.dll" ]
let website = "/Persimmon.Dried"
let githubLink = "https://github.com/persimmon-projects/Persimmon.Dried"
let info =
  [ "project-name", "Persimmon.Dried"
    "project-author", "persimmon-projects"
    "project-summary", "Random testing for Persimmon."
    "project-github", githubLink
    "project-nuget", "https://www.nuget.org/packages/Persimmon.Dried/"]

#I "../../packages/FSharp.Formatting/lib/net40"
#I "../../packages/FSharp.Compiler.Service/lib/net45"
#I "../../packages/FSharpVSPowerTools.Core/lib/net45"
#r "FSharpVSPowerTools.Core.dll"
#r "FSharp.Compiler.Service.dll"
#r "FSharp.Literate.dll"
#r "FSharp.CodeFormat.dll"
#r "FSharp.MetadataFormat.dll"
#r "FSharp.MarkDown.dll"
open System.IO
open FSharp.Literate
open FSharp.MetadataFormat

let (@@) path1 path2 = Path.Combine(path1, path2)

#if Release
let root = website
let configuration = "Release"
#else
let root = "file://" + (__SOURCE_DIRECTORY__ @@ "../output")
let configuration = "Debug"
#endif

let bin = __SOURCE_DIRECTORY__ @@ "../../src/Persimmon.Dried/bin" @@ configuration
let content = __SOURCE_DIRECTORY__ @@ "../content"
let output = __SOURCE_DIRECTORY__ @@ "../output"
let files = __SOURCE_DIRECTORY__ @@ "../files"
let templates = __SOURCE_DIRECTORY__ @@ "templates"
let formatting = __SOURCE_DIRECTORY__ @@ "../../packages/FSharp.Formatting/"
let docTemplate = formatting @@ "templates/docpage.cshtml"

let layoutRoots = [
  templates
  formatting @@ "templates"
  formatting @@ "templates/reference"
]

let buildReference () =
  let libDirs = [
    Path.GetFullPath(__SOURCE_DIRECTORY__ @@ "../../packages/Persimmon.1.0.0-beta5/lib/net20/")
    Path.GetFullPath(__SOURCE_DIRECTORY__ @@ "../../packages/FsRandom.1.3.3/lib/net40/")
  ]
  let outputDir = output @@ "reference"
  System.IO.Directory.CreateDirectory(outputDir) |> ignore
  for lib in referenceBinaries do
    MetadataFormat.Generate(
      __SOURCE_DIRECTORY__ @@  "../../src" @@ Path.GetFileNameWithoutExtension(lib) @@ "bin" @@ configuration @@ lib,
      outputDir,
      layoutRoots,
      parameters = ("root", root)::info,
      libDirs = libDirs,
      sourceRepo = githubLink @@ "tree/master",
      sourceFolder = __SOURCE_DIRECTORY__ @@ ".." @@ "..",
      publicOnly = true)

let buildDocumentation () =
  let subdirs = Directory.EnumerateDirectories(content, "*", SearchOption.AllDirectories)
  for dir in Seq.append [content] subdirs do
    let sub = if dir.Length > content.Length then dir.Substring(content.Length + 1) else "."
    Literate.ProcessDirectory
      (dir, docTemplate, output @@ sub, replacements = ("root", root)::info,
        layoutRoots = layoutRoots,  fsiEvaluator = new FsiEvaluator(), lineNumbers=false)

buildDocumentation ()
buildReference ()
