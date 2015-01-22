namespace Persimmon.Dried

[<Sealed>]
type Pretty(p: PrettyParameters -> string) =
  member __.Apply(prms) = p prms
  member this.Map(f: string -> string) = Pretty(fun prms -> f (p prms))
  member this.Bind(f: string -> Pretty) = Pretty(fun prms -> (f (p prms)).Apply(prms))

and PrettyParameters = {
  Verbosity: int
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Pretty =

  open System
  open System.Text

  module internal Helper =

    let newLine = Environment.NewLine

    let (-/) s1 s2 = if s2 = "" then s1 else s1+ newLine + s2

  module Parameters =

    let Default = { Verbosity = 0 }

  open Helper

  let inline map f (p: Pretty) = p.Map(f)
  let inline bind f (p: Pretty) = p.Bind(f)

  let pretty prms (p: Pretty) = p.Apply(prms)

  let pad s c length =
    if String.length s >= length then s
    else s + String(c, length - String.length s)

  let rec pbreak(s: string) lead length =
    if String.length s <= length then s
    else s.Substring(0, length) -/ pbreak (lead + s.Substring(length)) lead length

  let format (s: string) lead trail width =
    s.Split([|"\r\n";"\r";"\n"|], StringSplitOptions.None)
    |> Array.map (fun l -> pbreak (lead + l + trail) "  " width)
    |> String.concat newLine

  let private escapeControlChars (s: string) =
    let builder = StringBuilder()
    let rec loop i =
      if i < s.Length then
        if  Char.IsSurrogatePair(s, i) then
          let c = Char.ConvertToUtf32(s, i)
          builder.Append(sprintf "\\u%04x" c) |> ignore
        else
          builder.Append(s.Chars(i)) |> ignore
        loop (i + 1)
    loop 0
    builder.ToString()

  let prettyAny t = Pretty(fun _ -> sprintf "%A" t)

  let prettyString t = Pretty(fun _ -> "\""+ escapeControlChars t + "\"")

  let prettyList l = Pretty(fun _ -> (l |> List.fold (sprintf "%s \"%A\"; ") "[") + "]")

  let prettyExn (e: exn) = Pretty(fun prms ->
    let strs = e.ToString().Split([|"\r\n";"\r";"\n"|], StringSplitOptions.None)
    if prms.Verbosity <= 0 then [||]
    elif prms.Verbosity <= 1 then strs |> Seq.take 5 |> Seq.toArray
    else strs
    |> String.concat newLine)

  let prettyTime millis =
    let min = millis / (60L * 1000L)
    let sec = float (millis - (60L * 1000L * min)) / 1000.0
    if min <= 0L then sprintf "%.3f sec " sec
    else sprintf "%d min %.3f sec " min sec
