namespace Nini.Templ

open System
open System.Globalization
open System.Reflection

open Microsoft.FSharp.Reflection

open Argonaut

type EchoArguments =
| [<Rest; Mandatory>] Files of string list
| [<Shorthand("v")>] Verbose

  interface IArgumentTemplate with
    member x.Usage =
      match x with
      | Files _ -> "Files to parse"

type Command =
| Echo of EchoArguments list
| Test of EchoArguments list

type Arguments =
| [<Command>] Command of Command

  interface IArgumentTemplate with
    member x.Usage =
      match x with
      | Command (Echo _) -> "Echo blah... description"
      | Command (Test _) -> "Test description"

type Program () =
  member x.Main (args: string array) =
    let app = OptionsParser.create<Arguments>
    [| "echo"; "-v"; "test.templ" |] |> OptionsParser.run app
    printfn "Templar running: %+A" app
    0