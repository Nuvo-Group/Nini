namespace Nini.Templ

open System
open System.Globalization
open System.Reflection

open Microsoft.FSharp.Reflection

open Argonaut

type EchoArguments =
| [<Rest; Mandatory>] Files of string list
| [<Shorthand("v")>] Verbose
| [<Shorthand("o")>] Output of string

  interface IArgumentTemplate with
    member x.Usage =
      match x with
      | Files _ -> "Files to parse"

type Command =
| Echo of EchoArguments list
| Test of EchoArguments list

type OuterArguments =
| [<Command>] Command of Command

  interface IArgumentTemplate with
    member x.Usage =
      match x with
      | Command (Echo _) -> "Echo blah... description"
      | Command (Test _) -> "Test description"

type OuterCommand =
| Test of OuterArguments list

type Arguments =
| [<Command>] Command of OuterCommand

  interface IArgumentTemplate with
    member x.Usage =
      match x with
      | Command (Test _) -> "Test description"

type Program () =
  member x.Main (args: string array) =
    //let app = OptionsParser.create<Arguments>
    //[| "test"; "echo"; "-vvv"; "-o"; "test.html"; "test.templ" |] |> OptionsParser.run app |> printfn "%+A"
    //printfn "Templar running: %+A" app
    0