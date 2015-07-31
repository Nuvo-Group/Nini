module ``sample program``

// These tests simulate a sample application that allows for
// two different commands, and a set of flags and values.

// The command is optional, the output path is required, and
// input files (at least) one is required as well. Both the
// programs (and the default program which is what runs if
// you don't spesify a command) have this in common. The two
// commands also have some separate flags that are different
// between them.

// In an actual application, you would probably want to have the
// two command modules "Compile" and "Run" in separate files.

open Argonaut

type ProgramArguments = {
  output: string
  files: string list }
  
  with 
    static member internal empty = { output = ""; files = [] }

module Compile =
  // This is the first "command" module. You get it by running
  // compile [files] [arguments].

  let usage = "Compiles the files."

  type CompileArguments = {
    output: string
    files: string list
    binary: bool
    format: string option }

    with 
      static member internal from (args: ProgramArguments) = { 
        output = args.output
        files = args.files
        binary = false
        format = None }

  type CompileArgument =
  | [<Shorthand("b")>] Binary
  | [<Shorthand("f")>] Format of string

    interface IArgumentTemplate with
      member arg.Usage =
        match arg with
        | Binary -> "Compile to binary, instead of something else."
        | Format _ -> "Use the spesified format on the output."

  let processArgument args = function
    | Binary -> { args with binary = true }
    | Format f -> { args with format = Some f }

  let run args =
    OptionsParser.runList processArgument (CompileArguments.from args) (fun args ->
      // Here you have your program arguments, and can run your program

      args)

module Run =
  // This is the second "command" module. You get it by running
  // run [files] [arguments]

  let usage = "Run the files."

  type RunArguments = {
    output: string
    files: string list
    saveFile: string }

    with
      static member internal from (args: ProgramArguments) = {
        output = args.output
        files = args.files
        saveFile = "" }

  type RunArgument =
  | [<Mandatory; Shorthand("s")>] SaveFile of string

    interface IArgumentTemplate with
      member arg.Usage =
        match arg with
        | SaveFile _ -> "Path to save file."

  let processArgument args = function
    | SaveFile s -> { args with saveFile = s }

  let run args =
    OptionsParser.runList processArgument (RunArguments.from args) (fun args ->
      // Here you have your program arguments, and can run your code
      
      args)

type Command =
| Compile of Compile.CompileArgument list
| Run of Run.RunArgument list

type ApplicationArgument =
| [<Command>] Command of Command
| [<Mandatory; Shorthand("o")>] Output of string
| [<Mandatory; Rest>] Files of string list

  interface IArgumentTemplate with
    member arg.Usage =
      match arg with
      | Command (Compile _) -> Compile.usage
      | Command (Run _) -> Run.usage
      | Output _ -> "Output path."
      | Files _ -> "Input files."

let processArgument args = function
  | Output s -> { args with output = s }
  | Files f -> { args with files = f }
  | _ as argument-> invalidArg "argument" (sprintf "%+A is not a valid argument" argument)

let run args = function
  | Some (Compile a) -> Compile.run args a :> obj
  | Some (Run a) -> Run.run args a :> obj
  | None -> args :> obj

// Sample main function
// normally though, this would
// process the result, and either
// return 0, or print the error
// and return non-0.
let main args = 
  args |> OptionsParser.runCommand processArgument ProgramArguments.empty run


// *********************************************
//
//   Test code below
// *********************************************
open Xunit
open Compile
open Run

let testSuccess expected args =
  let result = main args

  Assert.Equal (expected, result)

[<Fact>]
let ``test successful inputs`` () =
  // No commands
  [| "input.txt"; "-o"; "output.txt" |] |> testSuccess { ProgramArguments.files = ["input.txt"]; output = "output.txt" }
  [| "input.txt"; "-o"; "output.txt"; "input2.txt" |] |> testSuccess { ProgramArguments.files = ["input.txt"; "input2.txt"]; output = "output.txt" }
  [| "input.txt"; "input2.txt"; "-o"; "output.txt" |] |> testSuccess { ProgramArguments.files = ["input.txt"; "input2.txt"]; output = "output.txt" }
  [| "-o"; "output.txt"; "input.txt"; "input2.txt" |] |> testSuccess { ProgramArguments.files = ["input.txt"; "input2.txt"]; output = "output.txt" }

  // Compile command
  [| "compile"; "input.txt"; "-o"; "output.txt" |] |> testSuccess { CompileArguments.files = ["input.txt"]; output = "output.txt"; binary = false; format = None }
  [| "compile"; "input.txt"; "-o"; "output.txt"; "input2.txt" |] |> testSuccess { CompileArguments.files = ["input.txt"; "input2.txt"]; output = "output.txt"; binary = false; format = None }
  [| "compile"; "input.txt"; "input2.txt"; "-o"; "output.txt" |] |> testSuccess { CompileArguments.files = ["input.txt"; "input2.txt"]; output = "output.txt"; binary = false; format = None }
  [| "compile"; "-o"; "output.txt"; "input.txt"; "input2.txt" |] |> testSuccess { CompileArguments.files = ["input.txt"; "input2.txt"]; output = "output.txt"; binary = false; format = None }
  [| "compile"; "-o"; "output.txt"; "-b"; "input.txt"; "input2.txt" |] |> testSuccess { CompileArguments.files = ["input.txt"; "input2.txt"]; output = "output.txt"; binary = true; format = None }
  [| "compile"; "-o"; "output.txt"; "input.txt"; "-b"; "input2.txt" |] |> testSuccess { CompileArguments.files = ["input.txt"; "input2.txt"]; output = "output.txt"; binary = true; format = None }
  [| "compile"; "-o"; "output.txt"; "-f"; "foo"; "input.txt"; "-b"; "input2.txt" |] |> testSuccess { CompileArguments.files = ["input.txt"; "input2.txt"]; output = "output.txt"; binary = true; format = Some "foo" }

  // Run command
  [| "run"; "input.txt"; "-o"; "output.txt"; "input2.txt"; "-s"; "savefile.txt" |] |> testSuccess { RunArguments.files = ["input.txt"; "input2.txt"]; output = "output.txt"; saveFile = "savefile.txt" }