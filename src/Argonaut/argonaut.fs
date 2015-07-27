module Argonaut.OptionsParser

open System
open System.Globalization

open Microsoft.FSharp.Reflection

[<NoComparison>]
type OptionsConfig<'Template when 'Template :> IArgumentTemplate> = private {
  culture: CultureInfo
  parsers: Type -> ArgParser option
  argInfo: ArgInfo.T list
  allowUnknown: bool }

let create<'Template when 'Template :> IArgumentTemplate> =
  //debug ()
  let t = typeof<'Template>
  let isUnion = FSharpType.IsUnion (t, true)

  if not isUnion then invalidArg typeof<'Template>.Name "UnionArgParser: template type inaccessible or not F# Discriminate Union."

  let culture = CultureInfo.InvariantCulture
  let parsers = Parsers.defaultParsers

  let argInfo =
    FSharpType.GetUnionCases (t, true)
    |> List.ofArray
    |> List.map ArgInfo.create

  let argInfo = ArgInfo.help :: argInfo

  { culture = culture
    parsers = parsers
    argInfo = argInfo
    allowUnknown = false }

let run (conf: OptionsConfig<'a>) (args: string array): Result<'a list> =
  let args = args |> List.ofArray

  let rec resolveCommands args commands involved current =
    match List.tryFind ArgInfo.isCommand current, args with
    | Some command, first :: rest ->
      // Potentially, there is a command in the first slot
      match ArgInfo.getCommand first command with
      | Some command ->
        // We have a command match
        resolveCommands rest (command :: commands) (current @ involved) (ArgInfo.children command)
      | None ->
        // We did not match a command
        commands, (current @ involved), args
    | _ ->
      // Here as well, no command match
      commands, (current @ involved), args

  debug ()
  let commands, involved, args = resolveCommands args [] [] conf.argInfo
  let involved = involved |> List.filter (ArgInfo.isCommand >> not)
  let restArg = List.tryFind ArgInfo.isRest involved
  let hasRest =
    match restArg with
    | Some arg -> true
    | None -> false

  let isArg (s: string) = s.StartsWith ("--", true, conf.culture) && s.Length > 2
  let isShort (s: string) = s.StartsWith ("-", true, conf.culture) && s.Length > 1
  let split (s: string) = System.Text.RegularExpressions.Regex.Split (s, String.Empty) |> List.ofArray

  let rec runSwitches switches acc =
    match switches with
    | [] -> Some acc
    | h :: t ->
      match involved |> List.tryFind (ArgInfo.short h) with
      | Some info ->
        // TODO: parse value
        failwith "not implemented"
      | None -> None

  let rec run' args acc restAcc =
    match args with
    | [] -> Success (List.rev acc, List.rev restAcc)
    | h :: t when isArg h ->
      // h is here an argument (like --help)
      let name = h.Substring 2
      match involved |> List.tryFind (ArgInfo.named name) with
      | Some info ->
        // TODO: parse value
        failwith "not implemented"
      | None when conf.allowUnknown ->
        run' t acc (h :: restAcc)
      | None ->
        Error (sprintf "Unknown argument %s" h, "")
    | h :: t when isShort h ->
      // h is here a short form (like -h)
      // It's allowed to combine short form arguments
      // like `ls -la` instead of `ls -l -a`, but
      // only if all of them are flags arguments.
      let short = h.Substring 1
      match short.Length with
      | 1 ->
        // single character, process as usual
        match involved |> List.tryFind (ArgInfo.short short) with
        | Some info ->
          // TODO: Parse value
          failwith "not implemented"
        | None when conf.allowUnknown ->
          run' t acc (h :: restAcc)
        | None ->
          Error (sprintf "Unknown argument %s" h, "")
      | _ ->
        // longer chain, for switches only
        match runSwitches (split short) acc with
        | Some acc -> run' t acc restAcc
        | None when conf.allowUnknown ->
          run' t acc (h :: restAcc)
        | None ->
          Error (sprintf "Unknown argument %s" h, "")
    | h :: t -> run' t acc (h :: restAcc)
    
  
  let result = run' args [] []
  match result with
  | Success (args, rest) ->
    // TODO: Implement
    failwith "not implemented"
  | Error (error, help) -> Error (error, help)
  | Help h -> Help h

let parse<'Template, 'Result when 'Template :> IArgumentTemplate> (fold: 'Result -> 'Template -> 'Result) (initial: 'Result): Result<'Result> =
  failwith "not implemented"