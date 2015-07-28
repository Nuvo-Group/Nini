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

module internal Internal =
  let makeResult t arguments commands =
    debug ()
    let grouped =
      arguments
      |> List.groupBy (fun i -> (i.GetType ()).BaseType)
      |> dict

    let bundled =
      commands
      |> List.zmap ArgInfo.commandType
      |> List.map (fun ((a, c), t) -> 
        match grouped.TryGetValue t with
        | true, g -> g, c, a
        | _ -> [], c, a)
      |> List.fold (fun inner (values, command, argument) ->
        let values =
          match inner with | Some v -> v :: values | None -> values
        let arg = ArgInfo.commandValue values command argument
        Some arg) None

    let outer =
      match grouped.TryGetValue t with
      | true, values -> values
      | false, _ -> []

    let result =
      match bundled with
      | Some command -> command :: outer
      | None -> outer

    List.dynamicCast t result

open Internal

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
        let (_, cmd) = command
        resolveCommands rest (command :: commands) (current @ involved) (ArgInfo.children cmd)
      | None ->
        // We did not match a command
        commands, (current @ involved), args
    | _ ->
      // Here as well, no command match
      commands, (current @ involved), args

  let commands, involved, args = resolveCommands args [] [] conf.argInfo
  let involved = involved |> List.filter (ArgInfo.isCommand >> not)
  let restArg = List.tryFind ArgInfo.isRest involved
  let hasRest =
    match restArg with
    | Some arg -> true
    | None -> false

  let isArg (s: string) = s.StartsWith ("--", true, conf.culture) && s.Length > 2
  let isShort (s: string) = s.StartsWith ("-", true, conf.culture) && s.Length > 1
  let split (s: string) = 
    // TODO: There has to be a better way...
    System.Text.RegularExpressions.Regex.Split (s, String.Empty) 
    |> List.ofArray
    |> List.filter (fun s -> not (String.IsNullOrEmpty s))

  let (&>>) l r v = l v && r v

  let rec runSwitches switches acc =
    match switches with
    | [] -> Some acc
    | h :: t ->
      match involved |> List.tryFind (ArgInfo.short h &>> ArgInfo.isFlag) with
      | Some info ->
        runSwitches t (ArgInfo.flagValue info :: acc)
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
        parse info t acc restAcc
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
          parse info t acc restAcc
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
  
  and parse info args acc restAcc =
    match ArgInfo.parseValue conf.parsers conf.culture args info with
    | Success (args, value) -> run' args (value :: acc) restAcc
    | Error (error, _) -> Error (error, "")
    | Help _ -> failwith "Should not happen"
  
  let result = run' args [] []
  match result with
  | Success (args, rest) ->
    let restArg, error =
      match rest, restArg with
      | [], None -> None, None
      | rest, Some restArg ->
        Some (ArgInfo.restValue rest restArg), None
      | rest, None -> None, Some (Error (sprintf "Unknown argument %s" (List.head rest), ""))

    match error with
    | Some error -> error
    | None ->
      let result = makeResult typeof<'a> (args @ ([restArg] |> List.choose id)) commands :?> 'a list
      Success result
  | Error (error, help) -> Error (error, help)
  | Help h -> Help h

let parse<'Template, 'Result when 'Template :> IArgumentTemplate> (fold: 'Result -> 'Template -> 'Result) (initial: 'Result): Result<'Result> =
  failwith "not implemented"