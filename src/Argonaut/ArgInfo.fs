module internal Argonaut.ArgInfo

open System
open System.Reflection
open System.Text.RegularExpressions

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Reflection.FSharpReflectionExtensions

type private UC =
  static member DefaultOf<'T> () = Unchecked.defaultof<'T>

let untypedDefault t =
  let m = typeof<UC>.GetMethod ("DefaultOf", BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic)
  let m = m.MakeGenericMethod [| t |]
  m.Invoke (null, [||])

let checkAttr (map: 'T -> 'a) (def: 'a) (uci: UnionCaseInfo) =
  let t = typeof<'T>
  let attrs = uci.GetCustomAttributes t |> Array.map (fun a -> a :?> 'T) |> List.ofArray
  match attrs with
  | [] -> def
  | [attr] -> map attr
  | _ -> failwithf "Multiple %s attributes found for case %s" t.Name uci.Name
   
let hasAttr t (uci: UnionCaseInfo) = 
  let attrs = uci.GetCustomAttributes t |> List.ofArray
  match attrs with
  | [] -> false
  | [_] -> true
  | _ -> failwith "Multiple %s attributes found for case %s" t.Name uci.Name

let regex = Regex "(?!^)([A-Z])"
let normalize name =
  let hyphenized = regex.Replace (name, (fun (m: Match) ->
    let group = m.Groups.[0]
    "-" + group.Value.ToLowerInvariant ()))
  hyphenized.ToLowerInvariant ()

let oneOrNone = function
  | [] -> None
  | [item] -> Some item
  | _ -> failwith "More than one item in list"

let getCommandType (t: Type) =
  let listType = typeof<obj list>.GetGenericTypeDefinition ()
  let isList = t.IsGenericType && t.GetGenericTypeDefinition () = listType
  if not isList then failwith "Type is not a list type"
  t.GetGenericArguments () |> Array.exactlyOne

let construct (case: UnionCaseInfo) =
  FSharpValue.PreComputeUnionConstructor (case, true)

let constructDefault (case: UnionCaseInfo) =
  let values = 
    case.GetFields ()
    |> Array.map (fun f -> f.PropertyType)
    |> Array.map untypedDefault

  construct case values

let getUsage (o: obj) =
  let templ = (o :?> IArgumentTemplate)
  templ.Usage


type T =
| Flag of Flag
| Argument of Argument
| Rest of Rest
| Commands of Commands

and [<NoComparison>] Flag = private {
  name: string
  short: string option
  value: obj }

and [<NoComparison>] Argument = private {
  name: string
  short: string option
  item: Type
  mandatory: bool
  value: obj -> obj }

and [<NoComparison>] Rest = private {
  mandatory: bool
  value: string list -> obj }

and [<NoComparison>] Commands = private {
  mandatory: bool
  commands: Command list
  value: obj list -> Command -> obj }

and Command = private {
  name: string
  usage: string
  children: T list
  argType: Type
  ctor: obj -> obj }

let help = Flag {
  name = "help"
  short = Some "h"
  value = None
}

let isCommand = function
  | Commands _ -> true
  | _ -> false

let isRest = function
  | Rest _ -> true
  | _ -> false

let isFlag = function
  | Flag _ -> true
  | _ -> false

let named name = function
  | Flag f -> f.name = name
  | Argument a -> a.name = name
  | _ -> false

let short short = function
  | Flag f -> f.short = Some short
  | Argument a -> a.short = Some short
  | _ -> false

let getCommand name = function
  | Commands c as arg ->
    match c.commands |> List.tryFind (fun c -> c.name = name) with
    | None -> None
    | Some co -> Some (arg, co)
  | _ -> None

let flagValue = function
  | Flag f -> f.value
  | _ -> failwith "Not a flag"

let restValue values = function
  | Rest r -> r.value values
  | _ -> failwith "Not a rest argument"

let commandValue values command = function
  | Commands c -> c.value values command
  | _ -> failwith "Not a command argument"

let commandType ((arg: T), (command: Command)) = 
  command.argType

let parseArgument (getParser: Type -> ArgParser option) culture args info =
  match getParser info.item with
  | Some parser ->
    match parser culture args with
    | Some (value, args) -> Success (args, info.value value)
    | None -> Error (sprintf "Could not parse %s as type %s" (List.head args) info.item.Name, "")
  | None -> Error (sprintf "No parser found for type %s" info.item.Name, "")

let parseValue getParser culture args = function
  | Flag f -> Success (args, f.value)
  | Argument a -> parseArgument getParser culture args a
  | _ -> failwith "Only applicable to flags and arguments"

let children command = command.children

let createCommandValue ctor (values: obj list) (command: Command) =
  let list = values |> List.dynamicCast command.argType
  let value = command.ctor list
  ctor [| value |]

let rec create (case: UnionCaseInfo) =
  let fields = case.GetFields () |> List.ofArray
  let t = fields |> List.map (fun p -> p.PropertyType) |> oneOrNone

  let caseCtor = construct case

  let command = hasAttr typeof<CommandAttribute> case
  let rest = hasAttr typeof<RestAttribute> case
  let mandatory = hasAttr typeof<MandatoryAttribute> case

  match command, rest with
  | true, false -> 
    // Command
    let t = Option.get t
    let cases = FSharpType.GetUnionCases (t, true) |> List.ofArray
    let commands =
      cases
      |> List.map (fun commandCase ->
        let argsType = 
          commandCase.GetFields () 
          |> Array.map (fun f -> f.PropertyType) 
          |> Array.exactlyOne 
          |> getCommandType

        let ctor = construct commandCase
        let name = checkAttr NameAttribute.name (normalize commandCase.Name) commandCase
        let usage = (constructDefault commandCase) |> Array.singleton |> caseCtor |> getUsage
        
        { name = name
          usage = usage
          children = FSharpType.GetUnionCases (argsType, true) |> List.ofArray |> List.map create
          argType = argsType
          ctor = fun v -> ctor [| v |] })

    Commands {
      mandatory = mandatory
      commands = commands
      value = createCommandValue caseCtor
    }

  | false, true ->
    if not (t = Some typeof<string list>) then failwith "Rest parameters must have type string list"

    // Rest parameter
    Rest {
      mandatory = mandatory
      value = (fun vals -> caseCtor [| vals :> obj |])
    }
  | false, false ->
    // Normal argument. Is either a flag (no value), or a argument (with value)
    let name = checkAttr NameAttribute.name (normalize case.Name) case
    let short = checkAttr (ShorthandAttribute.name >> Some) None case

    match t with
    | None ->
      // Flag
      Flag {
        name = name
        short = short
        value = caseCtor [||]
      }

    | Some t ->
      // Argument
      Argument {
        name = name
        short = short
        item = t
        mandatory = mandatory
        value = Array.singleton >> caseCtor
      }
      
  | true, true -> failwith "A argument cannot be both rest and command"