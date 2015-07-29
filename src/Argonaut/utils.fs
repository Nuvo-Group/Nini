[<AutoOpen>]
module internal Argonaut.Utils

open System

open FSharp.Reflection

let inline debug () =
  match System.Diagnostics.Debugger.IsAttached with
  | false -> System.Diagnostics.Debugger.Launch () |> ignore
  | true -> System.Diagnostics.Debugger.Break ()

module List =
  let private listType = (typeof<List<obj>>).GetGenericTypeDefinition ()

  let zmap fn list = List.map (fun i -> i, fn i) list
  let apply3 fn list = List.map (fun (a, b, c) -> fn a b c) list

  let dynamicCast t =
    let listType = listType.MakeGenericType [| t |]
    let cases = FSharpType.GetUnionCases (listType, true)
    let consCase = cases |> Array.find (fun c -> c.Name = "Cons")
    let emptyCase = cases |> Array.find (fun c -> c.Name = "Empty")

    let empty = FSharpValue.PreComputeUnionConstructor (emptyCase, true) [||]
    let cons = FSharpValue.PreComputeUnionConstructor (consCase, true)

    fun list ->
      list |> List.fold (fun list item -> cons [| item; list |]) empty

let private colorprint (fc, bc) (s: string) =
  let foreground, background = Console.ForegroundColor, Console.BackgroundColor
  try
    Console.ForegroundColor <- fc
    Console.BackgroundColor <- bc
    Console.Write s
  finally
    Console.ForegroundColor <- foreground
    Console.BackgroundColor <- background

let cprintf c fmt =
  Printf.kprintf (colorprint c) fmt

let cprintfn c fmt =
  Printf.kprintf ((fun s -> sprintf "%s%s" s Environment.NewLine) >> colorprint c) fmt