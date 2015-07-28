[<AutoOpen>]
module Argonaut.Utils

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