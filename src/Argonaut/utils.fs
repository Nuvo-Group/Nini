[<AutoOpen>]
module Argonaut.Utils

let inline debug () =
  match System.Diagnostics.Debugger.IsAttached with
  | false -> System.Diagnostics.Debugger.Launch () |> ignore
  | true -> System.Diagnostics.Debugger.Break ()