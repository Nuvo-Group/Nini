namespace HelloFSharp

open Microsoft.AspNet.Builder
open Nini.Types
open Nini.Http
open Nini.Http.Applicatives
open Nini.Http.Successful

type Startup() =
  let printPath (ctx: HttpContext) = async {
    printfn "Path: %s" (Path.value ctx.request.path)
    return Some ctx
  }

  let index = path "/" >>= ok "This is the index :)"
  let foobar = path "/foo/bar" >>= ok "This is the foobar"
  let get = GET >>= choose [index; foobar]
  
  let handle = 
    printPath >>= choose
      [ get
        RequestErrors.notfound "Resource not found..." ]

  member x.Configure (app: IApplicationBuilder) =
    app.UseNini handle |> ignore