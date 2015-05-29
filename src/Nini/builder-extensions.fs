[<AutoOpen>]
module Microsoft.AspNet.Builder.BuilderExtensions

type Microsoft.AspNet.Builder.IApplicationBuilder with
  member builder.UseNini (app: Nini.Types.WebPart) =
    builder.UseMiddleware<Nini.Middleware.Middleware>([| app :> obj |])