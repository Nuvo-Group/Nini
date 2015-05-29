module Nini.Middleware

open Microsoft.AspNet.Builder
open Microsoft.AspNet.Http
open Microsoft.Framework.Logging

type AspContext = HttpContext
type AspRequest = HttpRequest
type AspConnection = ConnectionInfo

open Nini.Types
open Nini.Http

let toConnection token (c: AspConnection) = async {
  let! cert = c.GetClientCertificateAsync token |> Async.AwaitTask
  let cert = match cert with | null -> None | _ as c -> Some c
  
  return { clientCertificate = cert
           isLocal           = c.IsLocal
           localIp           = c.LocalIpAddress
           localPort         = c.LocalPort
           remoteIp          = c.RemoteIpAddress
           remotePort        = c.RemotePort }
}

let toPath = Path.fromPathString

let toHeaders (h: IHeaderDictionary) =
  h |> Seq.collect (fun kvp -> kvp.Value |> Seq.map (fun v -> kvp.Key, v))
  |> List.ofSeq

let toRequest (r: AspRequest) =
  { protocol   = r.Protocol
    path       = r.Path |> toPath
    httpMethod = r.Method |> HttpMethod.parse
    headers    = r.Headers |> toHeaders
    body       = r.Body
    isHttps    = r.IsHttps }

let toContext (r: AspContext) = async {
  let! connection = r.Connection |> toConnection r.RequestAborted
  let request = r.Request |> toRequest
  let response = HttpResponse.empty
  
  return { connection = connection
           request    = request
           response   = response }
}

let awaitUnit (task: System.Threading.Tasks.Task) = async { 
  let! _ = task.ContinueWith (fun _ -> 0) |> Async.AwaitTask
  return ()
}

type Middleware (next: RequestDelegate, loggerFactory: ILoggerFactory, app: WebPart) =
  let logger = loggerFactory.CreateLogger<Middleware> ()

  member x.Invoke (context: AspContext) = 
    Async.StartAsTask (async {
      let! niniContext = context |> toContext

      let! result = app niniContext
      match result with
      | Some ctx ->
        // TODO: Do stuff with result
        let result = ctx.response
        context.Response.StatusCode <- result.status.code
        result.headers |> List.iter (fun (name, value) -> context.Response.Headers.Append (name, value))
        match result.content with
        | NullContent ->
          do! context.Response.Body.AsyncWrite (System.Text.Encoding.UTF8.GetBytes result.status.message)
        | Bytes b ->
          context.Response.ContentLength <- System.Nullable b.LongLength
          do! context.Response.Body.AsyncWrite b
        | Writer w ->
          let writer = fun (bytes, offset, count) -> context.Response.Body.AsyncWrite (bytes, offset, count)
          do! w writer
        return ()
      | None ->
        logger.LogVerbose "Request was not handled by Nini"
        do! next.Invoke context |> awaitUnit
    }, cancellationToken = context.RequestAborted) :> System.Threading.Tasks.Task