module Nini.Types

open System
open System.Text
open System.IO
open System.Net
open System.Security.Cryptography.X509Certificates

[<NoComparison>]
type HttpConnection =
  { clientCertificate   : X509Certificate2 option
    isLocal             : bool
    localIp             : IPAddress
    localPort           : int
    remoteIp            : IPAddress
    remotePort          : int }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpConnection =
  let empty =
    { clientCertificate = None
      isLocal           = true
      localIp           = IPAddress.Loopback
      localPort         = 0
      remoteIp          = IPAddress.Loopback
      remotePort        = 0 }

/// <summary>
/// These are the known HTTP methods. See http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html
/// </summary>
[<RequireQualifiedAccess>]
type HttpMethod =
  | GET
  | POST
  | DELETE
  | PUT
  | HEAD
  | CONNECT
  | PATCH
  | TRACE
  | OPTIONS
  | OTHER of string // This represents a method string that isn't one of the standard methods.

  override x.ToString() =
    match x with
    | GET     -> "GET"
    | POST    -> "POST"
    | DELETE  -> "DELETE"
    | PUT     -> "PUT"
    | HEAD    -> "HEAD"
    | CONNECT -> "CONNECT"
    | PATCH   -> "PATCH"
    | TRACE   -> "TRACE"
    | OPTIONS -> "OPTIONS"
    | OTHER s -> s

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpMethod =
  let parse (str:string) =
    match str.ToUpperInvariant() with
      | "GET"     -> HttpMethod.GET
      | "POST"    -> HttpMethod.POST
      | "DELETE"  -> HttpMethod.DELETE
      | "PUT"     -> HttpMethod.PUT
      | "HEAD"    -> HttpMethod.HEAD
      | "CONNECT" -> HttpMethod.CONNECT
      | "PATCH"   -> HttpMethod.PATCH
      | "TRACE"   -> HttpMethod.TRACE
      | "OPTIONS" -> HttpMethod.OPTIONS
      | s         -> HttpMethod.OTHER s

type HttpCode =
    | HTTP_100 | HTTP_101
    | HTTP_200 | HTTP_201 | HTTP_202 | HTTP_203 | HTTP_204 | HTTP_205 | HTTP_206
    | HTTP_300 | HTTP_301 | HTTP_302 | HTTP_303 | HTTP_304 | HTTP_305 | HTTP_307
    | HTTP_400 | HTTP_401 | HTTP_402 | HTTP_403 | HTTP_404 | HTTP_405 | HTTP_406
    | HTTP_407 | HTTP_408 | HTTP_409 | HTTP_410 | HTTP_411 | HTTP_412 | HTTP_413
    | HTTP_422 | HTTP_428 | HTTP_429 | HTTP_414 | HTTP_415 | HTTP_416 | HTTP_417
    | HTTP_500 | HTTP_501 | HTTP_502 | HTTP_503 | HTTP_504 | HTTP_505


    member x.code = 
      match x with 
      | HTTP_100 -> 100 | HTTP_101 -> 101 | HTTP_200 -> 200 | HTTP_201 -> 201
      | HTTP_202 -> 202 | HTTP_203 -> 203 | HTTP_204 -> 204 | HTTP_205 -> 205
      | HTTP_206 -> 206 | HTTP_300 -> 300 | HTTP_301 -> 301 | HTTP_302 -> 302
      | HTTP_303 -> 303 | HTTP_304 -> 304 | HTTP_305 -> 305 | HTTP_307 -> 307
      | HTTP_400 -> 400 | HTTP_401 -> 401 | HTTP_402 -> 402 | HTTP_403 -> 403
      | HTTP_404 -> 404 | HTTP_405 -> 405 | HTTP_406 -> 406 | HTTP_407 -> 407
      | HTTP_408 -> 408 | HTTP_409 -> 409 | HTTP_410 -> 410 | HTTP_411 -> 411
      | HTTP_412 -> 412 | HTTP_413 -> 413 | HTTP_414 -> 414 | HTTP_415 -> 415
      | HTTP_416 -> 416 | HTTP_417 -> 417 | HTTP_422 -> 422 | HTTP_428 -> 428
      | HTTP_429 -> 429 | HTTP_500 -> 500 | HTTP_501 -> 501 | HTTP_502 -> 502
      | HTTP_503 -> 503 | HTTP_504 -> 504 | HTTP_505 -> 505

    member x.reason = 
      match x with
      | HTTP_100 -> "Continue"
      | HTTP_101 -> "Switching Protocols"
      | HTTP_200 -> "OK"
      | HTTP_201 -> "Created"
      | HTTP_202 -> "Accepted"
      | HTTP_203 -> "Non-Authoritative Information"
      | HTTP_204 -> "No Content"
      | HTTP_205 -> "Reset Content"
      | HTTP_206 -> "Partial Content"
      | HTTP_300 -> "Multiple Choices"
      | HTTP_301 -> "Moved Permanently"
      | HTTP_302 -> "Found"
      | HTTP_303 -> "See Other"
      | HTTP_304 -> "Not Modified"
      | HTTP_305 -> "Use Proxy"
      | HTTP_307 -> "Temporary Redirect"
      | HTTP_400 -> "Bad Request"
      | HTTP_401 -> "Unauthorized"
      | HTTP_402 -> "Payment Required"
      | HTTP_403 -> "Forbidden"
      | HTTP_404 -> "Not Found"
      | HTTP_405 -> "Method Not Allowed"
      | HTTP_406 -> "Not Acceptable"
      | HTTP_407 -> "Proxy Authentication Required"
      | HTTP_408 -> "Request Timeout"
      | HTTP_409 -> "Conflict"
      | HTTP_410 -> "Gone"
      | HTTP_411 -> "Length Required"
      | HTTP_412 -> "Precondition Failed"
      | HTTP_413 -> "Request Entity Too Large"
      | HTTP_414 -> "Request-URI Too Long"
      | HTTP_415 -> "Unsupported Media Type"
      | HTTP_416 -> "Requested Range Not Satisfiable"
      | HTTP_417 -> "Expectation Failed"
      | HTTP_422 -> "Unprocessable Entity"
      | HTTP_428 -> "Precondition Required"
      | HTTP_429 -> "Too Many Requests"
      | HTTP_500 -> "Internal Server Error"
      | HTTP_501 -> "Not Implemented"
      | HTTP_502 -> "Bad Gateway"
      | HTTP_503 -> "Service Unavailable"
      | HTTP_504 -> "Gateway Timeout"
      | HTTP_505 -> "HTTP Version Not Supported"

    member x.message = 
      match x with 
      | HTTP_100 -> "Request received, please continue"
      | HTTP_101 -> "Switching to new protocol; obey Upgrade header"
      | HTTP_200 -> "Request fulfilled, document follows"
      | HTTP_201 -> "Document created, URL follows"
      | HTTP_202 -> "Request accepted, processing continues off-line"
      | HTTP_203 -> "Request fulfilled from cache"
      | HTTP_204 -> "Request fulfilled, nothing follows"
      | HTTP_205 -> "Clear input form for further input."
      | HTTP_206 -> "Partial content follows."
      | HTTP_300 -> "Object has several resources -- see URI list"
      | HTTP_301 -> "Object moved permanently -- see URI list"
      | HTTP_302 -> "Object moved temporarily -- see URI list"
      | HTTP_303 -> "Object moved -- see Method and URL list"
      | HTTP_304 -> "Document has not changed since given time"
      | HTTP_305 -> "You must use proxy specified in Location to access this resource."
      | HTTP_307 -> "Object moved temporarily -- see URI list"
      | HTTP_400 -> "Bad request syntax or unsupported method"
      | HTTP_401 -> "No permission -- see authorization schemes"
      | HTTP_402 -> "No payment -- see charging schemes"
      | HTTP_403 -> "Request forbidden -- authorization will not help"
      | HTTP_404 -> "Nothing matches the given URI"
      | HTTP_405 -> "Specified method is invalid for this resource."
      | HTTP_406 -> "URI not available in preferred format."
      | HTTP_407 -> "You must authenticate with this proxy before proceeding."
      | HTTP_408 -> "Request timed out; try again later."
      | HTTP_409 -> "Request conflict."
      | HTTP_410 -> "URI no longer exists and has been permanently removed."
      | HTTP_411 -> "Client must specify Content-Length."
      | HTTP_412 -> "Precondition in headers is false."
      | HTTP_413 -> "Entity is too large."
      | HTTP_414 -> "URI is too long."
      | HTTP_415 -> "Entity body in unsupported format."
      | HTTP_416 -> "Cannot satisfy request range."
      | HTTP_417 -> "Expect condition could not be satisfied."
      | HTTP_422 -> "The entity sent to the server was invalid."
      | HTTP_428 -> "You should verify the server accepts the request before sending it."
      | HTTP_429 -> "Request rate too high, chill out please."
      | HTTP_500 -> "Server got itself in trouble"
      | HTTP_501 -> "Server does not support this operation"
      | HTTP_502 -> "Invalid responses from another server/proxy."
      | HTTP_503 -> "The server cannot process the request due to a high load"
      | HTTP_504 -> "The gateway server did not receive a timely response"
      | HTTP_505 -> "Cannot fulfill request."

    member x.Describe () =
      sprintf "%d %s: %s" x.code x.reason x.message

    static member TryParse (code: int) =
      HttpCodeStatics.mapCases.Force () |> Map.tryFind ("HTTP_" + string code)

and private HttpCodeStatics() =
  static member val mapCases : Lazy<Map<string,HttpCode>> =
    lazy
      Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<HttpCode>)
      |> Array.map (fun case -> case.Name, Microsoft.FSharp.Reflection.FSharpValue.MakeUnion(case, [||]) :?> HttpCode)
      |> Map.ofArray

/// HTTP cookie
type HttpCookie =
  { name      : string
    value     : string
    expires   : DateTimeOffset option
    path      : string option
    domain    : string option
    secure    : bool
    httpOnly  : bool }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpCookie =
  let name cookie = cookie.name
  let value cookie = cookie.value
  let expires cookie = cookie.expires
  let path cookie = cookie.path
  let domain cookie = cookie.domain
  let secure cookie = cookie.secure
  let httpOnly cookie = cookie.httpOnly

  let mk name value =
    { name = name;
      value = value;
      expires = None;
      path = Some "/";
      domain = None;
      secure = false;
      httpOnly = true; }

  let empty = mk "" ""

  /// Assumes only valid characters go in, see http://tools.ietf.org/html/rfc6265#section-4.1.1
  let toHeader (x: HttpCookie) =
    let app (sb : StringBuilder) (value : string) = sb.Append value |> ignore
    let sb = new StringBuilder(String.Concat [ x.name; "="; x.value ])
    let app value = app sb (String.Concat [";"; value])
    let appkv k f_map v = v |> Option.iter (fun v -> app (String.Concat [ k; "="; f_map v ]))
    x.domain  |> appkv "Domain" id
    x.path    |> appkv "Path" id
    x.expires |> appkv "Expires" (fun (i : DateTimeOffset) -> i.ToString("R"))
    if x.httpOnly then app "HttpOnly"
    if x.secure    then app "Secure"
    sb.ToString ()

/// A file's mime type and if compression is enabled or not
type MimeType =
  { name         : string
    compression  : bool }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MimeType =
  let name mime = mime.name
  let compression mime = mime.compression

type MimeTypesMap = string -> MimeType option

/// A holder for uploaded file meta-data
[<NoComparison>]
type HttpUpload =
  { fieldName    : string
    fileName     : string
    mimeType     : string
    body         : Stream }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpUpload =
  let fieldName upload = upload.fieldName
  let fileName upload = upload.fileName
  let mimeType upload = upload.mimeType
  let body upload = upload.body

[<NoComparison>]
type Path =
  private { path : Microsoft.AspNet.Http.PathString }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Path =
  let empty =
     { path = Microsoft.AspNet.Http.PathString "/" }

  let fromPathString p = { path = p }

  let value p = p.path.Value

  let equals s p = p.path.Equals (Microsoft.AspNet.Http.PathString s)

/// A holder for the data extracted from the request.
[<NoComparison>]
type HttpRequest =
  { protocol        : string
    path            : Path
    httpMethod      : HttpMethod
    headers         : (string * string) list
    body            : Stream
    isHttps         : bool }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpRequest =
  let empty =
    { protocol      = "HTTP/1.1"
      path          = Path.empty
      httpMethod    = HttpMethod.OTHER ""
      headers       = ["HOST", "localhost"]
      body          = new MemoryStream ()
      isHttps       = false }

type Writer = (byte[] * int * int) -> Async<unit>

[<NoComparison; NoEquality>]
type HttpContent =
  | NullContent
  | Bytes of data : byte array
  | Writer of write : (Writer -> Async<unit>)

/// The HttpResult is the structure that you work with to tell Nini how to
/// send the response. Have a look at the docs for HttpContent for further
/// details on what is possible.
[<NoComparison; NoEquality>]
type HttpResponse =
  { status        : HttpCode
    headers       : (string * string) list
    content       : HttpContent
    writePreamble : bool }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpResponse =
  let empty =
    { status        = HTTP_404
      headers       = []
      content       = HttpContent.NullContent
      writePreamble = true }

[<NoComparison; NoEquality>]
type HttpContext =
  { request      : HttpRequest
    response     : HttpResponse
    connection   : HttpConnection }

/// A WebPart is an asynchronous function that transforms the HttpContext.  An asynchronous return
/// value of None indicates 'did not handle'.
type WebPart = HttpContext -> Async<HttpContext option>

/// An error handler takes the exception, a programmer-provided message, a
/// request (that failed) and returns an asynchronous workflow for the handling
/// of the error.
type ErrorHandler = Exception -> string -> WebPart

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpContext =
  let request context = context.request
  let connection context = context.connection
  let response context = context.response

  let empty =
    { request    = HttpRequest.empty
      response   = HttpResponse.empty
      connection = HttpConnection.empty }