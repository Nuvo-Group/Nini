module Nini.Http

open Nini.Types

/// Return success with some value
let inline succeed x = async.Return (Some x)

/// Return failure without any value
let fail: Async<HttpContext option> = async.Return None

/// Return failure with a value that is ignored
let never: WebPart = fun _ -> fail


/// Compose (bind) two web parts; see (>>=) -- note the different parameter
/// ordering
let inline bind (second : 'b -> Async<'c option>) (first : 'a -> Async<'b option>) : 'a -> Async<'c option> =
  fun x -> async {
      let! e = first x
      match e with
      | None ->
        return None
      | Some t ->
        return! second t 
  }

/// Compose (bind) two arguments, 'first' and 'second', so that the result of
/// the composition can be applied to an argument of 'input' and then passed
/// to 'second', if 'first' yields a value.
let inline (>>=) (first : 'a -> Async<'b option>)  (second : 'b -> Async<'c option>) : 'a -> Async<'c option> =
  fun x ->
    bind second first x

/// Left-to-right Kleisli composition of monads.
let inline (>=>) a b = fun x ->
  match a x with
  | None   -> b x
  | r      -> r

/// Left-to-right Kleisli composition of web parts.
let inline (<|>) (a : WebPart) (b : WebPart) : WebPart =
  fun x -> async {
      let! e = a x
      match e with
      | None   ->
        let! result = b x
        match result with
        | None -> return None
        | r -> return r
      | r -> return r
    }

/// Entry-point for composing the applicative routes of the http application,
/// by iterating the options, applying the context, arg, to the predicate
/// from the list of options, until there's a match/a Some(x) which can be
/// run.
let rec choose (options : WebPart list): WebPart =
  fun arg -> async {
    match options with
    | []        -> return None
    | p :: tail ->
      let! res = p arg 
      match res with
      | Some x -> return Some x
      | None   -> return! choose tail arg
  }

// Inject a webPart
//
// +------------+                                            +--------------+
// | url "/a"   +----------+                       +---------+   cont1      |
// +------------+          |                       |         +--------------+
//                         |                       |                         
// +-------------+         |       +----------+    |         +--------------+
// |  url "/b"   +---------+-------+ injected +----+---------+  cont2       |
// +-------------+         |       +----------+    |         +--------------+
//                         |                       |                         
// +-------------+         |                       |         +--------------+
// | url "/b"    +---------+                       +---------+  cont3       |
// +-------------+                                           +--------------+
let rec inject (postOp : WebPart) (pairs : (WebPart*WebPart) list) : WebPart =
  fun arg -> async {
    match pairs with
    | []        -> return None
    | (p,q) :: tail ->
      let! res = p arg
      match res with
      | Some x ->
        return! (postOp >>= q) x
      | None   -> return! inject postOp tail arg
    }

/// Pipe the request through to a bird that can peck at it.
///
/// Put another way, using 'warbler' lets you look at the first parameter and
/// then make a decision about what thing to return (it's most likely a
/// WebPart you'll be returning). (Remember, WebPart is
/// HttpContext -> Async<HttpContext option>) where HttpContext is 'a and
/// Async<_> is 'b.
let inline warbler f a = f a a //which bird? A Warbler!

/// The constant function, which returns its constant, no matter
/// its input.
/// - theorem: identity = (cnst |> warbler)
/// (warbler cnst) x = cnst x x = fun _ -> x
let inline cnst x = fun _ -> x

/// The conditional function that applies f x a if there's a value in d,
/// or otherwise, applies g a, if there is no value in d.
let cond d f g a =
  match d with
  | Choice1Of2 x -> f x a
  | Choice2Of2 _ -> g a


/// general response functions
module Response =
  let responseContent status cnt =
    fun (ctx: HttpContext) ->
      let response = { ctx.response with status = status; content = cnt }
      { ctx with response = response } |> succeed

  /// Respond with a given status code, http reason phrase, content in the body to a http request.
  let responseb status (cnt: byte array) = responseContent status (Bytes cnt)

  let response status text = responseb status (UTF8.bytes text)

open Response

/// Module that allows changing the output response in different ways.
/// Functions have signature f :: params... -> HttpContext -> HttpContext.
module Writers =

  /// Sets a header with the key and value specified
  let setHeader key value (ctx: HttpContext) =
    { ctx with response = { ctx.response with headers = (key, value) :: ctx.response.headers } } |> succeed

  /// Set the Content-Type header to the mime type given
  let setMimeType t = setHeader "Content-Type" t

open Writers

/// <summary><para>
/// 2xx successful responses
/// </para><para>
/// Functions have signature <code>f :: params... -&gt; HttpContext -&gt; Async&lt;unit&gt; option</code>.
/// </para><para>
/// Functions from here are 'end routes' in that they don't require you to keep
/// returning applicatives, but can end up in an async monad/workflow that writes
/// the data to the client in the end.
/// </para></summary>
module Successful =
  let okb = responseb HTTP_200
  let ok s = UTF8.bytes s |> okb

  let createdb = responseb HTTP_201
  let created s = UTF8.bytes s |> createdb

  let acceptedb = responseb HTTP_202
  let accepted s = UTF8.bytes s |> acceptedb

  let noContent = responseb HTTP_204 [||]

/// <summary><para>
/// Functions have signature <code>f :: params... -&gt; HttpContext -&gt; Async&lt;unit&gt; option</code>.
/// <para></para>
///
/// Functions from here are 'end routes' in that they don't require you to keep
/// returning applicatives, but can end up in an async monad/workflow that writes
/// the data to the client in the end.
/// <para></para>
///
/// This class of status code indicates that further action needs to be
/// taken by the user agent in order to fulfill the request.  The action
/// required MAY be carried out by the user agent without interaction
/// with the user if and only if the method used in the second request is
/// GET or HEAD. A client SHOULD detect infinite redirection loops, since
/// such loops generate network traffic for each redirection.</para></summary>
/// <remarks>
///    Note: previous versions of this specification recommended a
///    maximum of five redirections. Content developers should be aware
///    that there might be clients that implement such a fixed
///    limitation.
/// </remarks>
module Redirection =
  let inline private simpleRedir status = fun location ->
    setHeader "Location" location
    >>= responseb status [||]

  let movedPermanently = simpleRedir HTTP_301

  let found = simpleRedir HTTP_302

  let notModified = responseb HTTP_304 [||]

  let redirect url =
    setHeader "Location" url
    >>= setHeader "Content-Type" "text/html; charset=utf-8"
    >>= responseb HTTP_302 (
        UTF8.bytes(sprintf "<html>
    <body>
      <a href=\"%s\">%s</a>
    </body>
  </html>"
        url HTTP_302.message))

/// <summary><para>10.4 Client Error 4xx</para>
///
/// <para>The 4xx class of status code is intended for cases in which the
/// client seems to have erred. Except when responding to a HEAD request,
/// the server SHOULD include an entity containing an explanation of the
/// error situation, and whether it is a temporary or permanent
/// condition. These status codes are applicable to any request method.
/// User agents SHOULD display any included entity to the user.
/// <para></para>
/// If the client is sending data, a server implementation using TCP
/// SHOULD be careful to ensure that the client acknowledges receipt of
/// the packet(s) containing the response, before the server closes the
/// input connection. If the client continues sending data to the server
/// after the close, the server's TCP stack will send a reset packet to
/// the client, which may erase the client's unacknowledged input buffers
/// before they can be read and interpreted by the HTTP application.
/// </para></summary>
module RequestErrors =
  let badRequestb = responseb HTTP_400
  let badRequest s = badRequestb (UTF8.bytes s)

  let unauthorizedb s =
    setHeader "WWW-Authenticate" "Basic realm=\"protected\""
    >>= responseb HTTP_401 s

  let unauthorized s = unauthorizedb (UTF8.bytes s)

  let challenge = unauthorized HTTP_401.message

  let forbiddenb = responseb HTTP_403
  let fobidden = UTF8.bytes >> forbiddenb
  let notfoundb = responseb HTTP_404
  let notfound = UTF8.bytes >> notfoundb
  let methodNotAllowedb = responseb HTTP_405
  let methodNotAllowed = UTF8.bytes >> methodNotAllowedb
  let notAcceptableb = responseb HTTP_406
  let notAcceptable = UTF8.bytes >> notAcceptableb
  let requestTimeout = responseb HTTP_408 [||]
  let conflictb = responseb HTTP_409
  let conflict = UTF8.bytes >> conflictb
  let goneb = responseb HTTP_410
  let gone = UTF8.bytes >> goneb
  let unsupportedMediaTypeb = responseb HTTP_415
  let unsupportedMediaType = UTF8.bytes >> unsupportedMediaTypeb
  let unprocessableEntityb = responseb HTTP_422
  let unprocessableEntity = UTF8.bytes >> unprocessableEntityb
  let preconditionRequiredb = responseb HTTP_428
  let preconditionRequired = UTF8.bytes >> preconditionRequiredb
  let tooManyRequestsb = responseb HTTP_429
  let tooManyRequests = UTF8.bytes >> tooManyRequestsb

/// 10.5 Server Error 5xx
/// Response status codes beginning with the digit "5" indicate cases in
/// which the server is aware that it has erred or is incapable of
/// performing the request. Except when responding to a HEAD request, the
/// server SHOULD include an entity containing an explanation of the
/// error situation, and whether it is a temporary or permanent
/// condition. User agents SHOULD display any included entity to the
/// user. These response codes are applicable to any request method.
module ServerErrors =
  let internalErrorb = responseb HTTP_500
  let internalError = UTF8.bytes >> internalErrorb
  let notImplementedb = responseb HTTP_501
  let notImplemented = UTF8.bytes >> notImplementedb
  let badGatewayb = responseb HTTP_502
  let badGateway = UTF8.bytes >> badGatewayb
  let serviceUnavailableb = responseb HTTP_503
  let serviceUnavailable = UTF8.bytes >> serviceUnavailableb
  let gatewayTimeoutb = responseb HTTP_504
  let gatewayTimeout = UTF8.bytes >> gatewayTimeoutb
  let invalidHttpVersionb = responseb HTTP_505
  let invalidHttpVersion = UTF8.bytes >> invalidHttpVersionb

/// Module that deals with the applicatives of suave - use functions from this module
/// to filter what requests a given route responds to.
/// Functions have signature f :: params... -> HttpContext -> HttpContext option.
module Applicatives =
  let inline private eql s1 s2 =
    System.StringComparer.OrdinalIgnoreCase.Compare (s1, s2) = 0

  let inline private iff b x =
    async.Return (if b then Some x else None)

  let rec private findHeader n = function
    | [] -> None
    | (name, value) :: rest ->
      if eql n name then Some value else findHeader n rest

  let rec private hasHeaderValue n v = function
    | [] -> false
    | (name, value) :: rest ->
      if eql n name && eql v value then true else hasHeaderValue n v rest

  let path s (x: HttpContext) =
    iff (Path.equals s x.request.path) x

  let httpMethod (m: HttpMethod) (x: HttpContext) =
    iff (m = x.request.httpMethod) x

  let isHttps (x: HttpContext) =
    iff x.request.isHttps x

  let pathRegex regex =
    let regex = System.Text.RegularExpressions.Regex (regex, System.Text.RegularExpressions.RegexOptions.Compiled)
    fun (x: HttpContext) -> iff (regex.IsMatch (Path.value x.request.path)) x

  let hasHeader name (x: HttpContext) =
    iff (match findHeader name x.request.headers with | None -> false | _ -> true) x

  let header name value (x: HttpContext) =
    iff (hasHeaderValue name value x.request.headers) x

  let host name = header "HOST" name

  let GET     = httpMethod HttpMethod.GET
  let POST    = httpMethod HttpMethod.POST
  let DELETE  = httpMethod HttpMethod.DELETE
  let PUT     = httpMethod HttpMethod.PUT
  let HEAD    = httpMethod HttpMethod.HEAD
  let CONNECT = httpMethod HttpMethod.CONNECT
  let PATCH   = httpMethod HttpMethod.PATCH
  let TRACE   = httpMethod HttpMethod.TRACE
  let OPTIONS = httpMethod HttpMethod.OPTIONS
