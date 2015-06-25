module Nini.Templ.Parser

open Nini

type Node =
| Text of string
| Comment of string
| Element of Element

with
  static member tag name =
    Element (Element.fromTag name)

and Element = {
 tag: string
 attributes: Map<string, string option>
 children: Node list }

 with
  static member internal fromTag name = { tag = name; attributes = Map.empty; children = List.empty }

type Doc = {
  declaration: string option
  directives: Map<string, string list>
  nodes: Node list }

type ParserMessage =
| Info of string
| Warning of string
| Error of string

type private ParserState = {
  messages: ParserMessage list
  doc: Doc
  stack: Element list
  attribute: string option
  lexerState: Lexer.LexerState }

let private addDirective kind value directives =
  match Map.tryFind kind directives with
  | None -> Map.add kind [value] directives
  | Some list -> Map.add kind (value :: list) directives

let private openImpliesClose =
  let pSet = Set.ofList [ "p" ]
  let formTags = 
    Set.ofList [
      "input"
      "option"
      "optgroup"
      "select"
      "button"
      "datalist"
      "textarea" ]

  Map.ofList [
    "tr", Set.ofList [ "tr"; "th"; "td" ]
    "th", Set.ofList [ "th" ]
    "td", Set.ofList [ "thead"; "th"; "td" ]
    "body", Set.ofList [ "head"; "link"; "script" ]
    "li", Set.ofList [ "li" ]
    "p", pSet
    "h1", pSet
    "h2", pSet
    "h3", pSet
    "h4", pSet
    "h5", pSet
    "h6", pSet
    "select", formTags
    "input", formTags
    "output", formTags
    "button", formTags
    "datalist", formTags
    "textarea", formTags
    "option", Set.ofList [ "option" ]
    "optgroup", Set.ofList [ "optgroup" ]
  ]

let private voidElements =
  Set.ofList [
    "area"
    "base"
    "basefont"
    "br"
    "col"
    "command"
    "embed"
    "frame"
    "hr"
    "img"
    "input"
    "isindex"
    "keygen"
    "link"
    "meta"
    "param"
    "source"
    "track"
    "wbr"

    //common self closing svg elements
    "path"
    "circle"
    "ellipse"
    "line"
    "rect"
    "use"
    "stop"
    "polyline"
    "polygon"
  ]

let private add node doc = function
  | parent :: rest -> doc, { parent with children = node :: parent.children } :: rest
  | [] -> { doc with nodes = node :: doc.nodes }, []

let private close doc = function
  | tag :: parent :: rest -> doc, { parent with children = Element tag :: parent.children } :: rest
  | [tag] -> { doc with nodes = Element tag :: doc.nodes }, []
  | [] -> failwith "Stack empty"

let rec private closeWhile fn doc = function
  | h :: _ as stack when fn h ->
    let doc, stack = close doc stack
    closeWhile fn doc stack
  | _ as list -> doc, list

let rec private closeUnbalanced i doc messages stack =
  match i with
  | 0 ->
    let doc, stack = close doc stack
    doc, stack, messages
  | _ when i > 0 ->
    let tag = (List.head stack).tag
    let doc, stack = close doc stack
    let messages = Warning (sprintf "Closed tag %s due to unballance" tag) :: messages
    closeUnbalanced (i - 1) doc messages stack
  | _ -> failwithf "Invalid index %d" i

let rec private closeEnd doc messages stack =
  match stack with
  | [] -> doc, messages
  | tag :: t ->
    let doc, stack = close doc stack
    let messages = Warning (sprintf "Unclosed tag %s" tag.tag) :: messages
    closeEnd doc messages t

let private setAttribute name value stack messages =
  match stack, name with
  | _, None -> failwith "Attribute name is null"
  | [], _ -> failwith "Empty stack"
  | tag :: rest, Some name ->
    match Map.tryFind name tag.attributes with
    | None ->
      // attribute not present, set it.
      { tag with attributes = Map.add name value tag.attributes } :: rest, messages
    | Some _ ->
      // attribute already set, keep first value
      stack, Warning (sprintf "Tag %s has multiple instances of attribute with name %s" tag.tag name) :: messages

let rec private parseDoc state =
  let token, lexerState = Lexer.next state.lexerState
  match token with
  | Lexer.Declaration d ->
    let normalized = (d.Trim ()).Split ([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
    let keyword = normalized.[0].ToLowerInvariant ()
    let value = String.concat " " (Seq.skip 1 normalized)
    let doc = 
      match keyword with
      | "doctype" ->
        match state.doc.declaration with
        | Some old -> failwithf "Multiple doctypes spesified:\n Old doctype: %s\n New doctype: %s" old d
        | None -> { state.doc with declaration = Some d }
      | _ ->
        { state.doc with directives = addDirective keyword value state.doc.directives }
    
    parseDoc { state with doc = doc; lexerState = lexerState }

  | Lexer.OpenTagName tag ->
    let tag = tag.ToLowerInvariant ()
    let doc, stack =
      match Map.tryFind tag openImpliesClose with
      | Some tags ->
        closeWhile (fun e -> Set.contains e.tag tags) state.doc state.stack
      | _ -> state.doc, state.stack

    let stack = Element.fromTag tag :: stack
    
    parseDoc { state with doc = doc; stack = stack; lexerState = lexerState }

  | Lexer.OpenTagEnd ->
    match state.stack with
    | tag :: _ when Set.contains tag.tag voidElements ->
      // Auto-close void elements
      let doc, stack = close state.doc state.stack

      parseDoc { state with lexerState = lexerState; stack = stack; doc = doc }

    | _ ->
      parseDoc { state with lexerState = lexerState }

  | Lexer.CloseTagName tag ->
    let tag = tag.ToLowerInvariant ()

    match List.tryFindIndex (fun e -> e.tag = tag) state.stack with
    | None ->
      // Tag that is being closed was never opened
      // give a warning, but do nothing
      parseDoc { state with lexerState = lexerState; messages = Warning (sprintf "Tag %s closed, but never opened" tag) :: state.messages }

    | Some 0 ->
      // The closed tag is (correctly) the top element. Proceed with no warnings
      let doc, stack = close state.doc state.stack

      parseDoc { state with lexerState = lexerState; stack = stack; doc = doc }

    | Some i ->
      // The node that was closed is farther down the stack. This should produce a warning, but still work
      let doc, stack, messages = closeUnbalanced i state.doc state.messages state.stack

      parseDoc { state with lexerState = lexerState; stack = stack; doc = doc; messages = messages }

  | Lexer.SelfClosingTag ->
    let doc, stack = close state.doc state.stack

    parseDoc { state with lexerState = lexerState; stack = stack; doc = doc }

  | Lexer.AttributeName name ->
    parseDoc { state with lexerState = lexerState; attribute = Some name }

  | Lexer.AttributeData data ->
    let stack, messages = setAttribute state.attribute (Some data) state.stack state.messages

    parseDoc { state with lexerState = lexerState; attribute = None; stack = stack; messages = messages }

  | Lexer.AttributeEnd ->
    let stack, messages = setAttribute state.attribute None state.stack state.messages

    parseDoc { state with lexerState = lexerState; attribute = None; stack = stack; messages = messages }

  | Lexer.Whitespace content
  | Lexer.Text content ->
    let doc, stack = add (Text content) state.doc state.stack

    parseDoc { state with lexerState = lexerState; doc = doc; stack = stack }

  | Lexer.Comment comment ->
    let doc, stack = add (Comment comment) state.doc state.stack

    parseDoc { state with lexerState = lexerState; doc = doc; stack = stack }

  | Lexer.EOF ->
    closeEnd state.doc state.messages state.stack

  // TODO: Remove
  | _ -> raise (new System.NotImplementedException (sprintf "%A not implemented" token))

let rec private reverse nodes =
  nodes
  |> List.rev
  |> List.map (function | Element e -> Element { e with children = reverse e.children } | _ as n -> n)

let parse str =
  let lexState = Lexer.create str
  let doc = { declaration = None; directives = Map.empty; nodes = List.empty }
  let parseState = { doc = doc; stack = List.empty; lexerState = lexState; messages = List.empty; attribute = None }
  let reversedDoc, reversedMsg = parseDoc parseState
  { reversedDoc with nodes = reverse reversedDoc.nodes }, List.rev reversedMsg