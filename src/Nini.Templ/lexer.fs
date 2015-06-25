module Nini.Templ.Lexer

type private QuoteKind =
| Double
| Single
| Unquoted

type private State =
| Empty
| BeforeTagName // after <
| InTagName
| InSelfClosingTag
| BeforeClosingTagName
| InClosingTagName
| AfterClosingTagName

// attributes
| BeforeAttributeName
| InAttributeName
| AfterAttributeName
| BeforeAttributeValue
| InAttributeValue of QuoteKind

// declarations
| BeforeDeclaration
| InDeclaration

// processing instructions
| InProcessingInstruction

// comments
| BeforeComment
| InComment
| AfterComment1
| AfterComment2

// cdata
| BeforeCData1 // [
| BeforeCData2 // C
| BeforeCData3 // D
| BeforeCData4 // A
| BeforeCData5 // T
| BeforeCData6 // A
| InCData      // [
| AfterCData1  // ]
| AfterCData2  // ]

// special tags
| BeforeSpecial // S
| BeforeSpecialEnd // S

| BeforeScript1 // C
| BeforeScript2 // R
| BeforeScript3 // I
| BeforeScript4 // P
| BeforeScript5 // T
| AfterScript1  // C
| AfterScript2  // R
| AfterScript3  // I
| AfterScript4  // P
| AfterScript5  // T

| BeforeStyle1  // T
| BeforeStyle2  // Y
| BeforeStyle3  // L
| BeforeStyle4  // E
| AfterStyle1   // T
| AfterStyle2   // Y
| AfterStyle3   // L
| AfterStyle4   // E

// End
| EOF

type private Special =
| Script
| Style

type Token =
| Text of string
| Whitespace
| OpenTagName of string
| CloseTagName of string
| SelfClosingTag
| OpenTagEnd
| AttributeName of string
| AttributeData of string
| AttributeEnd // used for empty attributes
| Declaration of string
| ProcessingInstruction of string
| Comment of string
| CData of string
| EOF

type LexerState = private {
  state: State
  baseState: State
  buffer: string
  index: int
  sectionStart: int
  xmlMode: bool
  special: Special option }

let create template = {
  state = State.Empty
  baseState = State.Empty
  buffer = template
  index = 0
  sectionStart = 0
  xmlMode = false
  special = None }

let private whitespace c =
  c = ' ' || c = '\n' || c = '\t' || c = '\f' || c = '\r'

let private makeText text =
  match System.String.IsNullOrWhiteSpace text with
  | true -> Whitespace
  | false -> Text text

let private ifState (c: char) ifTrue next =
  fun state char ->
    if char = c then next { state with state = ifTrue } else next state

let private ifElseState (c: char) ifTrue ifFalse next =
  fun state char ->
    if char = c then next { state with state = ifTrue } else next { state with state = ifFalse }

let private ifElseStatei (c: char) ifTrue ifFalse next =
  let lower = c
  let upper = System.Char.ToUpperInvariant c
  fun state char ->
    if char = lower || char = upper then next { state with state = ifTrue } else next { state with state = ifFalse }

let private consumeSpecialNameChar (c: char) ifTrue next =
  let lower = c
  let upper = System.Char.ToUpperInvariant c
  fun state char ->
    let matches = char = lower || char = upper
    if matches then
      next { state with state = ifTrue }
    else
      next { state with state = State.InTagName; index = state.index - 1 }

let private consumeEndSpecialNameChar (c: char) ifTrue next =
  let lower = c
  let upper = System.Char.ToUpperInvariant c
  fun state char ->
    let matches = char = lower || char = upper
    if matches then
      next { state with state = ifTrue }
    else
      next { state with state = State.Empty; index = state.index - 1 }

let private read s =
  if s.index = s.buffer.Length then
    char 0, s
  else
    s.buffer.[s.index], { s with index = s.index + 1 }

let private section s =
  s.buffer.Substring (s.sectionStart, s.index - 1 - s.sectionStart)

let rec next state =
  match state.index with
  | n when n = state.buffer.Length -> EOF, state
  | _ ->
    let c, state = read state
    match state.state with
    | State.Empty -> text state c
    | State.BeforeTagName -> beforeTagName state c
    | State.InTagName -> inTagName state c
    | State.BeforeClosingTagName -> beforeClosingTagName state c
    | State.InClosingTagName -> inClosingTagName state c
    | State.AfterClosingTagName -> afterClosingTagName state c
    | State.InSelfClosingTag -> inSelfClosingTag state c
    
    // Attributes
    | State.BeforeAttributeName -> beforeAttributeName state c
    | State.InAttributeName -> inAttributeName state c
    | State.AfterAttributeName -> afterAttributeName state c
    | State.BeforeAttributeValue -> beforeAttributeValue state c
    | State.InAttributeValue k -> inAttributeValue state k c
    
    // Declarations
    | State.BeforeDeclaration -> beforeDeclaration state c
    | State.InDeclaration -> inDeclaration state c
    
    // Processing instructions
    | State.InProcessingInstruction -> inProcessingInstruction state c

    // Comments
    | State.BeforeComment -> beforeComment state c
    | State.InComment -> inComment state c
    | State.AfterComment1 -> afterComment1 state c
    | State.AfterComment2 -> afterComment2 state c

    // cdata
    | State.BeforeCData1 -> beforeCData1 state c
    | State.BeforeCData2 -> beforeCData2 state c
    | State.BeforeCData3 -> beforeCData3 state c
    | State.BeforeCData4 -> beforeCData4 state c
    | State.BeforeCData5 -> beforeCData5 state c
    | State.BeforeCData6 -> beforeCData6 state c
    | State.InCData -> inCData state c
    | State.AfterCData1 -> afterCData1 state c
    | State.AfterCData2 -> afterCData2 state c

    // special tags
    | State.BeforeSpecial -> beforeSpecial state c
    | State.BeforeSpecialEnd -> beforeSpecialEnd state c

    // script
    | State.BeforeScript1 -> beforeScript1 state c
    | State.BeforeScript2 -> beforeScript2 state c
    | State.BeforeScript3 -> beforeScript3 state c
    | State.BeforeScript4 -> beforeScript4 state c
    | State.BeforeScript5 -> beforeScript5 state c
    | State.AfterScript1 -> afterScript1 state c
    | State.AfterScript2 -> afterScript2 state c
    | State.AfterScript3 -> afterScript3 state c
    | State.AfterScript4 -> afterScript4 state c
    | State.AfterScript5 -> afterScript5 state c
    
    // style
    | State.BeforeStyle1 -> beforeStyle1 state c
    | State.BeforeStyle2 -> beforeStyle2 state c
    | State.BeforeStyle3 -> beforeStyle3 state c
    | State.BeforeStyle4 -> beforeStyle4 state c
    | State.AfterStyle1 -> afterStyle1 state c
    | State.AfterStyle2 -> afterStyle2 state c
    | State.AfterStyle3 -> afterStyle3 state c
    | State.AfterStyle4 -> afterStyle4 state c

    // EOF
    | State.EOF -> EOF, state

and private text state = function
  | '<' ->
    let newState = { state with state = BeforeTagName; sectionStart = state.index - 1 }
    if state.index > state.sectionStart + 1 then
      let str = section state
      makeText str, newState
    else
      next newState
  | _ -> next state

and private beforeTagName state = function
  | '/' -> next { state with state = State.BeforeClosingTagName }
  | '>' | _ when not (state.special = None) ->
    next { state with state = State.Empty }
  | w when whitespace w ->
    next { state with state = State.Empty }
  | '!' -> next { state with state = State.BeforeDeclaration; sectionStart = state.index }
  | '?' -> next { state with state = State.InProcessingInstruction; sectionStart = state.index }
  | '<' -> makeText (section state), { state with sectionStart = state.index }
  | _ as c ->
    let s =
      match state.xmlMode, c with
      | false, 's'
      | false, 'S' -> State.BeforeSpecial
      | _ -> State.InTagName

    next { state with state = s; sectionStart = state.index - 1 }

and private inTagName state = function
  | '/'
  | '>' -> OpenTagName (section state), { state with state = State.BeforeAttributeName; index = state.index - 1 }
  | w when whitespace w ->
    OpenTagName (section state), { state with state = State.BeforeAttributeName; index = state.index - 1 }
  | _ -> next state

and private beforeClosingTagName state = function
  | w when whitespace w -> next state
  | '>' -> next { state with state = State.Empty }
  | 's' when not (state.special = None) -> next { state with state = State.BeforeSpecialEnd }
  | 'S' when not (state.special = None) -> next { state with state = State.BeforeSpecialEnd }
  | _ when not (state.special = None) -> next { state with state = State.Empty; index = state.index - 1 }
  | _ -> next { state with state = State.InClosingTagName; sectionStart = state.index - 1 }

and private inClosingTagName state = function
  | '>' -> CloseTagName (section state), { state with state = State.AfterClosingTagName; index = state.index - 1; sectionStart = state.index }
  | w when whitespace w -> CloseTagName (section state), { state with state = State.AfterClosingTagName; index = state.index - 1 }
  | _ -> next state

and private afterClosingTagName state = function
  // skip everything until >
  | '>' -> next { state with state = State.Empty; index = state.index - 1 }
  | _ -> next state   

and private inSelfClosingTag state = function
  | '>' -> SelfClosingTag, { state with state = State.Empty; sectionStart = state.index }
  | w when not (whitespace w) -> next { state with state = State.AfterAttributeName; index = state.index - 1 }
  | _ -> next state

and private beforeAttributeName state = function
  | '>' -> OpenTagEnd, { state with state = State.Empty; sectionStart = state.index }
  | '/' -> next { state with state = State.InSelfClosingTag }
  | w when not (whitespace w) -> next { state with state = State.InAttributeName; sectionStart = state.index - 1 }
  | _ -> next state

and private inAttributeName state = function
  | '='
  | '/'
  | '>' -> AttributeName (section state), { state with state = State.AfterAttributeName; sectionStart = -1; index = state.index - 1 }
  | w when whitespace w -> AttributeName (section state), { state with state = State.AfterAttributeName; sectionStart = -1; index = state.index - 1 }
  | _ -> next state

and private afterAttributeName state = function
  | '=' -> next { state with state = State.BeforeAttributeValue }
  | '/'
  | '>' -> AttributeEnd, { state with state = State.BeforeAttributeName; sectionStart = state.index; index = state.index - 1 }
  | w when whitespace w -> AttributeEnd, { state with state = State.BeforeAttributeName; sectionStart = state.index; index = state.index - 1 }
  | _ -> next state

and private beforeAttributeValue state = function
  | '"' -> next { state with state = State.InAttributeValue QuoteKind.Double; sectionStart = state.index }
  | '\'' -> next { state with state = State.InAttributeValue QuoteKind.Single; sectionStart = state.index }
  | w when not (whitespace w) -> next { state with state = State.InAttributeValue QuoteKind.Unquoted; sectionStart = state.index - 1; index = state.index - 1 }
  | _ -> next state

and private inAttributeValue state kind c =
  match kind, c with
  | QuoteKind.Double, '"'  -> AttributeData (section state), { state with state = State.BeforeAttributeName }
  | QuoteKind.Single, '\'' -> AttributeData (section state), { state with state = State.BeforeAttributeName }
  | QuoteKind.Unquoted, '>' -> AttributeData (section state), { state with state = State.BeforeAttributeName; index = state.index - 1 }
  | QuoteKind.Unquoted, w when whitespace w -> AttributeData (section state), { state with state = State.BeforeAttributeName; index = state.index - 1 }
  | _ -> next state

and private beforeDeclaration state = function
  | '[' -> next { state with state = State.BeforeCData1 }
  | '-' -> next { state with state = State.BeforeComment }
  | _ -> next { state with state = State.InDeclaration }

and private inDeclaration state = function
  | '>' -> Declaration (section state), { state with state = State.Empty; sectionStart = state.index }
  | _ -> next state

and private inProcessingInstruction state = function
  | '>' -> ProcessingInstruction (section state), { state with state = State.Empty; sectionStart = state.index }
  | _ -> next state

and private beforeComment state = function
  | '-' -> next { state with state = State.InComment; sectionStart = state.index }
  | _ -> next { state with state = State.InDeclaration }

and private inComment state = function
  | '-' -> next { state with state = State.AfterComment1 }
  | _ -> next state

and private afterComment1 state = function
  | '-' -> next { state with state = State.AfterComment2 }
  | _ -> next { state with state = State.InComment }

and private afterComment2 state = function
  | '>' ->
    let content = section state
    let content = content.Substring (0, content.Length - 2)
    Comment content, { state with state = State.Empty; sectionStart = state.index }
  | '-' -> next state
  | _ -> next { state with state = State.InComment }

and private beforeCData1 = ifElseState 'C' State.BeforeCData2 State.InDeclaration next
and private beforeCData2 = ifElseState 'D' State.BeforeCData3 State.InDeclaration next
and private beforeCData3 = ifElseState 'A' State.BeforeCData4 State.InDeclaration next
and private beforeCData4 = ifElseState 'T' State.BeforeCData5 State.InDeclaration next
and private beforeCData5 = ifElseState 'A' State.BeforeCData6 State.InDeclaration next

and private beforeCData6 state = function
  | '[' -> next { state with state = State.InCData; sectionStart = state.index }
  | _ -> next { state with state = State.InDeclaration; index = state.index - 1 }

and private inCData state = function
  | ']' -> next { state with state = State.AfterCData1 }
  | _ -> next state

and private afterCData1 = ifState ']' State.AfterCData1 next

and private afterCData2 state = function
  | '>' ->
    let content = section state
    let content = content.Substring (0, content.Length - 2)
    CData content, { state with state = State.Empty; sectionStart = state.index }
  | ']' -> next state
  | _ -> next { state with state = State.InCData }

and private beforeSpecial state = function
  | 'c' | 'C' -> next { state with state = State.BeforeScript1 }
  | 't' | 'T' -> next { state with state = State.BeforeStyle1 }
  | _ -> next { state with state = State.InTagName; index = state.index - 1 }

and private beforeSpecialEnd state = function
  | 'c' | 'C' when state.special = Some Special.Script -> next { state with state = State.AfterScript1 }
  | 't' | 'T' when state.special = Some Special.Style  -> next { state with state = State.AfterStyle1 }
  | _ -> next { state with state = State.Empty }

and private beforeScript1 = consumeSpecialNameChar 'r' State.BeforeScript2 next
and private beforeScript2 = consumeSpecialNameChar 'i' State.BeforeScript3 next
and private beforeScript3 = consumeSpecialNameChar 'p' State.BeforeScript4 next
and private beforeScript4 = consumeSpecialNameChar 't' State.BeforeScript5 next

and private beforeScript5 state c =
  let state =
    if c = '/' || c = '>' || whitespace c then { state with special = Some Special.Script } else state

  next { state with state = State.InTagName; index = state.index - 1 }

and private afterScript1 = consumeEndSpecialNameChar 'r' State.AfterScript2 next
and private afterScript2 = consumeEndSpecialNameChar 'i' State.AfterScript3 next
and private afterScript3 = consumeEndSpecialNameChar 'p' State.AfterScript4 next
and private afterScript4 = consumeEndSpecialNameChar 't' State.AfterScript5 next

and private afterScript5 state c =
  if c = '>' || whitespace c then
    next { state with special = None; state = State.InClosingTagName; sectionStart = state.index - 7; index = state.index - 1 }
  else
    next { state with state = State.Empty }

and private beforeStyle1 = consumeSpecialNameChar 'y' State.BeforeStyle2 next
and private beforeStyle2 = consumeSpecialNameChar 'l' State.BeforeStyle3 next
and private beforeStyle3 = consumeSpecialNameChar 'e' State.BeforeStyle4 next

and private beforeStyle4 state c =
  let state =
    if c = '/' || c = '>' || whitespace c then { state with special = Some Special.Style } else state

  next { state with state = State.InTagName; index = state.index - 1 }

and private afterStyle1 = consumeEndSpecialNameChar 'y' State.AfterStyle2 next
and private afterStyle2 = consumeEndSpecialNameChar 'l' State.AfterStyle3 next
and private afterStyle3 = consumeEndSpecialNameChar 'e' State.AfterStyle4 next

and private afterStyle4 state c =
  if c = '>' || whitespace c then
    next { state with special = None; state = State.InClosingTagName; sectionStart = state.index - 6; index = state.index - 1 }
  else
    next { state with state = State.Empty }