module ``lexer tests``

open Xunit
open Xunit.Sdk
open Nini.TestCommon
open Nini.Templ

let inline trim (str: string) = str.Trim ()

let matchToken actual expected =
  let success = 
    match expected with
    | Lexer.Whitespace null ->
      match actual with
      | Lexer.Whitespace _ -> true
      | _ -> false
    | _ -> actual = expected

  if not success then
    raise (new AssertActualExpectedException (expected, actual, (sprintf "%A did not match %A" actual expected)))
  else ()

let testTemplate template expected =
  let rec run state expected =
    match expected with
    | [] ->
      // No more expected, thus expect EOF
      let token, _ = Lexer.next state
      matchToken token Lexer.Token.EOF
    | h :: t ->
      let token, state = Lexer.next state
      matchToken token h
      run state t
  
  let state = Lexer.create template
  run state expected

[<Fact>]
let ``parses empty string to EOF only`` () =
  let template = ""

  testTemplate template []

[<Fact>]
let ``parses doctypes`` () =
  let template = trim """
<!DOCTYPE html>
"""
  
  testTemplate template [
    Lexer.Declaration "DOCTYPE html"]

[<Fact>]
let ``parses simple document`` () =
  let template = trim """
<!DOCTYPE html>
<html>
  <head>
    <title>Foo</title>
  </head>
  <body class="foo" empty>
    <special-tag />
  </body>
</html>
"""
  
  testTemplate template [
    Lexer.Declaration "DOCTYPE html"
    Lexer.Whitespace null
    Lexer.OpenTagName "html"
    Lexer.OpenTagEnd
    Lexer.Whitespace null
    Lexer.OpenTagName "head"
    Lexer.OpenTagEnd
    Lexer.Whitespace null
    Lexer.OpenTagName "title"
    Lexer.OpenTagEnd
    Lexer.Text "Foo"
    Lexer.CloseTagName "title"
    Lexer.Whitespace null
    Lexer.CloseTagName "head"
    Lexer.Whitespace null
    Lexer.OpenTagName "body"
    Lexer.AttributeName "class"
    Lexer.AttributeData "foo"
    Lexer.AttributeName "empty"
    Lexer.AttributeEnd
    Lexer.OpenTagEnd
    Lexer.Whitespace null
    Lexer.OpenTagName "special-tag"
    Lexer.SelfClosingTag
    Lexer.Whitespace null
    Lexer.CloseTagName "body"
    Lexer.Whitespace null
    Lexer.CloseTagName "html" ]

[<Fact>]
let ``handles self-closing script tags`` () =
  let template = trim """
<!DOCTYPE html>
<script src="foobar.js" />
"""

  testTemplate template [
    Lexer.Declaration "DOCTYPE html"
    Lexer.Whitespace null
    Lexer.OpenTagName "script"
    Lexer.AttributeName "src"
    Lexer.AttributeData "foobar.js"
    Lexer.SelfClosingTag ]

[<Fact>]
let ``handles comments`` () =
  let template = trim """
<!DOCTYPE html>
<!-- comment
here -->
"""

  testTemplate template [
    Lexer.Declaration "DOCTYPE html"
    Lexer.Whitespace null
    Lexer.Comment " comment\nhere " ]

[<Fact>]
let ``handles tight code`` () =
  let template = trim """
<!DOCTYPE html>
<span>formatted-<span class="red">word</span></span>
"""

  testTemplate template [
    Lexer.Declaration "DOCTYPE html"
    Lexer.Whitespace null
    Lexer.OpenTagName "span"
    Lexer.OpenTagEnd
    Lexer.Text "formatted-"
    Lexer.OpenTagName "span"
    Lexer.AttributeName "class"
    Lexer.AttributeData "red"
    Lexer.OpenTagEnd
    Lexer.Text "word"
    Lexer.CloseTagName "span"
    Lexer.CloseTagName "span" ]