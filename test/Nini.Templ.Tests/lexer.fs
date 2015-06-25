module ``lexer tests``

open Xunit
open Xunit.Sdk
open Nini.TestCommon
open Nini.Templ

let inline trim (str: string) = str.Trim ()

let matchToken actual expected =
  if not (actual = expected) then
    raise (new AssertActualExpectedException (expected, actual, (sprintf "%A did not match %A" actual expected)))
  else ()

let testTemplate template expected =
  let rec run state expected =
    match expected with
    | [] ->
      // No more expected, thus expect EOF
      let token, state = Lexer.next state
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
    Lexer.Whitespace
    Lexer.OpenTagName "html"
    Lexer.OpenTagEnd
    Lexer.Whitespace
    Lexer.OpenTagName "head"
    Lexer.OpenTagEnd
    Lexer.Whitespace
    Lexer.OpenTagName "title"
    Lexer.OpenTagEnd
    Lexer.Text "Foo"
    Lexer.CloseTagName "title"
    Lexer.Whitespace
    Lexer.CloseTagName "head"
    Lexer.Whitespace
    Lexer.OpenTagName "body"
    Lexer.AttributeName "class"
    Lexer.AttributeData "foo"
    Lexer.AttributeName "empty"
    Lexer.AttributeEnd
    Lexer.OpenTagEnd
    Lexer.Whitespace
    Lexer.OpenTagName "special-tag"
    Lexer.SelfClosingTag
    Lexer.Whitespace
    Lexer.CloseTagName "body"
    Lexer.Whitespace
    Lexer.CloseTagName "html" ]

[<Fact>]
let ``handles self-closing script tags`` () =
  let template = trim """
<!DOCTYPE html>
<script src="foobar.js" />
"""

  testTemplate template [
    Lexer.Declaration "DOCTYPE html"
    Lexer.Whitespace
    Lexer.OpenTagName "script"
    Lexer.AttributeName "src"
    Lexer.AttributeData "foobar.js"
    Lexer.SelfClosingTag ]