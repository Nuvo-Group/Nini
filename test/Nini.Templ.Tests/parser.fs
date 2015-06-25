module ``parser tests``

open Xunit
open Xunit.Sdk
open Nini.TestCommon
open Nini.Templ
open Nini.Templ.Parser

let inline trim (str: string) = str.Trim ()

let testParse template expected =
  let parsed = parse template
  Assert.Equal (expected, parsed)

let rec removeWhitespace = function
  | Element e ->
    let children = 
      e.children 
      |> List.filter (function | Text s when System.String.IsNullOrWhiteSpace s -> false | _ -> true) 
      |> List.map removeWhitespace

    Element { e with children = children }
  | _ as n -> n

let testSingleElement template expected =
  let doc, messages = parse template
  let elements = List.choose (function | Element _ as n -> Some n | _ -> None) doc.nodes
  match elements with
  | [node] -> 
    let node = removeWhitespace node
    let actual = match node with | Element e -> e | _ -> failwith "Not an element"
    if not (actual = expected) then
      raise (new AssertActualExpectedException (expected, actual, (sprintf "%A did not match %A" actual expected)))
    else ()
  | [] -> failwith "No elements returned"
  | _ -> failwith "More than one element returned"

[<Fact>]
let ``parses doctypes`` () =
  let template = trim """
<!DOCTYPE html>
"""
  
  testParse template ({ declaration = Some "DOCTYPE html"; directives = Map.empty; nodes = List.empty }, [])

[<Fact>]
let ``parses simple node`` () =
  let template = trim """
<html></html>
"""
  
  testSingleElement template { tag = "html"; attributes = Map.empty; children = List.empty }

[<Fact>]
let ``handles attributes`` () =
  let template = trim """
<html class="no-js"></html>
"""

  testSingleElement template { tag = "html"; attributes = Map.ofList [ "class", Some "no-js" ]; children = List.empty }

[<Fact>]
let ``handles open-ended documents`` () =
  let template = trim """
<html class="no-js">
"""

  testSingleElement template { tag = "html"; attributes = Map.ofList [ "class", Some "no-js" ]; children = List.empty }

[<Fact>]
let ``handles comments`` () =
  let template = trim """
<!DOCTYPE html>
<!-- comment -->
"""
  let doc, messages = parse template
  let nodes = doc.nodes
  Assert.Equal (2, List.length nodes)

  //let comment = List.item 1 nodes
  let [_; comment] = nodes
  Assert.Equal (Comment " comment ", comment)

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
  
  testSingleElement template {
    tag = "html"
    attributes = Map.empty
    children = 
      [
        Element { 
          tag = "head"
          attributes = Map.empty
          children = 
            [
              Element {
                tag = "title"
                attributes = Map.empty
                children = 
                  [
                    Text "Foo"
                  ]
              }
            ]
        }
         
        Element {
          tag = "body"
          attributes = Map.ofList ["class", Some "foo"; "empty", None ]
          children = 
            [
              Element {
                tag = "special-tag"
                attributes = Map.empty
                children = []
              }
            ]
        }
      ]
  }