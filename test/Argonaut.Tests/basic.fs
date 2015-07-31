module ``basic argonaut tests``

open Xunit

open Argonaut

module List =
  let remove index list =
    let start = List.take index list
    let rest = List.skip (index + 1) list
    start @ rest

let checkArgs expected actual =
  // arguments come out un-ordered from argonaut
  // meaning the order should not be tested.

  // make sure all the arguments are present,
  // and pluck the out one at a time.

  let rec run list set i =
    match list with
    | [] -> 
      match set with
      | [] -> ()
      | _ -> raise (Sdk.AssertCollectionCountException (List.length expected, List.length actual))
    | head :: tail ->
      match List.tryFindIndex ((=) head) set with
      | None ->
        raise (Sdk.ContainsException (head, null))
      | Some index ->
        run tail (List.remove index set) (i + 1)

  // Run the function
  run expected actual 0

let assertSuccess validate result =
  match result with
  | Success v -> validate v
  | Help _ -> raise (Xunit.Sdk.IsTypeException ("Success", "Help"))
  | Error _ -> raise (Xunit.Sdk.IsTypeException ("Success", "Error"))

type Case1Arguments =
| [<Shorthand("t")>] TestArg1
| [<Shorthand("e")>] TestArg2
| [<Shorthand("s")>] StringArg of string

  interface IArgumentTemplate with
    member x.Usage = ""

type Case1Options = {
  arg1: bool
  arg2: bool
  str: string option }

let case1Context = OptionsParser.create<Case1Arguments>
let parse1 = OptionsParser.parse case1Context
let run1 folder state handler = OptionsParser.runContext case1Context folder state handler
let defaultOptions1 = { arg1 = false; arg2 = false; str = None }
let handleArg1 options = function
  | TestArg1 -> { options with arg1 = true }
  | TestArg2 -> { options with arg2 = true }
  | StringArg s -> { options with str = Some s }

let validateOptions1 ret (expected: 'a) (actual: 'a) =
  Assert.Equal<'a> (expected, actual)
  ret

[<Fact>]
let ``handles combination of flag shortnames`` () =
  // arrange
  let args = [| "-ttet" |]

  // act
  let result = parse1 args

  // assert
  assertSuccess (checkArgs [TestArg1; TestArg1; TestArg2; TestArg1]) result

[<Fact>]
let ``handles full name flags`` () =
  // arrange
  let args = [| "--test-arg1"; "--test-arg2" |]

  // act
  let result = parse1 args

  // assert
  assertSuccess (checkArgs [TestArg1; TestArg2]) result

[<Fact>]
let ``handles string arguments`` () =
  // arrange
  let args = [| "--string-arg"; "value1"; "-s"; "value2" |]

  // act
  let result = parse1 args

  // assert
  assertSuccess (checkArgs [StringArg "value1"; StringArg "value2"]) result

[<Fact>]
let ``runs properly`` () =
  // arrange
  let args1 = [| "-t"; "-s"; "some value" |]
  let args2 = [| "-e" |]

  let test expectedOptions args =
    let ret = obj ()
    let result = run1 handleArg1 defaultOptions1 (validateOptions1 ret expectedOptions) args
    Assert.Same (ret, result)

  // act & assert
  test { defaultOptions1 with arg1 = true; str = Some "some value" } args1
  test { defaultOptions1 with arg2 = true } args2