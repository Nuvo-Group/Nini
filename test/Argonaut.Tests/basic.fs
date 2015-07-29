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

let case1Context = OptionsParser.create<Case1Arguments>

[<Fact>]
let ``handles combination of flag shortnames`` () =
  // arrange
  let args = [| "-ttet" |]

  // act
  let result = OptionsParser.run case1Context args

  // assert
  assertSuccess (checkArgs [TestArg1; TestArg1; TestArg2; TestArg1]) result

[<Fact>]
let ``handles full name flags`` () =
  // arrange
  let args = [| "--test-arg1"; "--test-arg2" |]

  // act
  let result = OptionsParser.run case1Context args

  // assert
  assertSuccess (checkArgs [TestArg1; TestArg2]) result

[<Fact>]
let ``handles string arguments`` () =
  // arrange
  let args = [| "--string-arg"; "value1"; "-s"; "value2" |]

  // act
  let result = OptionsParser.run case1Context args

  // assert
  assertSuccess (checkArgs [StringArg "value1"; StringArg "value2"]) result