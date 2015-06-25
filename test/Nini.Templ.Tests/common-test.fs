module Nini.TestCommon

open System
open Xunit
open Xunit.Sdk

module Should =
  let inline equal (expected: obj) (actual: obj) = Assert.Equal (expected, actual)
  let inline be (expected: bool) (actual: bool) =
    match expected with
    | true -> Assert.True (actual)
    | false -> Assert.False (actual)