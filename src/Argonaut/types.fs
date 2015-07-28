namespace Argonaut

open System
open System.Globalization

type IArgumentTemplate =
  abstract Usage : string

type Result<'a> =
| Help of string
| Error of string * string
| Success of 'a

/// Consume all remaining command line arguments.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type RestAttribute () = inherit Attribute ()

/// Demands at least one parsed result for this branch; an exception is raised otherwise.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
type MandatoryAttribute () = inherit Attribute ()

/// Hidden in the help text
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
type HiddenAttribute () = inherit Attribute ()

/// Command argument
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
type CommandAttribute () = inherit Attribute ()

/// Sets a custom command line name.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type NameAttribute (name : string) =
  inherit Attribute ()
  member private x.Name = name
  static member internal name (x: NameAttribute) = x.Name

/// Sets alternative command line names.
[<AttributeUsage(AttributeTargets.Property, AllowMultiple = true)>]
type ShorthandAttribute (name : string) = 
  inherit Attribute ()
  member private x.Name = name
  static member internal name (x: ShorthandAttribute) = x.Name

type ArgParser = CultureInfo -> string list -> (obj * string list) option