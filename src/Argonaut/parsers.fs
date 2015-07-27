module internal Argonaut.Parsers

open System
open System.Globalization

let private mkParser<'T> (parse: string * CultureInfo -> 'T): Type * ArgParser =
  typeof<'T>, (fun c args -> match args with | arg :: t -> Some (parse (arg, c) :> obj), t | [] -> None, [])

let private standard = 
  dict [
    mkParser (fun (s, _) -> s) // string
    mkParser Int32.Parse ]

let defaultParsers t =
  match standard.TryGetValue t with
  | true, p -> Some p
  | false, _ -> None