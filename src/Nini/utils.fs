[<AutoOpen>]
module internal Nini.Utils

open System.Text

module UTF8 =
  let enc = UTF8Encoding false

  let bytes (t: string) = enc.GetBytes t