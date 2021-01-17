module Parser

open System

let safeEquals (it: string) (theOther: string) =
    String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

[<Literal>]
let HelpLabel = "Help"

let (|ListAttack|Attack|Help|ParseFailed|) (input: string) =
    let tryParseInt (arg: string) valueConstructor =
        let (worked, arg') = Int32.TryParse arg

        if worked then
            valueConstructor arg'
        else
            ParseFailed

    let parts = input.Split(' ') |> List.ofArray

    match parts with
    | [ verb ] when safeEquals verb ("ListAttack") -> ListAttack
    | [ verb; arg ] when safeEquals verb ("Attack") -> tryParseInt arg (fun value -> Attack value)
    | [ verb ] when safeEquals verb ("Help") -> Help
    | _ -> ParseFailed
