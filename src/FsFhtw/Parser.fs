module Parser

open System

open Domain.Types

let safeEquals (it: string) (theOther: string) =
    String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

let (|SelectGameMode|StartFight|Escape|ListAttack|Attack|Status|ParseFailed|) (input: string) =
    let tryParseInt (arg: string) valueConstructor =
        let (worked, arg') = Int32.TryParse arg

        if worked then
            valueConstructor arg'
        else
            ParseFailed

    let parts = input.Split(' ') |> List.ofArray

    match parts with
    | verb::args when safeEquals verb ("SelectGameMode") -> 
        match args with
        | a::xs when safeEquals a ("Random") -> SelectGameMode RandomSelect
        | a::xs when safeEquals a ("Selection") -> tryParseInt xs.Head (fun value -> SelectGameMode (SelectionSelect value))
        | _ -> ParseFailed
    | verb::args when safeEquals verb ("StartFight") -> StartFight
    | verb::args when safeEquals verb ("Escape") -> Escape
    | verb::args when safeEquals verb ("ListAttack") -> ListAttack
    | verb::args when safeEquals verb ("Attack") -> 
        match args with
        | v::xs -> tryParseInt v (fun value -> Attack value)
        | _ -> ParseFailed
    | verb::args when safeEquals verb ("Status") -> Status
    | _ -> ParseFailed



// Multi-Case Active Patterns cannot have more than 7 cases! So we ditched the Help Command and worked around it
