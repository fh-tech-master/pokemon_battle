module Repl

open System
open Parser
open Domain

type Message =
    | DomainMessage of Types.Message
    | HelpRequested
    | NotParsable of string

let read (input: string) =
    match input with
    | ListAttack -> Types.ListAttack |> DomainMessage
    | Attack v -> Types.Attack v |> DomainMessage
    | Status -> Types.Status |> DomainMessage
    | Help -> HelpRequested
    | ParseFailed -> NotParsable input

open Microsoft.FSharp.Reflection

let createHelpText (): string =
    FSharpType.GetUnionCases typeof<Types.Message>
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")

let evaluate (update: Types.Message -> Types.State -> Types.Response) (state: Types.State) (msg: Message) =
    match msg with
    | DomainMessage msg ->
        let response = update msg state
        let newState = response.State

        let message =
            match response.Message with
            | Some (m) -> sprintf "%s" m
            | None -> sprintf "The message was %A. New state is %A" msg newState

        (newState, message)
    | HelpRequested ->
        let message = createHelpText ()
        (state, message)
    | NotParsable originalInput ->
        let message =
            sprintf
                """"%s" was not parsable. %s"""
                originalInput
                "You can get information about known commands by typing \"Help\""

        (state, message)

let print (state: Types.State, outputToPrint: string) =
    printfn "%s\n" outputToPrint
    if not state.Finished then printf "> "
    state

let rec loop (state: Types.State) =
    if not state.Finished then
        Console.ReadLine()
        |> read
        |> evaluate Logic.update state
        |> print
        |> loop
