module Repl

open System
open Parser

type Message =
    | DomainMessage of Domain.Message
    | HelpRequested
    | NotParsable of string

type State = Domain.State
type Response = Domain.Response

let read (input: string) =
    match input with
    | ListAttack -> Domain.ListAttack |> DomainMessage
    | Attack v -> Domain.Attack v |> DomainMessage
    | Help -> HelpRequested
    | ParseFailed -> NotParsable input

open Microsoft.FSharp.Reflection

let createHelpText (): string =
    FSharpType.GetUnionCases typeof<Domain.Message>
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")

let evaluate (update: Domain.Message -> State -> Response) (state: State) (msg: Message) =
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

let print (state: State, outputToPrint: string) =
    printfn "%s\n" outputToPrint
    printf "> "

    state

let rec loop (state: State) =
    if not state.Finished then
        Console.ReadLine()
        |> read
        |> evaluate Domain.update state
        |> print
        |> loop
