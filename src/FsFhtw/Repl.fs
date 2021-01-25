module Repl

open System
open Parser
open Domain

type Message =
    | DomainMessage of Types.Message
    | HelpRequested

let read (input: string) =
    match input with
    | StartFight       -> Types.StartFight       |> DomainMessage
    | Escape           -> Types.Escape           |> DomainMessage
    | ListAttack       -> Types.ListAttack       |> DomainMessage
    | Attack v         -> Types.Attack v         |> DomainMessage
    | SelectGameMode v -> Types.SelectGameMode v |> DomainMessage
    | Status           -> Types.Status           |> DomainMessage
    | ParseFailed      -> HelpRequested

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

let neutralPhaseText =
    sprintf 
        "%s\n %s\n %s\n %s\n"
        "Please enter one of the following commands" 
        " - StartFight (starts the pokemon fight)"
        " - ListAttack (list all available attacks of your pokemon)" 
        " - Status (shows the current state of the fight)"

let fightingPhaseText = 
    sprintf 
        "%s\n %s\n %s\n %s\n %s\n"
        "Please enter one of the following commands" 
        " - ListAttack (list all available attacks of your pokemon)" 
        " - Attack [index] (your Pokemon attacks the enemy with the given Attack)" 
        " - Status (shows the current state of the fight)"
        " - Escape (leaves the fight and resets Pokemon HP)"

let selectGameModeText =
    sprintf
        "%s\n%s\n%s"
        " - SelectGameMode Random (to let the program choose which pokemon have to fight)"
        " - SelectGameMode Selection [index] (to choose your pokemon from the list)"
        Logic.printAllPokemons
    
let gameText (state: Types.State) =
    match state.GameMode with
    | Types.GameMode.NotSelected -> 
        selectGameModeText
    | _ -> match state.Phase with 
           | Types.Phase.Fighting -> fightingPhaseText
           | Types.Phase.Neutral -> neutralPhaseText
                   
let print (state: Types.State, outputToPrint: string) =
    printfn "%s\n" outputToPrint
    printfn "%s\n" (gameText state)
    printf "> "
    state

let rec loop (state: Types.State) =
      Console.ReadLine()
      |> read
      |> evaluate Logic.update state
      |> print
      |> loop
