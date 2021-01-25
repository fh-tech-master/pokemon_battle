open Domain.Logic

[<EntryPoint>]
let main argv =
    printfn "Welcome to the Pokemon Arena!"
    printfn "%s" Repl.selectGameModeText
    let initialState = init ()
    printfn "Press CTRL+C to stop the program.\n"
    printf "> "
    Repl.loop initialState
    0 // return an integer exit code
