open Domain.Logic

[<EntryPoint>]
let main argv =
    printfn "Welcome to the Pokemon Arena!"
    printfn "Please enter one of the following commands to fight.\n"
    printfn " - ListAttack (list all available attacks of your pokemon)"
    printfn " - Attack [index] (your Pokemon attacks the enemy with the given Attack)"
    printfn " - Status (shows the current state of the fight)"
    printfn " - Help (lists all available commands)\n"

    let initialState = init ()
    printfn "Luck chose %s as your pokemon" initialState.Pokemon1.Name
    printfn "Your opponent was given %s\n" initialState.Pokemon2.Name
    printfn "Press CTRL+C to stop the program.\n"
    printf "> "
    Repl.loop initialState
    0 // return an integer exit code
