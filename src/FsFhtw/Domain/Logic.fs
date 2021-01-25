namespace Domain

module Logic =

    open Domain.Types

    let pokemons: Pokemon list =
        [ { Name = "Glumanda"
            Hp = 100
            InitialHp = 100
            Type = Fire
            Attacks =
                [ { Name = "Glut"
                    Type = Fire
                    Damage = 30 }
                  { Name = "Tackle"
                    Type = Normal
                    Damage = 20 } ] }
          { Name = "Quaputzi"
            Hp = 100
            InitialHp = 100
            Type = Water
            Attacks =
                [ { Name = "Aquaknarre"
                    Type = Water
                    Damage = 30 } ] }
          { Name = "Rattfratz"
            Hp = 100
            InitialHp = 100
            Type = Normal
            Attacks =
                [ { Name = "Tackle"
                    Type = Normal
                    Damage = 20 }
                  { Name = "Ruckzuckhieb"
                    Type = Normal
                    Damage = 25 } ] }
          { Name = "Bisasam"
            Hp = 100
            InitialHp = 100
            Type = Grass
            Attacks =
                [ { Name = "Rankenhieb"
                    Type = Grass
                    Damage = 30 }
                  { Name = "Tackle"
                    Type = Normal
                    Damage = 20 } ] }
          { Name = "Voltoball"
            Hp = 100
            InitialHp = 100
            Type = Electric
            Attacks =
                [ { Name = "Donnerschock"
                    Type = Electric
                    Damage = 30 }
                  { Name = "Tackle"
                    Type = Normal
                    Damage = 20 } ] } ]

    let printAllPokemons =
        pokemons
        |> List.mapi (fun i p -> sprintf "(%i) %s [%s] - %i HP" i p.Name (string p.Type) p.InitialHp)
        |> List.fold (fun s n -> s + "\n" + n) ""


    let actionToString a: string =
        match a with
        | AttackAction (attacker, defender, attack, damageDealt) ->
            sprintf
                "%s (%d hp) attacked %s (%d hp) with %s and dealt %d damage"
                attacker.Name
                attacker.Hp
                defender.Name
                defender.Hp
                attack.Name
                damageDealt

    let randomInRange s e = System.Random().Next(s, e)

    let selectRandomPokemon' (): Pokemon =
        pokemons.[randomInRange 0 (List.length pokemons)]

    let selectRandomAttack p: PokemonAttack =
        p.Attacks.[randomInRange 0 (List.length p.Attacks)]

    let listAttack (state: State) (p: Pokemon) =
        let attackList = 
          p.Attacks
          |> List.mapi (fun i a -> sprintf "(%i) %s [%s] - %i Damage" i a.Name (string a.Type) a.Damage)
          |> List.fold (fun s n -> s + "\n" + n) ""
        { State = state
          Message = Some(attackList) }

    let statusMessage (p: Pokemon) =
        sprintf "%s has %i/%i HP" p.Name p.Hp p.InitialHp

    let status (state: State): Response =
        let message = sprintf "Your Pokemon %s\nCPU`s Pokemon %s" (statusMessage state.Pokemon1) (statusMessage state.Pokemon2)
        { State = state
          Message =  Some(message) }

    let calculateAttackAction (attacker: Pokemon) (defender: Pokemon) (attack: PokemonAttack): Action =
        let baseDamage = attack.Damage

        let resultingDamage =
            match (attack.Type, defender.Type) with
            | (Normal, _) -> baseDamage
            | (Water, Fire) -> 2 * baseDamage
            | (Water, Water) -> int (0.5 * float baseDamage)
            | (Water, Grass) -> int (0.5 * float baseDamage)
            | (Water, _) -> baseDamage
            | (Fire, Fire) -> int (0.5 * float baseDamage)
            | (Fire, Water) -> int (0.5 * float baseDamage)
            | (Fire, Grass) -> 2 * baseDamage
            | (Fire, _) -> baseDamage
            | (Grass, Fire) -> int (0.5 * float baseDamage)
            | (Grass, Grass) -> int (0.5 * float baseDamage)
            | (Grass, Water) -> 2 * baseDamage
            | (Grass, Electric) -> int (0.25 * float baseDamage)
            | (Grass, _) -> baseDamage
            | (Electric, Water) -> 2 * baseDamage
            | (Electric, Grass) -> int (0.5 * float baseDamage)
            | (Electric, Electric) -> int (0.5 * float baseDamage)
            | (Electric, _) -> baseDamage

        AttackAction(attacker, defender, attack, resultingDamage)

    let performPlayerAttack (state: State) (selectedAttack: PokemonAttack): State =
        let action =
            calculateAttackAction state.Pokemon1 state.Pokemon2 selectedAttack

        match action with
        | AttackAction (_, _, _, damageDealt) ->
            { state with
                  Actions = action :: state.Actions
                  Pokemon2 =
                      { state.Pokemon2 with
                            Hp = state.Pokemon2.Hp - damageDealt } }

    let performCpuAttack (state: State): State =
        let attack = selectRandomAttack state.Pokemon2

        let action =
            calculateAttackAction state.Pokemon2 state.Pokemon1 attack

        match action with
        | AttackAction (_, _, _, damageDealt) ->
            { state with
                  Actions = action :: state.Actions
                  Pokemon1 =
                      { state.Pokemon1 with
                            Hp = state.Pokemon1.Hp - damageDealt } }

    let attackUpdate (state: State) (attackId: int): Response =
        let pokemon1Attacks = state.Pokemon1.Attacks

        let newAttackId =
            if attackId < pokemon1Attacks.Length && attackId >= 0 then
                attackId
            else
                0

        let selectedAttack = pokemon1Attacks.Item(newAttackId)

        let stateAfterPlayerAttack = performPlayerAttack state selectedAttack

        let messagePlayerAttack =
            actionToString (List.head stateAfterPlayerAttack.Actions)

        match stateAfterPlayerAttack.Pokemon2.Hp with
        | hp when hp > 0 ->
            let stateAfterCpuAttack = performCpuAttack stateAfterPlayerAttack

            let messageCpuAttack =
                sprintf "%s\n%s" messagePlayerAttack (actionToString (List.head stateAfterCpuAttack.Actions))

            match stateAfterCpuAttack.Pokemon1.Hp with
            | hp when hp > 0 ->
                { State = stateAfterCpuAttack
                  Message = Some messageCpuAttack }
            | _ ->
                let messagePlayerLost =
                    sprintf "%s\nYou have lost the fight. Better luck next time!" messageCpuAttack

                let pokemon2 = { state.Pokemon2 with Hp = state.Pokemon2.InitialHp }
                { State = { stateAfterCpuAttack with Phase = Neutral; Pokemon2 = pokemon2; GameMode = NotSelected }
                  Message = Some messagePlayerLost }
        | _ ->
            let messagePlayerWon =
                sprintf
                    "%s\nCongratulations you have defeated %s"
                    messagePlayerAttack
                    stateAfterPlayerAttack.Pokemon2.Name
            
            let pokemon1 = { state.Pokemon1 with Hp = state.Pokemon1.InitialHp }
            { State = { stateAfterPlayerAttack with Phase = Neutral; Pokemon1 = pokemon1; Pokemon2 = selectRandomPokemon' () }
              Message = Some messagePlayerWon }

    let init (): State =
        { Pokemon1 = selectRandomPokemon' ()
          Pokemon2 = selectRandomPokemon' ()
          Actions = []
          Phase = Neutral
          GameMode = NotSelected }

    let messageUsableInGameMode (state: State) msg = 
        match state.GameMode with 
        | NotSelected -> 
            match msg with
            | SelectGameMode _ -> true
            | _ -> false
        | _ ->
            match state.Phase with
            | Fighting ->
                match msg with
                | Escape | ListAttack | Status | Attack _ -> true
                | _ -> false
            | Neutral ->
                match msg with 
                | StartFight | ListAttack | Status -> true
                | _ -> false

    let selectPlayerPokemonById (id: int): Option<Pokemon> = 
        match id with
        | i when i >= 0 && i < pokemons.Length -> Some (pokemons.Item i)
        | _ -> None

    let escapeFight (state: State): Response =
        let pokemon1 = { state.Pokemon1 with Hp = state.Pokemon1.InitialHp }
        let pokemon2 = { state.Pokemon2 with Hp = state.Pokemon2.InitialHp }
        { State = { state with Phase = Neutral; Pokemon1 = pokemon1; Pokemon2 = pokemon2 }
          Message = Some("You have escaped the fight") }

    let startFight (state: State): Response =
        { State = { state with Phase = Fighting }
          Message = Some("You have started the fight") }

    let selectGameMode (state: State) (request: SelectGameModeRequest): Response =
        let message = sprintf "Your opponent was given %s" state.Pokemon2.Name
        match request with
        | RandomSelect -> 
            let playerPokemon = selectRandomPokemon' ()
            { State = { state with GameMode = Random; Pokemon1 = playerPokemon}; Message = Some(sprintf "You have selected the random gamemode. Luck chose %s as your pokemon\n%s" playerPokemon.Name message)}
        | SelectionSelect pokemonId -> 
            let pokemon = selectPlayerPokemonById pokemonId
            match pokemon with
            | Some p -> { State = { state with GameMode = Selection; Pokemon1 = p }; Message = Some(sprintf "You have selected %s as your pokemon\n%s" p.Name message)}
            | None -> { State = { state with GameMode = NotSelected}; Message = Some("You have selected an invalid pokemon")}
            
    let update (msg: Message) (model: State): Response =
        if not (messageUsableInGameMode model msg) then
            { State = model; Message = Some "Unable to perform this message"}
        else match msg with
              | SelectGameMode r -> selectGameMode model r
              | StartFight       -> startFight model
              | Escape           -> escapeFight model
              | ListAttack       -> listAttack model model.Pokemon1
              | Status           -> status model
              | Attack x         -> attackUpdate model x
