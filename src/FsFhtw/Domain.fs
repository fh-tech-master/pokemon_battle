module Domain

type PokemonType =
    | Normal
    | Water
    | Fire
    | Grass
    | Electric

type PokemonAttack =
    { Name: string
      Type: PokemonType
      Damage: int }

type Pokemon =
    { Name: string
      Hp: int
      InitialHp: int
      Type: PokemonType
      Attacks: PokemonAttack list }

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

type Action = AttackAction of attacker: Pokemon * defender: Pokemon * attack: PokemonAttack * damageDealt: int

let actionToString a: string =
    match a with
    | AttackAction (attacker, defender, attack, damageDealt) ->
        sprintf
            "%s (%d hp) attacked %s (%d hp) with %s and dealt %d damage"
            //"%s (%d hp) attacked %s (%d hp) with %s and dealt %d damage (dbg: %A)"
            attacker.Name
            attacker.Hp
            defender.Name
            defender.Hp
            attack.Name
            damageDealt
//attacker

type State =
    { Pokemon1: Pokemon
      Pokemon2: Pokemon
      Actions: Action list
      Finished: bool }

type Response =
    { State: State
      Message: string option }

let randomInRange s e = System.Random().Next(s, e)

let selectRandomPokemon' (): Pokemon =
    pokemons.[randomInRange 0 (List.length pokemons)]

let selectRandomPokemon: Pokemon * Pokemon =
    (selectRandomPokemon' (), selectRandomPokemon' ())

let selectRandomAttack p: PokemonAttack =
    p.Attacks.[randomInRange 0 (List.length p.Attacks)]

let init (): State =
    let (pokemon1, pokemon2) = selectRandomPokemon

    { Pokemon1 = pokemon1
      Pokemon2 = pokemon2
      Actions = []
      Finished = false }

type Message =
    | ListAttack
    | Attack of int

let listAttack (p: Pokemon) =
    p.Attacks
    |> List.mapi (fun i a -> sprintf "(%i) %s [%s] - %i Damage" i a.Name (string a.Type) a.Damage)
    |> List.fold (fun s n -> s + "\n" + n) ""


let calculateAttackAction (attacker: Pokemon) (defender: Pokemon) (attack: PokemonAttack): Action =
    let baseDamage = attack.Damage

    let resultingDamage =
        match (attacker.Type, defender.Type) with
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

            { State =
                  { stateAfterCpuAttack with
                        Finished = true }
              Message = Some messagePlayerLost }
    | _ ->
        let messagePlayerWon =
            sprintf "%s\nCongratulations you have defeated %s" messagePlayerAttack stateAfterPlayerAttack.Pokemon2.Name

        { State =
              { stateAfterPlayerAttack with
                    Finished = true }
          Message = Some messagePlayerWon }

let update (msg: Message) (model: State): Response =
    match msg with
    | ListAttack ->
        { State = model
          Message = Some(listAttack model.Pokemon1) }
    | Attack x -> attackUpdate model x
