namespace Domain

module Types =
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

    type Action = AttackAction of attacker: Pokemon * defender: Pokemon * attack: PokemonAttack * damageDealt: int

    type GameMode = 
        | NotSelected
        | Random
        | Selection

    type Phase =
        | Fighting
        | Neutral

    type State =
        { Pokemon1: Pokemon
          Pokemon2: Pokemon
          Actions: Action list
          Phase: Phase
          GameMode: GameMode }

    type Response =
        { State: State
          Message: string option }

    type SelectGameModeRequest =
        | RandomSelect
        | SelectionSelect of int

    type Message =
        | SelectGameMode of SelectGameModeRequest
        | StartFight
        | Escape
        | ListAttack
        | Attack of int
        | Status
