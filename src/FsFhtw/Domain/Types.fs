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

    type State =
        { Pokemon1: Pokemon
          Pokemon2: Pokemon
          Actions: Action list
          Finished: bool }

    type Response =
        { State: State
          Message: string option }

    type Message =
        | ListAttack
        | Attack of int
        | Status
