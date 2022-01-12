module Corpuses exposing (..)

import Corpuses.HarryPotterSpells as HarryPotterSpells
import Corpuses.Lotr as Lotr
import Corpuses.Namur as Namur
import Corpuses.Texas as Texas
import Dict exposing (Dict)


corpuses : Dict String ( String, String )
corpuses =
    [ ( "namur", ( "Belgium - Namur province", Namur.corpus ) )
    , ( "texas", ( "U.S. - Texas", Texas.corpus ) )
    , ( "hpspells", ( "Harry Potter Spells", HarryPotterSpells.corpus ) )
    , ( "lotr", ( "Lord of the Rings Locations", Lotr.corpus ) )
    , ( "zcustom", ( "Custom...", "" ) )
    ]
        |> Dict.fromList
