module NGram exposing (..)

import Dict exposing (Dict)
import Dict.Any exposing (AnyDict)
import List.Extra
import Random
import WordModel exposing (Corpus)


{-| Sorting function for `Dict.Any` keyed by `Maybe Char`
-}
maybeCharKey : Maybe Char -> Int
maybeCharKey c =
    Maybe.map Char.toCode c |> Maybe.withDefault 0 |> (+) 1


{-| N-gram generation rules.

Each N-gram (represented as a `String`) is mapped to a dictionary, itself
mapping characters (`Maybe Char`) to frequencies (`Int`).

Start-of-phrase is represented by an empty N-gram, `""`.

End-of-phrase is represented by `Nothing`.

-}
type alias Rules =
    Dict String (AnyDict Int (Maybe Char) Int)



-- type alias Params =
--     { n : Int }


{-| N-gram based phrase generator.
-}
type alias NGram =
    { n : Int
    , rules : Rules
    }


{-| Create an empty N-gram model.
-}
empty : Int -> NGram
empty n =
    { n = n
    , rules = Dict.empty
    }


{-| Train a new N-gram model from a corpus.
-}
train : Int -> Corpus -> NGram
train n corpus =
    update corpus (empty n)


{-| Update an existing N-gram model from a corpus.
-}
update : Corpus -> NGram -> NGram
update corpus model =
    case corpus of
        [] ->
            model

        word :: rest ->
            updateWithPhrase word model
                |> update rest


{-| Update an existing N-gram model with a given phrase.
-}
updateWithPhrase : String -> NGram -> NGram
updateWithPhrase word { n, rules } =
    { n = n, rules = List.foldl updateRulesWithNGram rules (strToNGrams n word) }


{-| Convert a phrase into a list of N-gram.

    strToNGrams 2 "foo"
        == [ ( "", Just 'f' ), ( "f", Just 'o' ), ( "fo", Just 'o' ), ( "oo", Nothing ) ]

-}
strToNGrams : Int -> String -> List ( String, Maybe Char )
strToNGrams n str =
    let
        helper : Int -> List ( String, Maybe Char )
        helper i =
            let
                -- N-gram before position `i`
                -- Might be smaller than `n` if `i < n`.
                prev =
                    String.slice (max 0 (i - n)) i str

                -- Character at `i`, if there is one.
                next =
                    String.dropLeft i str
                        |> String.uncons
                        |> Maybe.map Tuple.first
            in
            case next of
                Nothing ->
                    -- Last N-gram of the string
                    [ ( prev, next ) ]

                Just _ ->
                    -- There's more: recurse!
                    ( prev, next ) :: helper (i + 1)
    in
    helper 0


{-| Update the rules of an N-gram model with a specific N-gram.
-}
updateRulesWithNGram : ( String, Maybe Char ) -> Rules -> Rules
updateRulesWithNGram ( prev, next ) rules =
    let
        -- Increment the frequency of the given N-gram.
        incrementRule value =
            Maybe.withDefault (Dict.Any.empty maybeCharKey) value
                |> Dict.Any.update next (\x -> Just (Maybe.withDefault 0 x + 1))
                |> Just
    in
    Dict.update prev incrementRule rules


{-| Make a generator from an N-gram model.

Running the generator samples the N-gram model, producing a phrase based on
the frequency of letters in the source corpus.

-}
sampler : NGram -> Random.Generator String
sampler { n, rules } =
    let
        helper : String -> Random.Generator String
        helper soFar =
            let
                -- Last N-gram
                prev =
                    String.right n soFar
            in
            case Dict.get prev rules of
                Nothing ->
                    -- No rules for last N-gram: end here
                    Random.constant soFar

                Just d ->
                    -- Otherwise: sample the dictionary
                    dictionarySampler d
                        |> Random.andThen
                            (\maybeC ->
                                case maybeC of
                                    Nothing ->
                                        -- End of phrase: return what we have
                                        Random.constant soFar

                                    Just c ->
                                        -- Character: continue building recursively
                                        helper (soFar ++ String.fromChar c)
                            )
    in
    helper ""


{-| Make a generator from a dictionary that maps characters to frequencies.

Sampling the generator produces a single character from the dictionary,
according to its frequency.

-}
dictionarySampler : AnyDict Int (Maybe Char) Int -> Random.Generator (Maybe Char)
dictionarySampler d =
    let
        counts =
            Dict.Any.toList d |> List.map (\( a, b ) -> ( toFloat b, a ))
    in
    List.Extra.uncons counts
        |> Maybe.map (\( head, tail ) -> Random.weighted head tail)
        |> Maybe.withDefault (Random.constant Nothing)
