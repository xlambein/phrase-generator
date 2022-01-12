module Tests exposing (..)

import Dict
import Dict.Any
import Expect
import List.Extra
import Main exposing (prepareCorpus)
import NGram exposing (dictionarySampler, maybeCharKey, sampler, strToNGrams, updateRulesWithNGram)
import Random
import Set
import Test exposing (..)


testPrepareCorpus : Test
testPrepareCorpus =
    describe "prepareCorpus"
        [ test "corpus with extra lines, uppercase letters, and extra whitespace" <|
            \_ ->
                prepareCorpus "Hello\nwORld\n\n   foo quux \n BAR\n\n"
                    |> Expect.equal [ "hello", "world", "foo quux", "bar" ]
        , test "empty corpus" <|
            \_ ->
                prepareCorpus ""
                    |> Expect.equal []
        , test "whitespace-only corpus" <|
            \_ ->
                prepareCorpus "   \n\n   \n"
                    |> Expect.equal []
        ]


testStrToNGram : Test
testStrToNGram =
    describe "strToNGrams"
        [ test "3-gram on non-empty string" <|
            \_ ->
                strToNGrams 3 "abcd"
                    |> Expect.equal [ ( "", Just 'a' ), ( "a", Just 'b' ), ( "ab", Just 'c' ), ( "abc", Just 'd' ), ( "bcd", Nothing ) ]
        ]


testUpdateRulesWithNGram : Test
testUpdateRulesWithNGram =
    describe "updateRulesWithNGram"
        [ test "empty n-gram, end of word" <|
            \_ ->
                updateRulesWithNGram ( "", Nothing ) Dict.empty
                    |> Expect.equal (Dict.singleton "" (Dict.Any.singleton Nothing 1 maybeCharKey))
        , test "non-empty n-gram, end of word" <|
            \_ ->
                updateRulesWithNGram ( "abc", Nothing ) Dict.empty
                    |> Expect.equal (Dict.singleton "abc" (Dict.Any.singleton Nothing 1 maybeCharKey))
        , test "non-empty n-gram" <|
            \_ ->
                updateRulesWithNGram ( "abc", Just 'd' ) Dict.empty
                    |> Expect.equal (Dict.singleton "abc" (Dict.Any.singleton (Just 'd') 1 maybeCharKey))
        , test "empty n-gram" <|
            \_ ->
                updateRulesWithNGram ( "", Just 'd' ) Dict.empty
                    |> Expect.equal (Dict.singleton "" (Dict.Any.singleton (Just 'd') 1 maybeCharKey))
        , test "existing model, empty n-gram, end of word" <|
            \_ ->
                updateRulesWithNGram ( "", Nothing ) (Dict.singleton "" (Dict.Any.singleton Nothing 5 maybeCharKey))
                    |> Expect.equal (Dict.singleton "" (Dict.Any.singleton Nothing 6 maybeCharKey))
        , test "existing model, empty n-gram" <|
            \_ ->
                updateRulesWithNGram ( "", Just 'd' ) (Dict.singleton "" (Dict.Any.singleton (Just 'd') 5 maybeCharKey))
                    |> Expect.equal (Dict.singleton "" (Dict.Any.singleton (Just 'd') 6 maybeCharKey))
        ]


testNGramSampler : Test
testNGramSampler =
    describe "testNGramSampler"
        [ test "empty model" <|
            \_ ->
                Random.step (sampler { n = 1, rules = Dict.empty }) (Random.initialSeed 0)
                    |> Tuple.first
                    |> Expect.equal ""
        , test "empty string model" <|
            \_ ->
                let
                    rules =
                        Dict.fromList
                            [ ( "", Dict.Any.fromList maybeCharKey [ ( Nothing, 1 ) ] )
                            ]

                    gen =
                        sampler { n = 1, rules = rules }
                in
                Random.step gen (Random.initialSeed 0)
                    |> Tuple.first
                    |> Expect.equal ""
        , test "'bar' model" <|
            \_ ->
                let
                    rules =
                        Dict.fromList
                            [ ( "", Dict.Any.fromList maybeCharKey [ ( Just 'b', 1 ) ] )
                            , ( "b", Dict.Any.fromList maybeCharKey [ ( Just 'a', 1 ) ] )
                            , ( "a", Dict.Any.fromList maybeCharKey [ ( Just 'r', 1 ) ] )
                            , ( "r", Dict.Any.fromList maybeCharKey [ ( Nothing, 1 ) ] )
                            ]

                    gen =
                        sampler { n = 1, rules = rules }
                in
                Random.step gen (Random.initialSeed 0)
                    |> Tuple.first
                    |> Expect.equal "bar"
        , test "'quux' 2-gram model" <|
            \_ ->
                let
                    rules =
                        Dict.fromList
                            [ ( "", Dict.Any.fromList maybeCharKey [ ( Just 'q', 1 ) ] )
                            , ( "q", Dict.Any.fromList maybeCharKey [ ( Just 'u', 1 ) ] )
                            , ( "qu", Dict.Any.fromList maybeCharKey [ ( Just 'u', 1 ) ] )
                            , ( "uu", Dict.Any.fromList maybeCharKey [ ( Just 'x', 1 ) ] )
                            , ( "ux", Dict.Any.fromList maybeCharKey [ ( Nothing, 1 ) ] )
                            ]

                    gen =
                        sampler { n = 2, rules = rules }
                in
                Random.step gen (Random.initialSeed 0)
                    |> Tuple.first
                    |> Expect.equal "quux"
        , test "'bar' or 'quux' 2-gram model" <|
            \_ ->
                let
                    rules =
                        Dict.fromList
                            [ ( "", Dict.Any.fromList maybeCharKey [ ( Just 'b', 1 ), ( Just 'q', 1 ) ] )
                            , ( "b", Dict.Any.fromList maybeCharKey [ ( Just 'a', 1 ) ] )
                            , ( "ba", Dict.Any.fromList maybeCharKey [ ( Just 'r', 1 ) ] )
                            , ( "ar", Dict.Any.fromList maybeCharKey [ ( Nothing, 1 ) ] )
                            , ( "q", Dict.Any.fromList maybeCharKey [ ( Just 'u', 1 ) ] )
                            , ( "qu", Dict.Any.fromList maybeCharKey [ ( Just 'u', 1 ) ] )
                            , ( "uu", Dict.Any.fromList maybeCharKey [ ( Just 'x', 1 ) ] )
                            , ( "ux", Dict.Any.fromList maybeCharKey [ ( Nothing, 1 ) ] )
                            ]

                    gen =
                        sampler { n = 2, rules = rules }
                in
                List.Extra.scanl
                    (\_ ( str, seed ) -> Random.step gen seed)
                    ( "", Random.initialSeed 0 )
                    (List.range 0 100)
                    -- Remove initial empty string
                    |> List.drop 1
                    |> List.map Tuple.first
                    |> Set.fromList
                    |> Expect.equalSets (Set.fromList [ "bar", "quux" ])
        ]


testDictionarySampler : Test
testDictionarySampler =
    describe "dictionarySampler"
        [ test "empty dictionary" <|
            \_ ->
                dictionarySampler (Dict.Any.empty maybeCharKey)
                    |> (\gen -> Random.step gen (Random.initialSeed 0))
                    |> Tuple.first
                    |> Expect.equal Nothing
        , test "dictionary with Nothing" <|
            \_ ->
                dictionarySampler (Dict.Any.singleton Nothing 123 maybeCharKey)
                    |> (\gen -> Random.step gen (Random.initialSeed 0))
                    |> Tuple.first
                    |> Expect.equal Nothing
        , test "dictionary with Just 'c'" <|
            \_ ->
                dictionarySampler (Dict.Any.singleton (Just 'c') 456 maybeCharKey)
                    |> (\gen -> Random.step gen (Random.initialSeed 0))
                    |> Tuple.first
                    |> Expect.equal (Just 'c')
        , test "dictionary has expected distribution" <|
            \_ ->
                Dict.Any.fromList maybeCharKey [ ( Nothing, 1 ), ( Just 'c', 3 ) ]
                    |> dictionaryDistribution 10000
                    |> Dict.Any.get Nothing
                    |> Maybe.withDefault -1
                    |> Expect.within (Expect.Absolute 0.01) 0.25
        ]


{-| Approximate the distribution of a `dictionarySampler` by running it many times.
-}
dictionaryDistribution : Int -> Dict.Any.AnyDict Int (Maybe Char) Int -> Dict.Any.AnyDict Int (Maybe Char) Float
dictionaryDistribution n d =
    let
        gen =
            dictionarySampler d

        initialSeed =
            Random.initialSeed 0
    in
    List.foldl
        (\_ ( distrib, seed ) ->
            Random.step gen seed
                |> Tuple.mapFirst
                    (\c -> Dict.Any.update c (\x -> Maybe.withDefault 0 x + 1 |> Just) distrib)
        )
        ( Dict.Any.empty maybeCharKey, initialSeed )
        (List.range 0 n)
        |> Tuple.first
        |> Dict.Any.map (\_ count -> toFloat count / toFloat n)
