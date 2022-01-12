module PostProcessing exposing (..)

import List.Extra
import Random
import WordModel exposing (Corpus, WordModel)


type alias Params p =
    { excludeCorpus : Bool
    , capitalize : Bool
    , inner : p
    }


type alias Model m =
    { corpus : Corpus
    , excludeCorpus : Bool
    , capitalize : Bool
    , inner : m
    }


postProcessingWordModel : WordModel p m -> WordModel (Params p) (Model m)
postProcessingWordModel innerWM =
    { empty = empty innerWM.empty
    , train = train innerWM.train
    , sampler = sampler innerWM.sampler
    }


empty : (p -> m) -> Params p -> Model m
empty innerEmpty params =
    { corpus = []
    , excludeCorpus = params.excludeCorpus
    , capitalize = params.capitalize
    , inner = innerEmpty params.inner
    }


train : (Corpus -> m -> m) -> Corpus -> Model m -> Model m
train innerTrain corpus model =
    { model | corpus = model.corpus ++ corpus, inner = innerTrain corpus model.inner }


sampler : (m -> Random.Generator String) -> Model m -> Random.Generator String
sampler innerSampler model =
    innerSampler model.inner
        |> resampleUntil 100 (sampleIsValid model)
        |> Random.map (Maybe.withDefault "")
        |> Random.map
            (\str ->
                if model.capitalize then
                    capitalize str

                else
                    str
            )


{-| Verify that a sample is valid.

Currently, only checks that the sample isn't in the corpus.

-}
sampleIsValid : Model m -> String -> Bool
sampleIsValid model sample =
    not (model.excludeCorpus && List.any ((==) sample) model.corpus)


{-| Trigger a generator at most `maxAttempts` times, until `predicate` is True.
-}
resampleUntil : Int -> (a -> Bool) -> Random.Generator a -> Random.Generator (Maybe a)
resampleUntil maxAttempts predicate generator =
    generator
        |> Random.andThen
            (\s ->
                if predicate s then
                    Random.constant (Just s)

                else if maxAttempts == 1 then
                    Random.constant Nothing

                else
                    resampleUntil (maxAttempts - 1) predicate generator
            )


{-| Capitalize A String.

The heuristic is very basic: any character preceded by a space or a hyphen is
capitalized.

-}
capitalize : String -> String
capitalize str =
    String.toList str
        |> List.Extra.scanl
            (\current prev ->
                if prev == ' ' || prev == '-' then
                    Char.toUpper current

                else
                    current
            )
            ' '
        |> String.fromList
        |> String.dropLeft 1
