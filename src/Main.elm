module Main exposing (..)

import Browser
import Corpuses exposing (corpuses)
import Dict
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, fieldset, label, legend, option, p, s, select, text, textarea)
import Html.Attributes exposing (href, style, target, type_)
import Html.Events exposing (onClick, onInput)
import List.Extra
import NGram exposing (NGram, sampler, train)
import Random
import Task
import WordModel exposing (Corpus)


value_ : String -> Html.Attribute msg
value_ =
    Html.Attributes.value


{-| Transform a raw corpus into phrases that can be used for training.

Converts the whole text to lowercase, splits it at new lines, trims each line,
and remove any empty line.

-}
prepareCorpus : String -> Corpus
prepareCorpus corpus =
    String.toLower corpus
        |> String.lines
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { wordModel : Maybe NGram
    , corpus : Corpus
    , samples : List String

    -- form fields
    , fieldPreset : String
    , fieldCorpus : String
    , fieldN : String
    , resultsCapitalize : Bool
    , resultsExcludeCorpus : Bool

    -- errors
    , resultError : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { wordModel = Nothing
            , corpus = []
            , samples = []
            , fieldPreset = ""
            , fieldCorpus = ""
            , fieldN = "3"
            , resultsCapitalize = True
            , resultsExcludeCorpus = True
            , resultError = False
            }
    in
    update (SetFieldPreset "namur") model



-- UPDATE


type Msg
    = GenerateSample
    | NewSample (Maybe String)
    | CorpusFileRequested
    | CorpusFileSelected File
    | SetFieldPreset String
    | SetFieldCorpus String
    | SetFieldN String
    | ToggleResultsCapitalize
    | ToggleResultsExcludeCorpus


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateSample ->
            let
                -- Retrain the model if necessary
                ( corpus, wordModel ) =
                    case model.wordModel of
                        Nothing ->
                            let
                                newCorpus =
                                    prepareCorpus model.fieldCorpus

                                -- TODO parsing error?
                                n =
                                    String.toInt model.fieldN |> Maybe.withDefault 1
                            in
                            ( newCorpus, train n newCorpus )

                        Just x ->
                            ( model.corpus, x )

                newModel =
                    { model | corpus = corpus, wordModel = Just wordModel }

                cmd =
                    sampler wordModel
                        |> resampleUntil 100 (verifySample newModel)
                        |> Random.generate NewSample
            in
            ( newModel, cmd )

        NewSample maybeSample ->
            case maybeSample of
                Nothing ->
                    ( { model | resultError = True }, Cmd.none )

                Just sample ->
                    let
                        capitalized =
                            if model.resultsCapitalize then
                                capitalize sample

                            else
                                sample
                    in
                    ( { model
                        | resultError = False
                        , samples = capitalized :: model.samples
                      }
                    , Cmd.none
                    )

        CorpusFileRequested ->
            ( model
            , Select.file [ "text/plain" ] CorpusFileSelected
            )

        CorpusFileSelected file ->
            ( model
            , Task.perform SetFieldCorpus (File.toString file)
            )

        SetFieldPreset fieldPreset ->
            ( { model
                | fieldPreset = fieldPreset
                , fieldCorpus =
                    Dict.get fieldPreset corpuses
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault ""

                -- Set the model to `Nothing` to trigger retraining at next use
                , wordModel = Nothing
              }
            , Cmd.none
            )

        SetFieldCorpus fieldCorpus ->
            ( { model
                | fieldPreset = "zcustom"
                , fieldCorpus = fieldCorpus

                -- Set the model to `Nothing` to trigger retraining at next use
                , wordModel = Nothing
              }
            , Cmd.none
            )

        SetFieldN fieldN ->
            -- Set the model to `Nothing` to trigger retraining at next use
            ( { model | fieldN = fieldN, wordModel = Nothing }, Cmd.none )

        ToggleResultsCapitalize ->
            ( { model | resultsCapitalize = not model.resultsCapitalize }, Cmd.none )

        ToggleResultsExcludeCorpus ->
            ( { model | resultsExcludeCorpus = not model.resultsExcludeCorpus }, Cmd.none )


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


{-| Verify that a sample is valid.

Currently, only checks that the sample isn't in the corpus.

-}
verifySample : Model -> String -> Bool
verifySample model sample =
    not (model.resultsExcludeCorpus && List.any ((==) sample) model.corpus)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "padding" "1em" ]
        [ Html.h1 [] [ text "Phrase Generator" ]
        , fieldset []
            [ legend [] [ text "Corpus" ]
            , p [] [ text "A corpus is the list of words and phrases that the model will learn from, and try to emulate." ]

            -- Preset selection
            , p []
                [ label [ Html.Attributes.for "presets" ] [ text "Select an existing corpus: " ]
                , select [ Html.Attributes.id "presets", onInput SetFieldPreset ] <|
                    (Dict.toList corpuses
                        |> List.map
                            (\( name, ( title, _ ) ) ->
                                option
                                    [ value_ name
                                    , Html.Attributes.selected (name == model.fieldPreset)
                                    ]
                                    [ text title ]
                            )
                    )
                ]

            -- Load corpus from file
            , p []
                [ label [ Html.Attributes.for "load-from-file" ] [ text "Or load one from a file: " ]
                , button [ Html.Attributes.id "load-from-file", onClick CorpusFileRequested ] [ text "Load corpus from file" ]
                ]

            -- Corpus textarea
            , p []
                [ label [ Html.Attributes.for "corpus" ] [ text "Or modify the current one right here: " ]
                , Html.br [] []
                , textarea [ Html.Attributes.id "corpus", Html.Attributes.rows 7, onInput SetFieldCorpus, value_ model.fieldCorpus ] []
                ]
            ]
        , fieldset []
            [ legend [] [ text "Model (N-Gram)" ]
            , p []
                [ text "This model generates phrases character-by-character.  It uses the previous "
                , Html.em [] [ text "N" ]
                , text " characters to make a decision about the next one, based on what it learnt from the corpus."
                ]

            -- N parameter
            , p []
                [ label [ Html.Attributes.for "param-n" ] [ Html.em [] [ text "N" ], text " = " ]
                , Html.input
                    [ Html.Attributes.id "param-n"
                    , type_ "number"
                    , style "width" "2.5em"
                    , style "text-align" "right"
                    , Html.Attributes.min "1"
                    , value_ model.fieldN
                    , onInput SetFieldN
                    ]
                    []
                ]
            , p []
                [ text "Smaller "
                , Html.em [] [ text "N" ]
                , text " gives more diversity, but the results will also be more nonsensical."
                ]
            ]
        , fieldset []
            [ legend [] [ text "Post-processing" ]

            -- Capitalize
            , p []
                [ Html.input [ Html.Attributes.id "param-capitalize", type_ "checkbox", Html.Attributes.checked model.resultsCapitalize, onClick ToggleResultsCapitalize ] []
                , label [ Html.Attributes.for "param-capitalize" ] [ text " Capitalize results" ]
                ]

            -- Exclude corpus
            , p []
                [ Html.input [ Html.Attributes.id "param-exclude-corpus", type_ "checkbox", Html.Attributes.checked model.resultsExcludeCorpus, onClick ToggleResultsExcludeCorpus ] []
                , label [ Html.Attributes.for "param-exclude-corpus" ] [ text " Exclude results that exist in the corpus" ]
                ]
            ]
        , fieldset [] <|
            [ legend [] [ text "Results" ]

            -- "Generate" button
            , p [] [ button [ onClick GenerateSample ] [ text "Generate!" ] ]
            ]
                -- "Could not generate" error message
                ++ viewWhen model.resultError
                    (p
                        [ style "color" "darkred" ]
                        [ text "Could not generate something not in the corpus.  Maybe try a less constrained model?" ]
                    )
                -- Samples
                ++ List.map (div [] << List.singleton << text) model.samples
        , p
            [ style "font-size" "0.9em", style "text-align" "right" ]
            [ Html.a
                [ href "https://github.com/xlambein/phrase-generator", target "_blank" ]
                [ text "Source code on GitHub :>" ]
            ]
        ]


{-| Conditionally display an HTML element.
-}
viewWhen : Bool -> Html msg -> List (Html msg)
viewWhen shouldDisplay elem =
    if shouldDisplay then
        [ elem ]

    else
        []
