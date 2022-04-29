module Model exposing (..)

import Browser.Dom as Dom
import Process
import Task
import Wordle exposing (Entry, allEntriesGrey, isAlreadySolved, suggestWords, toEntry)
import Words


type alias Model =
    { seed : Int
    , lifecycle : Lifecycle
    }


type Msg
    = NoOp
    | Type String
    | Submit
    | SubmitWord String
    | UpdateAccuracy (List Entry)
    | RequestSuggestions
    | CalculateSuggestions


type Lifecycle
    = Welcome String
    | Solving (List Entry) (LoadingStatus (List String)) String


type LoadingStatus a
    = Empty
    | Loading
    | Error String
    | Data a


inputId =
    "word-input"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Type input ->
            case model.lifecycle of
                Welcome _ ->
                    ( { model | lifecycle = Welcome input }
                    , Cmd.none
                    )

                Solving entries maybeSuggestions _ ->
                    ( { model | lifecycle = Solving entries maybeSuggestions input }
                    , Cmd.none
                    )

        Submit ->
            case model.lifecycle of
                Welcome firstWord ->
                    addEntry model [] firstWord

                Solving entries _ nextWord ->
                    addEntry model entries nextWord

        SubmitWord word ->
            case model.lifecycle of
                Welcome _ ->
                    addEntry model [] word

                Solving entries _ _ ->
                    addEntry model entries word

        UpdateAccuracy newEntries ->
            case model.lifecycle of
                Solving _ _ nextWord ->
                    ( { model | lifecycle = Solving newEntries Empty nextWord }
                    , Cmd.none
                    )

                Welcome _ ->
                    ( model, Cmd.none )

        RequestSuggestions ->
            case model.lifecycle of
                Solving entries _ nextWord ->
                    if allEntriesGrey entries then
                        let
                            errorMessage =
                                "Have another guess first, or click on the letters above and set their colour to match what Wordle said."
                        in
                        ( { model | lifecycle = Solving entries (Error errorMessage) nextWord }
                        , Cmd.none
                        )

                    else if isAlreadySolved entries then
                        ( { model | lifecycle = Solving entries (Error "This Wordle is already solved!") nextWord }
                        , Cmd.none
                        )

                    else
                        ( { model | lifecycle = Solving entries Loading nextWord }
                        , Task.perform (always CalculateSuggestions) (Process.sleep 100)
                        )

                Welcome _ ->
                    ( model, Cmd.none )

        CalculateSuggestions ->
            case model.lifecycle of
                Solving entries _ nextWord ->
                    let
                        suggestions =
                            List.sort <| suggestWords Words.words entries
                    in
                    ( { model | lifecycle = Solving entries (Data suggestions) nextWord }
                    , focusInput
                    )

                Welcome _ ->
                    ( model, Cmd.none )


addEntry : Model -> List Entry -> String -> ( Model, Cmd Msg )
addEntry model entries word =
    case toEntry word of
        Just nextEntry ->
            ( { model | lifecycle = Solving (List.append entries [ nextEntry ]) Empty "" }
            , focusInput
            )

        Nothing ->
            ( model, Cmd.none )


focusInput : Cmd Msg
focusInput =
    Task.attempt (always NoOp) (Dom.focus inputId)
