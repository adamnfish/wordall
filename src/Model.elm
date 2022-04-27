module Model exposing (..)

import Process
import Task
import Wordle exposing (Entry, suggestWords, toEntry)
import Words


type alias Model =
    { seed : Int
    , lifecycle : Lifecycle
    }


type Msg
    = NoOp
    | Type String
    | Submit
    | UpdateAccuracy (List Entry)
    | RequestSuggestions
    | CalculateSuggestions


type Lifecycle
    = Welcome String
    | Solving (List Entry) (LoadingStatus (List String)) String


type LoadingStatus a
    = Empty
    | Loading
    | Data a


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
                    case toEntry firstWord of
                        Just firstEntry ->
                            ( { model | lifecycle = Solving [ firstEntry ] Empty "" }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Solving entries _ nextWord ->
                    case toEntry nextWord of
                        Just firstEntry ->
                            ( { model | lifecycle = Solving (List.append entries [ firstEntry ]) Empty "" }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

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
                    , Cmd.none
                    )

                Welcome _ ->
                    ( model, Cmd.none )
