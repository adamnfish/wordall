module Model exposing (..)

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


type Lifecycle
    = Welcome String
    | Solving (List Entry) (Maybe (List String)) String


update : Msg -> Model -> ( Model, Cmd msg )
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
                            ( { model | lifecycle = Solving [ firstEntry ] Nothing "" }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Solving entries _ nextWord ->
                    case toEntry nextWord of
                        Just firstEntry ->
                            ( { model | lifecycle = Solving (List.append entries [ firstEntry ]) Nothing "" }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

        UpdateAccuracy newEntries ->
            case model.lifecycle of
                Solving _ maybeSuggestions nextWord ->
                    ( { model | lifecycle = Solving newEntries Nothing nextWord }
                    , Cmd.none
                    )

                Welcome _ ->
                    ( model, Cmd.none )

        RequestSuggestions ->
            case model.lifecycle of
                Solving entries _ nextWord ->
                    let
                        suggestions =
                            List.sort <| suggestWords Words.words entries
                    in
                    ( { model | lifecycle = Solving entries (Just suggestions) nextWord }
                    , Cmd.none
                    )

                Welcome _ ->
                    ( model, Cmd.none )
