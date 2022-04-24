module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Wordle exposing (..)


main : Program () { seed : Int, lifecycle : Lifecycle } Msg
main =
    Browser.document
        { init =
            \_ ->
                ( { seed = 0
                  , lifecycle = Welcome ""
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions =
            \_ ->
                Sub.none
        }


type alias Model =
    { seed : Int
    , lifecycle : Lifecycle
    }


type Msg
    = NoOp
    | Type String
    | Submit
    | UpdateAccuracy (List Entry)


type Lifecycle
    = Welcome String
    | Solving (List Entry) String


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

                Solving entries _ ->
                    ( { model | lifecycle = Solving entries input }
                    , Cmd.none
                    )

        Submit ->
            case model.lifecycle of
                Welcome firstWord ->
                    case toEntry firstWord of
                        Just firstEntry ->
                            ( { model | lifecycle = Solving [ firstEntry ] "" }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Solving entries nextWord ->
                    case toEntry nextWord of
                        Just firstEntry ->
                            ( { model | lifecycle = Solving (List.append entries [ firstEntry ]) "" }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

        UpdateAccuracy newEntries ->
            case model.lifecycle of
                Solving _ nextWord ->
                    ( { model | lifecycle = Solving newEntries nextWord }
                    , Cmd.none
                    )

                Welcome _ ->
                    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Wordall"
    , body =
        [ layout
            []
          <|
            ui model
        ]
    }


ui : Model -> Element Msg
ui model =
    Element.column
        [ width fill
        , spacing 25
        ]
        [ row
            [ width fill
            , padding 25
            , Background.color <| rgb255 40 50 60
            , Font.color <| rgba255 230 240 250 0.8
            , Font.size 30
            ]
            [ text "Wordall"
            ]
        , case model.lifecycle of
            Welcome firstWord ->
                welcomeUi model firstWord

            Solving entries nextWord ->
                solvingUi model entries nextWord
        ]


welcomeUi : Model -> String -> Element Msg
welcomeUi model firstWord =
    column
        [ width <| px 400
        , centerX
        ]
        [ row
            [ spacing 15 ]
            [ Input.text
                []
                { onChange = Type
                , text = firstWord
                , placeholder = Nothing
                , label =
                    Input.labelBelow
                        []
                    <|
                        text "Input word"
                }
            , Input.button
                [ padding 15
                , Background.color <| rgb255 40 50 60
                , Font.color <| rgba255 230 240 250 0.8
                , alignTop
                ]
                { onPress = Just Submit
                , label = text "Start"
                }
            ]
        ]


solvingUi : Model -> List Entry -> String -> Element Msg
solvingUi model entries nextWord =
    Element.none
