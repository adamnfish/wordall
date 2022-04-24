module View exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import List.Extra
import Model exposing (Lifecycle(..), Model, Msg(..))
import Wordle exposing (Accuracy(..), Entry(..), EntryChar)


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

            Solving entries maybeSuggestions nextWord ->
                solvingUi model entries maybeSuggestions nextWord
        ]


welcomeUi : Model -> String -> Element Msg
welcomeUi model firstWord =
    column
        [ width <| px 400
        , centerX
        , spacing 45
        ]
        [ nextEntryComponent firstWord
        , column
            [ padding 15
            , spacing 15
            , Background.color <| rgb255 210 210 80
            ]
            [ paragraph
                []
                [ text "Wordall is an assistant for Wordle. "
                ]
            , paragraph
                []
                [ text "Enter your guesses, and click on the letters to tell Wordall which colour they are. "
                , text "It will recommend possible answers for you!"
                ]
            ]
        ]


solvingUi : Model -> List Entry -> Maybe (List String) -> String -> Element Msg
solvingUi model entries maybeSuggestions nextWord =
    let
        updateAccuracyFn : Int -> Int -> Accuracy -> Msg
        updateAccuracyFn entryIndex entryCharIndex newAccuracy =
            let
                newEntries =
                    List.Extra.updateAt entryIndex
                        (\(Filled ec1 ec2 ec3 ec4 ec5) ->
                            if entryCharIndex == 0 then
                                Filled { ec1 | accuracy = newAccuracy } ec2 ec3 ec4 ec5

                            else if entryCharIndex == 1 then
                                Filled ec1 { ec2 | accuracy = newAccuracy } ec3 ec4 ec5

                            else if entryCharIndex == 2 then
                                Filled ec1 ec2 { ec3 | accuracy = newAccuracy } ec4 ec5

                            else if entryCharIndex == 3 then
                                Filled ec1 ec2 ec3 { ec4 | accuracy = newAccuracy } ec5

                            else if entryCharIndex == 4 then
                                Filled ec1 ec2 ec3 ec4 { ec5 | accuracy = newAccuracy }

                            else
                                Filled ec1 ec2 ec3 ec4 ec5
                        )
                        entries
            in
            UpdateAccuracy newEntries
    in
    column
        [ width <| px 400
        , centerX
        , spacing 25
        ]
        [ column
            [ spacing 5 ]
          <|
            List.indexedMap (\i -> entryComponent (updateAccuracyFn i)) entries
        , nextEntryComponent nextWord
        , case maybeSuggestions of
            Nothing ->
                Input.button
                    [ padding 15
                    , Background.color <| rgb255 40 50 60
                    , Font.color <| rgba255 230 240 250 0.8
                    , alignTop
                    ]
                    { onPress = Just RequestSuggestions
                    , label = text "Calculate suggestions"
                    }

            Just suggestions ->
                column
                    []
                <|
                    List.map text suggestions
        ]


entryComponent : (Int -> Accuracy -> Msg) -> Entry -> Element Msg
entryComponent msgFn (Filled ec1 ec2 ec3 ec4 ec5) =
    row
        [ spacing 8 ]
        [ entryCharComponent (msgFn 0) ec1
        , entryCharComponent (msgFn 1) ec2
        , entryCharComponent (msgFn 2) ec3
        , entryCharComponent (msgFn 3) ec4
        , entryCharComponent (msgFn 4) ec5
        ]


entryCharComponent : (Accuracy -> Msg) -> EntryChar -> Element Msg
entryCharComponent msgFn entryChar =
    let
        nextAccuracy =
            case entryChar.accuracy of
                Green ->
                    Yellow

                Yellow ->
                    Grey

                Grey ->
                    Green
    in
    Input.button
        [ width <| px 45
        , height <| px 45
        , Background.color <|
            case entryChar.accuracy of
                Green ->
                    rgb255 100 255 100

                Yellow ->
                    rgb255 255 255 100

                Grey ->
                    rgb255 40 50 60
        , Border.width 2
        , Border.color <|
            case entryChar.accuracy of
                Green ->
                    rgb255 50 50 50

                Yellow ->
                    rgb255 50 50 50

                Grey ->
                    rgb255 230 230 230
        , Font.color <|
            case entryChar.accuracy of
                Green ->
                    rgba255 50 50 50 0.8

                Yellow ->
                    rgba255 50 50 50 0.8

                Grey ->
                    rgba255 250 250 250 0.8
        ]
        { onPress = Just <| msgFn nextAccuracy
        , label =
            el [ centerX, centerY ] <|
                text (String.fromChar entryChar.char)
        }


nextEntryComponent : String -> Element Msg
nextEntryComponent currentEntry =
    row
        [ spacing 15 ]
        [ Input.text
            []
            { onChange = Type
            , text = currentEntry
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
            , label = text "Add guess"
            }
        ]
