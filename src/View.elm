module View exposing (view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import List.Extra
import Model exposing (Lifecycle(..), LoadingStatus(..), Model, Msg(..), inputId)
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
        , Font.family
            [ Font.typeface "Nunito" ]
        , Font.regular
        ]
        [ row
            [ width fill
            , padding 25
            , Background.color theme.dark
            , Font.color <| textColour theme.light
            , Font.size 30
            ]
            [ text "Wordall"
            ]
        , case model.lifecycle of
            Welcome firstWord ->
                welcomeUi model firstWord

            Solving entries suggestionsData nextWord ->
                solvingUi model entries suggestionsData nextWord
        ]


welcomeUi : Model -> String -> Element Msg
welcomeUi model firstWord =
    column
        [ width <| px 370
        , centerX
        , spacing 45
        ]
        [ nextEntryComponent firstWord
        , column
            [ padding 15
            , spacing 15
            , Background.color theme.medium
            , Font.color <| textColour theme.white
            , Border.rounded 4
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


solvingUi : Model -> List Entry -> LoadingStatus (List String) -> String -> Element Msg
solvingUi model entries suggestionsData nextWord =
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
        [ width <| px 370
        , centerX
        , spacing 25
        ]
        [ column
            [ spacing 9 ]
          <|
            List.indexedMap (\i -> entryComponent (updateAccuracyFn i)) entries
        , nextEntryComponent nextWord
        , case suggestionsData of
            Empty ->
                Input.button
                    [ padding 15
                    , Background.color theme.secondary
                    , Font.color <| textColour theme.white
                    , alignTop
                    ]
                    { onPress = Just RequestSuggestions
                    , label = text "Calculate suggestions"
                    }

            Loading ->
                el
                    [ width fill
                    , paddingXY 8 16
                    , Border.rounded 4
                    , Background.color theme.dark
                    , Font.color <| textColour theme.light
                    ]
                <|
                    text "Calculating possible answers..."

            Error message ->
                paragraph
                    [ width fill
                    , paddingXY 8 12
                    , Border.rounded 4
                    , Background.color theme.dark
                    , Font.color <| textColour theme.light
                    ]
                    [ text message
                    ]

            Data suggestions ->
                if List.isEmpty suggestions then
                    el
                        [ width fill
                        , paddingXY 8 12
                        , Border.rounded 4
                        , Background.color theme.dark
                        , Font.color <| textColour theme.light
                        ]
                    <|
                        text "No matching words found!"

                else
                    el
                        [ width fill
                        , padding 8
                        , Background.color theme.medium
                        , Border.rounded 4
                        ]
                    <|
                        wrappedRow
                            [ width fill
                            , spacing 8
                            ]
                        <|
                            List.map (suggestionNugget SubmitWord) suggestions
        ]


suggestionNugget : (String -> Msg) -> String -> Element Msg
suggestionNugget msgFn suggestion =
    el
        []
    <|
        Input.button
            [ padding 8
            , Background.color theme.secondary
            , Font.color <| textColour theme.white
            , Border.rounded 4
            ]
            { onPress = Just <| msgFn suggestion
            , label = text suggestion
            }



--el
--    [ padding 8
--    , Background.color theme.dark
--    , Font.color theme.light
--    , Border.rounded 4
--    ]
--<|
--    text suggestion


entryComponent : (Int -> Accuracy -> Msg) -> Entry -> Element Msg
entryComponent msgFn (Filled ec1 ec2 ec3 ec4 ec5) =
    row
        [ spacing 9 ]
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
        [ width <| px 67
        , height <| px 67
        , padding 1
        , Font.size 30
        , Font.extraBold
        , Background.color theme.light
        , Border.width 3
        , Border.color <|
            case entryChar.accuracy of
                Green ->
                    theme.secondary

                Yellow ->
                    theme.secondary

                Grey ->
                    theme.secondary
        , Font.color <|
            case entryChar.accuracy of
                Green ->
                    theme.white

                Yellow ->
                    theme.white

                Grey ->
                    theme.white
        ]
        { onPress = Just <| msgFn nextAccuracy
        , label =
            el
                [ width fill
                , height fill
                , Background.color <|
                    case entryChar.accuracy of
                        Green ->
                            theme.wordleGreen

                        Yellow ->
                            theme.wordleYellow

                        Grey ->
                            theme.wordleGrey
                ]
            <|
                el [ centerX, centerY ] <|
                    text (String.toUpper <| String.fromChar entryChar.char)
        }


nextEntryComponent : String -> Element Msg
nextEntryComponent currentEntry =
    row
        [ spacing 8
        , padding 8
        , Background.color theme.medium
        , Border.rounded 4
        ]
        [ el
            [ width fill ]
          <|
            Input.text
                [ width fill
                , Border.width 2
                , Border.color theme.secondary
                , htmlAttribute <| Html.Attributes.id inputId
                ]
                { onChange = Type
                , text = currentEntry
                , placeholder = Nothing
                , label =
                    Input.labelHidden "Enter your guess"
                }
        , Input.button
            [ padding 14
            , Background.color theme.secondary
            , Font.color <| textColour theme.white
            , alignTop
            ]
            { onPress = Just Submit
            , label = text "Add guess"
            }
        ]



-- Theme


theme =
    { light = rgb255 253 240 213
    , white = rgb255 250 250 250
    , dark = rgb255 0 48 73
    , medium = rgb255 102 155 188
    , secondary = rgb255 193 18 31
    , secondaryDark = rgb255 120 0 0
    , wordleYellow = rgb255 255 176 17
    , wordleGreen = rgb255 44 110 73
    , wordleGrey = rgb255 81 80 82
    }



-- Theme utilities


textColour : Element.Color -> Element.Color
textColour colour =
    let
        rgba =
            Element.toRgb colour
    in
    Element.fromRgb { rgba | alpha = 0.9 }


clamp : Float -> Float
clamp f =
    Basics.max 0 <| Basics.min 1 f


darken : Float -> Element.Color -> Element.Color
darken darkenFactor colour =
    let
        rgba =
            Element.toRgb colour
    in
    Element.fromRgb
        { red = clamp <| rgba.red * (1 - darkenFactor)
        , green = clamp <| rgba.green * (1 - darkenFactor)
        , blue = clamp <| rgba.blue * (1 - darkenFactor)
        , alpha = rgba.alpha
        }


lighten : Float -> Element.Color -> Element.Color
lighten lightenFactor colour =
    let
        rgba =
            Element.toRgb colour
    in
    Element.fromRgb
        { red = clamp <| ((1 - rgba.red) * lightenFactor) + rgba.red
        , green = clamp <| ((1 - rgba.green) * lightenFactor) + rgba.green
        , blue = clamp <| ((1 - rgba.blue) * lightenFactor) + rgba.blue
        , alpha = rgba.alpha
        }
