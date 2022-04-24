module Wordle exposing (..)

import List.Extra


type Entry
    = Filled EntryChar EntryChar EntryChar EntryChar EntryChar


type alias EntryChar =
    { char : Char
    , accuracy : Accuracy
    }


type Accuracy
    = Green
    | Yellow
    | Grey


toEntry : String -> Maybe Entry
toEntry string =
    let
        entryChars =
            List.map
                (\c ->
                    { char = c
                    , accuracy = Grey
                    }
                )
                (String.toList string)
    in
    Maybe.map5
        Filled
        (List.Extra.getAt 0 entryChars)
        (List.Extra.getAt 1 entryChars)
        (List.Extra.getAt 2 entryChars)
        (List.Extra.getAt 3 entryChars)
        (List.Extra.getAt 4 entryChars)


entryAsList : Entry -> List EntryChar
entryAsList entry =
    case entry of
        Filled ec1 ec2 ec3 ec4 ec5 ->
            [ ec1, ec2, ec3, ec4, ec5 ]


suggestWords : List String -> List Entry -> List String
suggestWords wordlist entries =
    List.filter
        (\word ->
            let
                matches : Bool
                matches =
                    List.all
                        (\entry ->
                            let
                                positionMatches : Bool
                                positionMatches =
                                    -- line up entry chars and word chars
                                    -- green chars should match
                                    -- yellow chars should not match
                                    -- otherwise we are not fussed
                                    List.Extra.zip
                                        (String.toList word)
                                        (entryAsList entry)
                                        |> List.all
                                            (\( wordChar, { accuracy, char } ) ->
                                                if accuracy == Green then
                                                    wordChar == char

                                                else if accuracy == Yellow then
                                                    wordChar /= char

                                                else
                                                    True
                                            )

                                yellowEntryChars : List EntryChar
                                yellowEntryChars =
                                    List.filter
                                        (\{ accuracy } ->
                                            accuracy == Yellow
                                        )
                                        (entryAsList entry)
                                        |> List.Extra.unique

                                containsAllYellowChars : Bool
                                containsAllYellowChars =
                                    -- yellow chars must be a subset of the word's chars
                                    let
                                        yellowChars =
                                            List.map .char yellowEntryChars

                                        chars =
                                            String.toList word
                                    in
                                    isSubwordOf yellowChars chars

                                greyEntryChars : List Char
                                greyEntryChars =
                                    List.filter
                                        (\{ accuracy } ->
                                            accuracy == Grey
                                        )
                                        (entryAsList entry)
                                        |> List.map .char
                                        |> List.filter
                                            (\char ->
                                                -- ignore grey chars if there's a green version of that char in this entry
                                                not
                                                    (entryAsList entry
                                                        |> List.any (\ec -> ec.accuracy == Green && ec.char == char)
                                                    )
                                            )

                                doesNotContainGreys : Bool
                                doesNotContainGreys =
                                    -- any grey chars should not appear in the word at all
                                    -- *UNLESS* they also appear as green already!
                                    not <|
                                        List.any
                                            (\c ->
                                                List.member c greyEntryChars
                                            )
                                        <|
                                            String.toList word
                            in
                            positionMatches && containsAllYellowChars && doesNotContainGreys
                        )
                        entries
            in
            matches
        )
        wordlist


isSubwordOf : List Char -> List Char -> Bool
isSubwordOf candidate check =
    let
        candidateChars =
            List.Extra.gatherEquals candidate

        checkChars =
            List.Extra.gatherEquals check
    in
    List.all
        (\( candidateChar, candidateRepeats ) ->
            -- must be present in at least the correct quantity
            List.any
                (\( char, repeats ) ->
                    char == candidateChar && (List.length candidateRepeats <= List.length repeats)
                )
                checkChars
        )
        candidateChars
