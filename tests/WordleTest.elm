module WordleTest exposing (..)

import Expect
import Test exposing (Test, describe, test)
import Wordle exposing (Accuracy(..), Entry(..), suggestWords)


suite : Test
suite =
    describe "suggestWord"
        [ test "includes words with a matching Green character" <|
            \_ ->
                suggestWords
                    [ "abcde"
                    , "fghij"
                    , "klmno"
                    , "fpqrs"
                    ]
                    [ Filled
                        { char = 'f', accuracy = Green }
                        { char = 'w', accuracy = Grey }
                        { char = 'x', accuracy = Grey }
                        { char = 'y', accuracy = Grey }
                        { char = 'z', accuracy = Grey }
                    ]
                    |> Expect.equal
                        [ "fghij"
                        , "fpqrs"
                        ]
        , test "includes words with multiple matching Green characters" <|
            \_ ->
                suggestWords
                    [ "abcde"
                    , "fghij"
                    , "klmno"
                    , "fpqrs"
                    ]
                    [ Filled
                        { char = 'f', accuracy = Green }
                        { char = 'g', accuracy = Green }
                        { char = 'x', accuracy = Grey }
                        { char = 'y', accuracy = Grey }
                        { char = 'z', accuracy = Grey }
                    ]
                    |> Expect.equal
                        [ "fghij"
                        ]
        , test "excludes words with a matching Yellow character" <|
            \_ ->
                suggestWords
                    [ "fghij" ]
                    [ Filled
                        { char = 'f', accuracy = Yellow }
                        { char = 'w', accuracy = Grey }
                        { char = 'x', accuracy = Grey }
                        { char = 'y', accuracy = Grey }
                        { char = 'z', accuracy = Grey }
                    ]
                    |> Expect.equal []
        , test "includes words with a non-matching Yellow character" <|
            \_ ->
                suggestWords
                    [ "abcde"
                    , "fghij"
                    , "klmno"
                    , "pqrst"
                    ]
                    [ Filled
                        { char = 'w', accuracy = Grey }
                        { char = 'x', accuracy = Grey }
                        { char = 'y', accuracy = Grey }
                        { char = 'z', accuracy = Grey }
                        { char = 'f', accuracy = Yellow }
                    ]
                    |> Expect.equal
                        [ "fghij"
                        ]
        , test "excludes words that are missing a Yellow character" <|
            \_ ->
                suggestWords
                    [ "abcde"
                    , "fghij"
                    , "klmno"
                    , "pqrst"
                    ]
                    [ Filled
                        { char = 'v', accuracy = Yellow }
                        { char = 'w', accuracy = Grey }
                        { char = 'x', accuracy = Grey }
                        { char = 'y', accuracy = Grey }
                        { char = 'z', accuracy = Grey }
                    ]
                    |> Expect.equal []
        , test "candidates only need a single instance of duplicate yellow chars" <|
            \_ ->
                suggestWords
                    [ "abcde"
                    , "fghij"
                    , "klmno"
                    , "pqrst"
                    , "uvxyz"
                    ]
                    [ Filled
                        { char = 'a', accuracy = Grey }
                        { char = 'b', accuracy = Grey }
                        { char = 'c', accuracy = Grey }
                        { char = 'v', accuracy = Yellow }
                        { char = 'v', accuracy = Yellow }
                    ]
                    |> Expect.equal [ "uvxyz" ]
        , test "excludes words that include a Grey character" <|
            \_ ->
                suggestWords
                    [ "abcde"
                    , "fghij"
                    , "klmno"
                    , "pqrst"
                    ]
                    [ Filled
                        { char = 'g', accuracy = Grey }
                        { char = 'w', accuracy = Grey }
                        { char = 'x', accuracy = Grey }
                        { char = 'y', accuracy = Grey }
                        { char = 'z', accuracy = Grey }
                    ]
                    |> Expect.equal
                        [ "abcde"
                        , "klmno"
                        , "pqrst"
                        ]
        , test "ignore grey chars if that char is already green (Wordle doesn't show the duplicate as yellow in these cases)" <|
            \_ ->
                suggestWords
                    [ "inert"
                    ]
                    [ Filled
                        { char = 'd', accuracy = Grey }
                        { char = 'e', accuracy = Yellow }
                        { char = 'a', accuracy = Grey }
                        { char = 't', accuracy = Yellow }
                        { char = 'h', accuracy = Grey }
                    , Filled
                        { char = 'c', accuracy = Grey }
                        { char = 'r', accuracy = Yellow }
                        { char = 'e', accuracy = Green }
                        { char = 'p', accuracy = Grey }
                        { char = 't', accuracy = Green }
                    , Filled
                        { char = 'o', accuracy = Grey }
                        { char = 'v', accuracy = Grey }
                        { char = 'e', accuracy = Green }
                        { char = 'r', accuracy = Green }
                        { char = 't', accuracy = Green }
                    , Filled
                        { char = 'e', accuracy = Grey }
                        { char = 'x', accuracy = Grey }
                        { char = 'e', accuracy = Green }
                        { char = 'r', accuracy = Green }
                        { char = 't', accuracy = Green }
                    ]
                    |> Expect.equal
                        [ "inert"
                        ]
        ]
