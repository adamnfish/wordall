module WordsTest exposing (..)

import Expect
import Test exposing (Test, describe, test)
import Wordle exposing (Accuracy(..), Entry(..), suggestWord)


suite : Test
suite =
    describe "suggestWord"
        [ test "includes words with a matching Green character" <|
            \_ ->
                suggestWord
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
                suggestWord
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
                suggestWord
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
                suggestWord
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
                suggestWord
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
        , test "excludes words that include a Grey character" <|
            \_ ->
                suggestWord
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
        ]
