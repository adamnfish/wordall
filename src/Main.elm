module Main exposing (..)

import Browser
import Model exposing (Lifecycle(..), Msg, update)
import View exposing (view)


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
