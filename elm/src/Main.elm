module Main (..) where

import Retro
import Html
import StartApp.Simple exposing (start)


main : Signal Html.Html
main =
  start
    { model = Retro.emptyModel
    , update = Retro.update
    , view = Retro.view
    }
