import Retro
import StartApp.Simple exposing (start)

main =
  start
    { model = Retro.emptyModel
    , update = Retro.update
    , view = Retro.view
    }
