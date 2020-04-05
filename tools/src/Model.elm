module Model exposing (Model, Msg(..))

import File exposing (File)


type alias Model =
    { sourceText : Maybe String }


type Msg
    = Input String
    | SourceRequested
    | SourceSelected File
    | SourceLoaded String
