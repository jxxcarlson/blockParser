module Model exposing (Flags, Model, Msg(..), init)

import Cmd.Extra exposing (withNoCmd)
import File exposing (File)


type alias Model =
    { registerA : Maybe String
    , registerB : Maybe String
    , registerC : Maybe String
    , registerD : Maybe String
    , registerE : Maybe String
    , registerF : Maybe String
    , registerM : Maybe String
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    { registerA = Nothing
    , registerB = Nothing
    , registerC = Nothing
    , registerD = Nothing
    , registerE = Nothing
    , registerF = Nothing
    , registerM = Nothing
    }
        |> withNoCmd


type Msg
    = Input String
