module Model exposing (Flags, Model, Msg(..), initModel)

import Json.Encode as E


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


initModel : Flags -> Model
initModel _ =
    { registerA = Just sourceA
    , registerB = Just sourceB
    , registerC = Nothing
    , registerD = Nothing
    , registerE = Nothing
    , registerF = Nothing
    , registerM = Just sourceA
    }


type Msg
    = Input String
    | ReceivedDataFromJS E.Value


sourceA =
    """| section A

| subsection B

C
aaa
bbb
ccc

| subsection D

E

F

| section G
"""


sourceB =
    """xxx
yyy"""
