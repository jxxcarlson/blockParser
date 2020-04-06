module Model exposing (Flags, Model, Msg(..), initModel)


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
    { registerA = Nothing
    , registerB = Nothing
    , registerC = Nothing
    , registerD = Nothing
    , registerE = Nothing
    , registerF = Nothing
    , registerM = Just initialSource
    }


type Msg
    = Input String


initialSource =
    """| section A

| subsection B

C

| subsection D

E

F

| section G"""
