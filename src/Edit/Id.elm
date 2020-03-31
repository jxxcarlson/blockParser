module Edit.Id exposing (Id, display, init)


type Id
    = Id String


init : String -> Id
init str =
    Id str


display : Id -> String
display (Id str) =
    str
