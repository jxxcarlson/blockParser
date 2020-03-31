module Edit.Id exposing
    ( Id(..)
    , incrementNodeId
    , incrementVersion
    , init
    , nodeId
    , stringValue
    , version
    )


type Id
    = Id Int Int


stringValue : Id -> String
stringValue (Id version_ nodeId_) =
    String.fromInt version_ ++ "," ++ String.fromInt nodeId_


init : Int -> Int -> Id
init version_ nodeId_ =
    Id version_ nodeId_


nodeId : Id -> Int
nodeId (Id _ nodeId_) =
    nodeId_


version : Id -> Int
version (Id version_ _) =
    version_


incrementVersion : Id -> Id
incrementVersion (Id version_ id) =
    Id (version_ + 1) id


incrementNodeId : Id -> Id
incrementNodeId (Id version_ id) =
    Id version_ (id + 1)
