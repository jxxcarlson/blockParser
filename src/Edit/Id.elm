module Edit.Id exposing (Id(..), incrementVersion, init, nodeId, version)


type Id
    = Id Int Int


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
