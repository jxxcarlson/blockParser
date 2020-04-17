module ReviewConfig exposing (config)

import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoExposingEverything.rule
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule
    ]