module BlockType.LanguageC exposing
    ( BlockKind(..)
    , BlockType(..)
    , blockType
    , getBlockKind
    , gte
    , order
    )


type BlockType
    = Root
    | Section Int Int
    | Paragraph Int
    | Math
    | None


type BlockKind
    = Tight String
    | Loose String
    | Unclassified


gte : BlockType -> BlockType -> Bool
gte a b =
    case order a b of
        GT ->
            True

        EQ ->
            True

        LT ->
            False


{-|

    order Root Paragraph
    --> GT

    order Paragraph Paragraph
    --> EQ

    order Paragraph Root
    --> LT

-}
order : BlockType -> BlockType -> Order
order a b =
    case ( a, b ) of
        ( Root, Section _ _ ) ->
            GT

        ( Root, Paragraph _ ) ->
            GT

        ( Section _ _, Root ) ->
            LT

        ( Section blockLevel1 sectionLevel1, Section blockLevel2 sectionLevel2 ) ->
            if blockLevel1 < blockLevel2 then
                LT

            else if blockLevel1 > blockLevel2 then
                GT

            else if sectionLevel1 < sectionLevel2 then
                LT

            else if sectionLevel1 > sectionLevel2 then
                GT

            else
                EQ

        ( Section blockLevel1 _, Paragraph blockLevel2 ) ->
            reverseCompare blockLevel1 blockLevel2

        ( Section _ _, Math ) ->
            GT

        ( Math, Root ) ->
            LT

        ( Paragraph _, Root ) ->
            LT

        ( Paragraph blockLevel1, Paragraph blockLevel2 ) ->
            reverseCompare blockLevel1 blockLevel2

        ( Paragraph blockLevel1, Section blockLevel2 _ ) ->
            reverseCompare blockLevel1 blockLevel2

        ( None, None ) ->
            EQ

        ( None, _ ) ->
            LT

        ( _, _ ) ->
            EQ


compare : Int -> Int -> Order
compare i j =
    if i < j then
        LT

    else if i > j then
        GT

    else
        EQ


reverseCompare : Int -> Int -> Order
reverseCompare i j =
    if i < j then
        GT

    else if i > j then
        LT

    else
        EQ


blockType : Int -> List String -> BlockType
blockType blockLevel args_ =
    let
        args =
            case List.head args_ of
                Just "|" ->
                    List.drop 1 args_

                _ ->
                    args_
    in
    case List.head args of
        Nothing ->
            None

        Just arg ->
            case arg of
                "section" ->
                    Section blockLevel 1

                "subsection" ->
                    Section blockLevel 2

                "subsubsection" ->
                    Section blockLevel 3

                "subsubsubsection" ->
                    Section blockLevel 4

                "math" ->
                    Math

                _ ->
                    Paragraph blockLevel


getBlockKind : List String -> BlockKind
getBlockKind args =
    case List.head args of
        Nothing ->
            Unclassified

        Just name ->
            case List.member name looseBlockNames of
                True ->
                    Loose name

                False ->
                    Tight name


looseBlockNames =
    []
