module Edit.BlockType exposing (BlockType(..), blockType, gte, order)


type BlockType
    = Root
    | Section Int
    | Paragraph
    | None


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
        ( Root, Section _ ) ->
            GT

        ( Root, Paragraph ) ->
            GT

        ( Section _, Root ) ->
            LT

        ( Section i, Section j ) ->
            if i < j then
                GT

            else if i > j then
                LT

            else
                EQ

        ( Section _, Paragraph ) ->
            GT

        ( Paragraph, Root ) ->
            LT

        ( Paragraph, Section _ ) ->
            LT

        ( None, None ) ->
            EQ

        ( None, _ ) ->
            LT

        ( _, _ ) ->
            EQ


blockType : List String -> BlockType
blockType args =
    case List.head args of
        Nothing ->
            None

        Just arg ->
            case arg of
                "section" ->
                    Section 1

                "subsection" ->
                    Section 2

                "subsubsection" ->
                    Section 3

                "subsubsubsection" ->
                    Section 4

                _ ->
                    Paragraph
