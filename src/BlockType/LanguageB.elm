module BlockType.LanguageB exposing (BlockType(..), blockType, order)


type BlockType
    = Root
    | Section Int
    | Paragraph
    | Math
    | None


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

        ( Section _, Math ) ->
            GT

        ( Math, Root ) ->
            LT

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

                "math" ->
                    Math

                _ ->
                    Paragraph
