module Language.B.Line exposing (LineType(..), classify)


type alias Line =
    { lineNumber : Int
    , content : String
    , lineType : LineType
    }


{-| Think of the List String as the arguments of a BlockHeading
BlockParserTest: | section 1 Intro
--> BlockHeading ["section", "1", "Intro"]
-}
type LineType
    = Blank
    | Text
    | BlockHeading (List String)
    | BlockEnd String


{-|

    > classify 1 "| Math"
    Line { content = "| Math", lineNumber = 1, lineType = BlockHeading ["Math"] }
    >  classify 1 "| Section 1 Introduction"
    Line { content = "| Section 1 Introduction", lineNumber = 1, lineType = BlockHeading ["Section","1","Introduction"] }
    > classify 1 ".quote"
    Line { content = ".quote", lineNumber = 1, lineType = BlockEnd "quote" }
    > classify 1 "This is a test."
    Line { content = "This is a test.", lineNumber = 1, lineType = Text }
    > classify 1 ""
    Line { content = "", lineNumber = 1, lineType = Blank }

-}
classify : Int -> String -> Line
classify lineNumber str =
    case String.left 1 str of
        "|" ->
            parseBlockHeading lineNumber str

        "." ->
            parseBlockEnd lineNumber str

        _ ->
            parseTextLine lineNumber str


parseBlockHeading : Int -> String -> Line
parseBlockHeading lineNumber str =
    let
        args : List String
        args =
            str
                |> String.dropLeft 1
                |> String.words
                |> List.map String.trim
    in
    { lineNumber = lineNumber
    , content = str
    , lineType = BlockHeading args
    }


parseBlockEnd : Int -> String -> Line
parseBlockEnd lineNumber str =
    let
        arg : String
        arg =
            str
                |> String.dropLeft 1
                |> String.trim
    in
    { lineNumber = lineNumber
    , content = str
    , lineType = BlockEnd arg
    }


parseTextLine : Int -> String -> Line
parseTextLine lineNumber str =
    let
        lineType =
            if str == "" then
                Blank

            else
                Text
    in
    { lineNumber = lineNumber
    , content = str
    , lineType = lineType
    }
