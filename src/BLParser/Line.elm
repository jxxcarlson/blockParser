module BLParser.Line exposing (LineType(..), classify)


type alias Line =
    { lineNumber : Int
    , content : String
    , lineType : LineType
    }


indentationModulus =
    3


type alias IndentationLevel =
    Int


{-| Think of the List String as the arguments of a BlockHeading

    BlockParserTest: | section 1 Intro
    --> BlockHeading ["section", "1", "Intro"]

-}
type LineType
    = Blank
    | Text IndentationLevel
    | BlockHeading IndentationLevel (List String)
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
    let
        str_ =
            String.trimLeft str

        numberOfLeadingSpaces =
            String.length str - String.length str_

        level =
            modBy indentationModulus numberOfLeadingSpaces

        --_ =
        --    Debug.log "classify" ( level, str )
    in
    case String.left 1 str_ of
        "|" ->
            parseBlockHeading lineNumber level str

        "." ->
            parseBlockEnd lineNumber str

        _ ->
            parseTextLine lineNumber level str


parseBlockHeading : Int -> IndentationLevel -> String -> Line
parseBlockHeading lineNumber level str =
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
    , lineType = BlockHeading level args
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


parseTextLine : Int -> IndentationLevel -> String -> Line
parseTextLine lineNumber level str =
    let
        lineType =
            if str == "" then
                Blank

            else
                Text level
    in
    { lineNumber = lineNumber
    , content = str
    , lineType = lineType
    }
