module BLParser.Parse exposing
    ( ParserState
    , deleteRangeInSource
    , getId
    , getSource
    , getSourceMap
    , getZipper
    , parse
    , parseSource
    , parseString
    , setBzs
    , setSource
    , toTree
    )

-- import BLParser.Block as Block exposing (Block)

import BLParser.Id as Id exposing (Id)
import BLParser.Language exposing (Language)
import BLParser.Source as Source exposing (Source)
import BLParser.SourceMap as SourceMap exposing (SourceMap)
import Language.Block as Block exposing (Block(..))
import Loop exposing (Step(..), loop)
import Stack exposing (Stack)
import Tree as Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


type ParserState blockType
    = ParserState
        { source : Source
        , sourceMap : SourceMap
        , cursor : Int
        , bzs : BlockZipperState blockType
        , arrayLength : Int
        , counter : Int
        , version : Int
        , id : Maybe Id
        }


initParserState : Language blockType -> Id -> Source -> ParserState blockType
initParserState lang id source =
    ParserState
        { source = source
        , sourceMap = SourceMap.empty source
        , cursor = 0
        , bzs = initState lang
        , arrayLength = Source.length source
        , counter = Id.nodeId id
        , version = Id.version id
        , id = Just id
        }


parseSource : Language blockType -> Id -> Source -> ParserState blockType
parseSource lang id source =
    parse lang (initParserState lang id source)


parseString : Language blockType -> Id -> String -> ParserState blockType
parseString lang id str =
    parseSource lang id (Source.fromString str)


initState : Language blockType -> BlockZipperState blockType
initState lang =
    { zipper = Zipper.fromTree (s lang.root), stack = Stack.init |> Stack.push (Block.typeOf lang.root) }


parse : Language blockType -> ParserState blockType -> ParserState blockType
parse lang parserState =
    loop parserState (nextState lang)
        |> updateSourceMap


toTree : ParserState blockType -> Tree (Block blockType)
toTree state =
    getBZS state
        |> .zipper
        |> Zipper.toTree


sourceLength : ParserState blockType -> Int
sourceLength (ParserState data) =
    data.arrayLength


updateSourceMap : ParserState blockType -> ParserState blockType
updateSourceMap parserState =
    let
        newSourceMap =
            parserState
                |> toTree
                |> SourceMap.fromTree
    in
    setSourceMap newSourceMap parserState



-- |> updateSourceMap


nextState : Language blockType -> ParserState blockType -> Step (ParserState blockType) (ParserState blockType)
nextState lang parserState =
    --let
    --    _ =
    --        Debug.log "n" (getCounter parserState)
    --
    --    _ =
    --        Debug.log "STACK" <|
    --            getStack parserState
    --
    --    _ =
    --        Debug.log "TREE" (getZipper parserState |> Zipper.toTree |> Tree.map (\b -> ( Block.stringOf b, Block.idOf b )))
    --in
    case getCursor parserState < sourceLength parserState of
        False ->
            Done parserState

        True ->
            let
                newBlock =
                    lang.getBlock (getCursor parserState) (getSource parserState)
            in
            case Stack.top (getStack parserState) of
                Nothing ->
                    Done parserState

                Just btAtStackTop ->
                    --let
                    --    _ =
                    --        Debug.log "(NB, TS)" ( Block.typeOf newBlock, btAtStackTop )
                    --in
                    if lang.gte (Block.typeOf newBlock) btAtStackTop then
                        --let
                        --    _ =
                        --        Debug.log "Pop" btAtStackTop
                        --in
                        Loop (map par parserState |> incrementCounter)

                    else
                        Loop
                            (let
                                newId =
                                    Maybe.map Id.incrementNodeId (getId parserState)

                                updatedBlock =
                                    Block.setId newId newBlock

                                --_ =
                                --    Debug.log "Push" updatedBlock
                             in
                             map (ap updatedBlock) parserState
                                |> map lc
                                |> updateCursor (Block.blockEnd newBlock)
                                |> incrementCounter
                                |> updateId newId
                            )



-- GETTERS


getBZS : ParserState blockType -> BlockZipperState blockType
getBZS (ParserState data) =
    data.bzs


getZipper : ParserState blockType -> Zipper (Block blockType)
getZipper (ParserState data) =
    data.bzs.zipper


getStack : ParserState blockType -> Stack blockType
getStack (ParserState data) =
    data.bzs.stack


getId : ParserState blockType -> Maybe Id
getId (ParserState data) =
    data.id


getCounter : ParserState blockType -> Int
getCounter (ParserState data) =
    data.counter


getSource : ParserState blockType -> Source
getSource (ParserState data) =
    data.source


getCursor : ParserState blockType -> Int
getCursor (ParserState data) =
    data.cursor


getSourceMap : ParserState blockType -> SourceMap
getSourceMap (ParserState data) =
    data.sourceMap



-- SETTERS, MODIFIERS


setSource : Source -> ParserState blockType -> ParserState blockType
setSource newSource (ParserState data) =
    ParserState { data | source = newSource }


setBzs : Language blockType -> Tree (Block blockType) -> ParserState blockType -> ParserState blockType
setBzs lang ast (ParserState data) =
    let
        newBzs =
            { zipper = Zipper.fromTree ast, stack = Stack.init |> Stack.push (Block.typeOf lang.root) }
    in
    ParserState { data | bzs = newBzs }


replaceRangeInSource : Int -> Int -> Source -> ParserState blockType -> ParserState blockType
replaceRangeInSource from to source (ParserState data) =
    ParserState { data | source = Source.replaceRange from to source data.source }


insertBeforeIndex : Int -> Source -> ParserState blockType -> ParserState blockType
insertBeforeIndex k source (ParserState data) =
    ParserState { data | source = Source.insertBeforeIndex k source data.source }


deleteRangeInSource : Int -> Int -> ParserState blockType -> ParserState blockType
deleteRangeInSource from to parserState =
    setSource (Source.deleteRange from to (getSource parserState)) parserState


setSourceMap : SourceMap -> ParserState blockType -> ParserState blockType
setSourceMap sourceMap (ParserState data) =
    ParserState { data | sourceMap = sourceMap }


type alias BlockZipperState blockType =
    { zipper : Zipper (Block blockType), stack : Stack blockType }


type alias Position =
    { line : Int, column : Int }



-- XXXX


updateCursor : Int -> ParserState blockType -> ParserState blockType
updateCursor k (ParserState ps) =
    ParserState { ps | cursor = k }


updateId : Maybe Id -> ParserState blockType -> ParserState blockType
updateId id (ParserState ps) =
    ParserState { ps | id = id }


incrementCounter : ParserState blockType -> ParserState blockType
incrementCounter (ParserState ps) =
    ParserState { ps | counter = ps.counter + 1 }



--- XXX


s =
    Tree.singleton


at =
    appendTreeToFocus


ap : Block blockType -> BlockZipperState blockType -> BlockZipperState blockType
ap b state =
    { state | zipper = at (s b) state.zipper }


map : (BlockZipperState blockType -> BlockZipperState blockType) -> ParserState blockType -> ParserState blockType
map f (ParserState ps) =
    let
        oldBzs =
            ps.bzs
    in
    ParserState { ps | bzs = f oldBzs }


par : BlockZipperState blockType -> BlockZipperState blockType
par state =
    case Zipper.parent state.zipper of
        Nothing ->
            state

        Just z ->
            let
                newStack =
                    state.stack |> Stack.pop
            in
            { state | stack = newStack, zipper = z }


lc : BlockZipperState blockType -> BlockZipperState blockType
lc state =
    case Zipper.lastChild state.zipper of
        Nothing ->
            state

        Just z ->
            { state | stack = Stack.push (Block.typeOf (Zipper.label z)) state.stack, zipper = z }


appendTreeToFocus : Tree blockType -> Zipper blockType -> Zipper blockType
appendTreeToFocus t_ z =
    let
        newTree =
            Tree.appendChild t_ (Zipper.tree z)
    in
    Zipper.replaceTree newTree z
