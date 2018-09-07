module Value exposing
    ( Cardinality(..)
    , Set
    , Value(..)
    , append
    , dropLast
    , empty
    , redact
    , update
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Objecthash.Hash as Hash
import Objecthash.Value as OValue exposing (Value(..))
import Seal exposing (Seal(..))


type alias Bytes =
    Hash.ByteList


type alias Set =
    Array Seal


type alias Name =
    String


type Cardinality
    = One
    | Many


type Value
    = String Seal
    | Set Set


emptySet : Value
emptySet =
    Set (Array.fromList [ Seal.empty ])


empty : Cardinality -> Value
empty card =
    case card of
        One ->
            String Seal.empty

        Many ->
            emptySet


dropLast : Value -> Value
dropLast value =
    case value of
        String _ ->
            String Seal.empty

        Set xs ->
            let
                len =
                    Array.length xs
            in
            if len <= 1 then
                emptySet

            else
                Set (Array.slice 0 (len - 1) xs)


append : Value -> Value
append value =
    case value of
        String str ->
            String str

        Set xs ->
            Set (Array.push Seal.empty xs)


update : Int -> String -> Value -> Value
update idx token value =
    case value of
        String seal ->
            String (Seal.update token seal)

        Set xs ->
            let
                seal =
                    Maybe.withDefault Seal.empty (Array.get idx xs)
            in
            Set (Array.set idx (Seal.update token seal) xs)


{-| TODO: Should be renamed to toggleRedaction
-}
redact : Int -> Value -> Value
redact idx value =
    case value of
        String seal ->
            String (Seal.toggle seal)

        Set xs ->
            let
                seal =
                    Maybe.withDefault Seal.empty (Array.get idx xs)
            in
            Set (Array.set idx (Seal.toggle seal) xs)


cardinalityFromString : String -> Cardinality
cardinalityFromString input =
    case input of
        "1" ->
            One

        "n" ->
            Many

        _ ->
            One


cardinalityToString : Cardinality -> String
cardinalityToString card =
    case card of
        One ->
            "1"

        Many ->
            "n"
