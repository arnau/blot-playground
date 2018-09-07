module Blob exposing
    ( Blob
    , addValue
    , cardinalityFromString
    , cardinalityToString
    , changeValue
    , empty
    , get
    , insert
    , member
    , remove
    , removeValue
    , rename
    , toList
    , update
    )

-- import Html exposing (Html, button, div, input, label, text)
-- import Html.Attributes exposing (for, id, value)
-- import Html.Events exposing (onClick, onInput)

import Array exposing (Array)
import Dict exposing (Dict)
import Objecthash.Hash as Hash
import Objecthash.Value as OValue exposing (Value(..))
import Seal exposing (Seal(..))
import Value exposing (Cardinality(..), Set, Value(..))


type alias Name =
    String


type alias Bare =
    List ( Name, Value )


type Blob
    = Blob Bare


type Error
    = Unknown Name
    | Duplicated Name


empty : Blob
empty =
    Blob []


member : Name -> Blob -> Bool
member name (Blob blob) =
    member_ name blob


member_ : Name -> Bare -> Bool
member_ name bare =
    case get_ name bare of
        Nothing ->
            False

        _ ->
            True


get : Name -> Blob -> Maybe Value
get name (Blob bare) =
    get_ name bare


get_ : Name -> Bare -> Maybe Value
get_ name bare =
    case List.filter (\( n, _ ) -> name == n) bare of
        [] ->
            Nothing

        ( _, value ) :: rest ->
            Just value


insert : Name -> Value -> Blob -> Result Error Blob
insert name value (Blob bare) =
    if member_ name bare then
        Err (Duplicated name)

    else
        Ok (Blob (( name, value ) :: bare))


rename : Name -> Name -> Blob -> Result Error Blob
rename name newName (Blob bare) =
    let
        collect ( name_, value_ ) acc =
            case acc of
                Err xs ->
                    if name_ == name then
                        Ok (( newName, value_ ) :: xs)

                    else
                        Err (( newName, value_ ) :: xs)

                Ok xs ->
                    Ok (( name_, value_ ) :: xs)
    in
    case List.foldr collect (Err []) bare of
        Err _ ->
            Err (Unknown name)

        Ok xs ->
            Ok (Blob xs)


update : Name -> Value -> Blob -> Result Error Blob
update name value (Blob bare) =
    let
        updateValue ( attr_, val_ ) =
            if name == attr_ then
                ( attr_, value )

            else
                ( attr_, val_ )
    in
    if member_ name bare then
        Ok (Blob (List.map updateValue bare))

    else
        Err (Unknown name)


remove : Name -> Blob -> Blob
remove name (Blob bare) =
    Blob (List.filter (\( name_, _ ) -> name /= name_) bare)


toList : Blob -> Bare
toList (Blob bare) =
    bare



-- Value


removeValue : Name -> Blob -> Blob
removeValue name blob =
    case get name blob of
        Nothing ->
            blob

        Just value ->
            update name (Value.dropLast value) blob
                |> Result.withDefault blob


addValue : Name -> Blob -> Blob
addValue name blob =
    case get name blob of
        Nothing ->
            blob

        Just value ->
            update name (Value.append value) blob
                |> Result.withDefault blob


changeValue : Name -> Int -> String -> Blob -> Blob
changeValue name idx token blob =
    case get name blob of
        Nothing ->
            blob

        Just value ->
            update name (Value.update idx token value) blob
                |> Result.withDefault blob


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
