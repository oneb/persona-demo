{-
   Persona
   KSF Media unified login service

   The version of the OpenAPI document: 1.3.0

   NOTE: This file is auto generated by the openapi-generator.
   https://github.com/openapitools/openapi-generator.git
   Do not edit this file manually.
-}


module Data.Product exposing (Product, decoder, encode)

import Data.ActiveDays as ActiveDays exposing (ActiveDays)
import DateOnly exposing (DateOnly)
import Data.Paper as Paper exposing (Paper)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias Product =
    { id : String
    , name : String
    , active : ActiveDays
    , nextDelivery : Maybe (DateOnly)
    , paper : Paper
    , digital : Bool
    }


decoder : Decoder Product
decoder =
    Decode.succeed Product
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "active" ActiveDays.decoder
        |> optional "nextDelivery" (Decode.nullable DateOnly.decoder) Nothing
        |> required "paper" Paper.decoder
        |> required "digital" Decode.bool



encode : Product -> Encode.Value
encode model =
    Encode.object
        [ ( "id", Encode.string model.id )
        , ( "name", Encode.string model.name )
        , ( "active", ActiveDays.encode model.active )
        , ( "nextDelivery", Maybe.withDefault Encode.null (Maybe.map DateOnly.encode model.nextDelivery) )
        , ( "paper", Paper.encode model.paper )
        , ( "digital", Encode.bool model.digital )

        ]

