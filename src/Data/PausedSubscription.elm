{-
   Persona
   KSF Media unified login service

   The version of the OpenAPI document: 1.3.0

   NOTE: This file is auto generated by the openapi-generator.
   https://github.com/openapitools/openapi-generator.git
   Do not edit this file manually.
-}


module Data.PausedSubscription exposing (PausedSubscription, decoder, encode)

import DateOnly exposing (DateOnly)
import DateOnly exposing (DateOnly)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias PausedSubscription =
    { startDate : DateOnly
    , endDate : Maybe (DateOnly)
    }


decoder : Decoder PausedSubscription
decoder =
    Decode.succeed PausedSubscription
        |> required "startDate" DateOnly.decoder
        |> optional "endDate" (Decode.nullable DateOnly.decoder) Nothing



encode : PausedSubscription -> Encode.Value
encode model =
    Encode.object
        [ ( "startDate", DateOnly.encode model.startDate )
        , ( "endDate", Maybe.withDefault Encode.null (Maybe.map DateOnly.encode model.endDate) )

        ]

