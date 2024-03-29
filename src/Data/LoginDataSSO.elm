{-
   Persona
   KSF Media unified login service

   The version of the OpenAPI document: 1.3.0

   NOTE: This file is auto generated by the openapi-generator.
   https://github.com/openapitools/openapi-generator.git
   Do not edit this file manually.
-}


module Data.LoginDataSSO exposing (LoginDataSSO, decoder, encode)

import Uuid exposing (Uuid)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias LoginDataSSO =
    { uuid : Uuid
    , accessToken : String
    }


decoder : Decoder LoginDataSSO
decoder =
    Decode.succeed LoginDataSSO
        |> required "uuid" Uuid.decoder
        |> required "accessToken" Decode.string



encode : LoginDataSSO -> Encode.Value
encode model =
    Encode.object
        [ ( "uuid", Uuid.encode model.uuid )
        , ( "accessToken", Encode.string model.accessToken )

        ]


