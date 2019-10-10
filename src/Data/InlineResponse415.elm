{-
   Persona
   KSF Media unified login service

   The version of the OpenAPI document: 1.3.0

   NOTE: This file is auto generated by the openapi-generator.
   https://github.com/openapitools/openapi-generator.git
   Do not edit this file manually.
-}


module Data.InlineResponse415 exposing (InlineResponse415, HttpStatus(..), decoder, encode)

import Data.InlineResponse415UnsupportedMediaType as InlineResponse415UnsupportedMediaType exposing (InlineResponse415UnsupportedMediaType)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias InlineResponse415 =
    { httpStatus : Maybe (HttpStatus)
    , unsupportedMediaType : Maybe (InlineResponse415UnsupportedMediaType)
    , httpCode : Maybe (Int)
    }


type HttpStatus
    = UnsupportedMediaType



decoder : Decoder InlineResponse415
decoder =
    Decode.succeed InlineResponse415
        |> optional "http_status" (Decode.nullable httpStatusDecoder) Nothing
        |> optional "unsupported_media_type" (Decode.nullable InlineResponse415UnsupportedMediaType.decoder) Nothing
        |> optional "http_code" (Decode.nullable Decode.int) Nothing



encode : InlineResponse415 -> Encode.Value
encode model =
    Encode.object
        [ ( "http_status", Maybe.withDefault Encode.null (Maybe.map encodeHttpStatus model.httpStatus) )
        , ( "unsupported_media_type", Maybe.withDefault Encode.null (Maybe.map InlineResponse415UnsupportedMediaType.encode model.unsupportedMediaType) )
        , ( "http_code", Maybe.withDefault Encode.null (Maybe.map Encode.int model.httpCode) )

        ]



httpStatusDecoder : Decoder HttpStatus
httpStatusDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Unsupported media type" ->
                        Decode.succeed UnsupportedMediaType

                    other ->
                        Decode.fail <| "Unknown type: " ++ other
            )



encodeHttpStatus : HttpStatus -> Encode.Value
encodeHttpStatus model =
    case model of
        UnsupportedMediaType ->
            Encode.string "Unsupported media type"



