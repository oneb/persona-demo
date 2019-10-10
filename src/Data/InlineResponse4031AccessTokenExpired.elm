{-
   Persona
   KSF Media unified login service

   The version of the OpenAPI document: 1.3.0

   NOTE: This file is auto generated by the openapi-generator.
   https://github.com/openapitools/openapi-generator.git
   Do not edit this file manually.
-}


module Data.InlineResponse4031AccessTokenExpired exposing (InlineResponse4031AccessTokenExpired, Description(..), decoder, encode)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias InlineResponse4031AccessTokenExpired =
    { description : Maybe (Description)
    }


type Description
    = TheAccessTokenYouProvidedIsExpiredPleaseReauthenticate



decoder : Decoder InlineResponse4031AccessTokenExpired
decoder =
    Decode.succeed InlineResponse4031AccessTokenExpired
        |> optional "description" (Decode.nullable descriptionDecoder) Nothing



encode : InlineResponse4031AccessTokenExpired -> Encode.Value
encode model =
    Encode.object
        [ ( "description", Maybe.withDefault Encode.null (Maybe.map encodeDescription model.description) )

        ]



descriptionDecoder : Decoder Description
descriptionDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "The access token you provided is expired. Please reauthenticate." ->
                        Decode.succeed TheAccessTokenYouProvidedIsExpiredPleaseReauthenticate

                    other ->
                        Decode.fail <| "Unknown type: " ++ other
            )



encodeDescription : Description -> Encode.Value
encodeDescription model =
    case model of
        TheAccessTokenYouProvidedIsExpiredPleaseReauthenticate ->
            Encode.string "The access token you provided is expired. Please reauthenticate."


