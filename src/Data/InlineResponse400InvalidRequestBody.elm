{-
   Persona
   KSF Media unified login service

   The version of the OpenAPI document: 1.3.0

   NOTE: This file is auto generated by the openapi-generator.
   https://github.com/openapitools/openapi-generator.git
   Do not edit this file manually.
-}


module Data.InlineResponse400InvalidRequestBody exposing (InlineResponse400InvalidRequestBody, Description(..), decoder, encode)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias InlineResponse400InvalidRequestBody =
    { message : Maybe (String)
    , description : Maybe (Description)
    }


type Description
    = CouldNotParse



decoder : Decoder InlineResponse400InvalidRequestBody
decoder =
    Decode.succeed InlineResponse400InvalidRequestBody
        |> optional "message" (Decode.nullable Decode.string) Nothing
        |> optional "description" (Decode.nullable descriptionDecoder) Nothing



encode : InlineResponse400InvalidRequestBody -> Encode.Value
encode model =
    Encode.object
        [ ( "message", Maybe.withDefault Encode.null (Maybe.map Encode.string model.message) )
        , ( "description", Maybe.withDefault Encode.null (Maybe.map encodeDescription model.description) )

        ]



descriptionDecoder : Decoder Description
descriptionDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Couldn't parse the request body." ->
                        Decode.succeed CouldNotParse

                    other ->
                        Decode.fail <| "Unknown type: " ++ other
            )



encodeDescription : Description -> Encode.Value
encodeDescription model =
    case model of
        CouldNotParse ->
            Encode.string "Couldn't parse the request body."


