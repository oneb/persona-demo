{-
   Persona
   KSF Media unified login service

   The version of the OpenAPI document: 1.3.0

   NOTE: This file is auto generated by the openapi-generator.
   https://github.com/openapitools/openapi-generator.git
   Do not edit this file manually.
-}


module Data.InlineResponse4032EmailAddressInUse exposing (InlineResponse4032EmailAddressInUse, Description(..), decoder, encode)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias InlineResponse4032EmailAddressInUse =
    { existingProvider : Maybe (String)
    , mergeToken : Maybe (String)
    , description : Maybe (Description)
    }


type Description
    = UserHasAnotherRecordRegisteredPleaseMergeTheAccounts



decoder : Decoder InlineResponse4032EmailAddressInUse
decoder =
    Decode.succeed InlineResponse4032EmailAddressInUse
        |> optional "existing_provider" (Decode.nullable Decode.string) Nothing
        |> optional "merge_token" (Decode.nullable Decode.string) Nothing
        |> optional "description" (Decode.nullable descriptionDecoder) Nothing



encode : InlineResponse4032EmailAddressInUse -> Encode.Value
encode model =
    Encode.object
        [ ( "existing_provider", Maybe.withDefault Encode.null (Maybe.map Encode.string model.existingProvider) )
        , ( "merge_token", Maybe.withDefault Encode.null (Maybe.map Encode.string model.mergeToken) )
        , ( "description", Maybe.withDefault Encode.null (Maybe.map encodeDescription model.description) )

        ]



descriptionDecoder : Decoder Description
descriptionDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "User has another record registered. Please merge the accounts." ->
                        Decode.succeed UserHasAnotherRecordRegisteredPleaseMergeTheAccounts

                    other ->
                        Decode.fail <| "Unknown type: " ++ other
            )



encodeDescription : Description -> Encode.Value
encodeDescription model =
    case model of
        UserHasAnotherRecordRegisteredPleaseMergeTheAccounts ->
            Encode.string "User has another record registered. Please merge the accounts."


