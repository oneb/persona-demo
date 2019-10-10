{-
   Persona
   KSF Media unified login service

   The version of the OpenAPI document: 1.3.0

   NOTE: This file is auto generated by the openapi-generator.
   https://github.com/openapitools/openapi-generator.git
   Do not edit this file manually.
-}


module Data.LegalConsent exposing (LegalConsent, decoder, encode)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias LegalConsent =
    { screenName : String
    , consentId : String
    , dateAccepted : String
    }


decoder : Decoder LegalConsent
decoder =
    Decode.succeed LegalConsent
        |> required "screenName" Decode.string
        |> required "consentId" Decode.string
        |> required "dateAccepted" Decode.string



encode : LegalConsent -> Encode.Value
encode model =
    Encode.object
        [ ( "screenName", Encode.string model.screenName )
        , ( "consentId", Encode.string model.consentId )
        , ( "dateAccepted", Encode.string model.dateAccepted )

        ]


