{-
   Persona
   KSF Media unified login service

   The version of the OpenAPI document: 1.3.0

   NOTE: This file is auto generated by the openapi-generator.
   https://github.com/openapitools/openapi-generator.git
   Do not edit this file manually.
-}


module Data.User exposing (User, decoder, encode)

import Uuid exposing (Uuid)
import Data.Address as Address exposing (Address)
import Data.Subscription as Subscription exposing (Subscription)
import Data.GdprConsent as GdprConsent exposing (GdprConsent)
import Data.LegalConsent as LegalConsent exposing (LegalConsent)
import Data.PendingAddressChange as PendingAddressChange exposing (PendingAddressChange)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias User =
    { uuid : Uuid
    , email : String
    , firstName : Maybe (String)
    , lastName : Maybe (String)
    , address : Maybe (Address)
    , cusno : String
    , subs : (List Subscription)
    , consent : (List GdprConsent)
    , legal : (List LegalConsent)
    , pendingAddressChanges : Maybe ((List PendingAddressChange))
    }


decoder : Decoder User
decoder =
    Decode.succeed User
        |> required "uuid" Uuid.decoder
        |> required "email" Decode.string
        |> optional "firstName" (Decode.nullable Decode.string) Nothing
        |> optional "lastName" (Decode.nullable Decode.string) Nothing
        |> optional "address" (Decode.nullable Address.decoder) Nothing
        |> required "cusno" Decode.string
        |> required "subs" (Decode.list Subscription.decoder)
        |> required "consent" (Decode.list GdprConsent.decoder)
        |> required "legal" (Decode.list LegalConsent.decoder)
        |> optional "pendingAddressChanges" (Decode.nullable (Decode.list PendingAddressChange.decoder)) Nothing



encode : User -> Encode.Value
encode model =
    Encode.object
        [ ( "uuid", Uuid.encode model.uuid )
        , ( "email", Encode.string model.email )
        , ( "firstName", Maybe.withDefault Encode.null (Maybe.map Encode.string model.firstName) )
        , ( "lastName", Maybe.withDefault Encode.null (Maybe.map Encode.string model.lastName) )
        , ( "address", Maybe.withDefault Encode.null (Maybe.map Address.encode model.address) )
        , ( "cusno", Encode.string model.cusno )
        , ( "subs", (Encode.list Subscription.encode) model.subs )
        , ( "consent", (Encode.list GdprConsent.encode) model.consent )
        , ( "legal", (Encode.list LegalConsent.encode) model.legal )
        , ( "pendingAddressChanges", Maybe.withDefault Encode.null (Maybe.map (Encode.list PendingAddressChange.encode) model.pendingAddressChanges) )

        ]


