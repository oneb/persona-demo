{-
   Persona
   KSF Media unified login service

   The version of the OpenAPI document: 1.3.0

   NOTE: This file is auto generated by the openapi-generator.
   https://github.com/openapitools/openapi-generator.git
   Do not edit this file manually.
-}


module Data.Subscription exposing (Subscription, decoder, encode)

import Data.Package as Package exposing (Package)
import Data.SubscriptionDates as SubscriptionDates exposing (SubscriptionDates)
import Data.Campaign as Campaign exposing (Campaign)
import Data.PausedSubscription as PausedSubscription exposing (PausedSubscription)
import Data.DeliveryAddress as DeliveryAddress exposing (DeliveryAddress)
import Data.PendingAddressChange as PendingAddressChange exposing (PendingAddressChange)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias Subscription =
    { subsno : Int
    , extno : Int
    , cusno : Int
    , paycusno : Int
    , kind : String
    , state : String
    , pricegroup : Maybe (String)
    , package : Package
    , dates : SubscriptionDates
    , extsubsexists : Bool
    , campaign : Maybe (Campaign)
    , paused : Maybe ((List PausedSubscription))
    , deliveryAddress : Maybe (DeliveryAddress)
    , pendingAddressChanges : Maybe ((List PendingAddressChange))
    }


decoder : Decoder Subscription
decoder =
    Decode.succeed Subscription
        |> required "subsno" Decode.int
        |> required "extno" Decode.int
        |> required "cusno" Decode.int
        |> required "paycusno" Decode.int
        |> required "kind" Decode.string
        |> required "state" Decode.string
        |> optional "pricegroup" (Decode.nullable Decode.string) Nothing
        |> required "package" Package.decoder
        |> required "dates" SubscriptionDates.decoder
        |> required "extsubsexists" Decode.bool
        |> optional "campaign" (Decode.nullable Campaign.decoder) Nothing
        |> optional "paused" (Decode.nullable (Decode.list PausedSubscription.decoder)) Nothing
        |> optional "deliveryAddress" (Decode.nullable DeliveryAddress.decoder) Nothing
        |> optional "pendingAddressChanges" (Decode.nullable (Decode.list PendingAddressChange.decoder)) Nothing



encode : Subscription -> Encode.Value
encode model =
    Encode.object
        [ ( "subsno", Encode.int model.subsno )
        , ( "extno", Encode.int model.extno )
        , ( "cusno", Encode.int model.cusno )
        , ( "paycusno", Encode.int model.paycusno )
        , ( "kind", Encode.string model.kind )
        , ( "state", Encode.string model.state )
        , ( "pricegroup", Maybe.withDefault Encode.null (Maybe.map Encode.string model.pricegroup) )
        , ( "package", Package.encode model.package )
        , ( "dates", SubscriptionDates.encode model.dates )
        , ( "extsubsexists", Encode.bool model.extsubsexists )
        , ( "campaign", Maybe.withDefault Encode.null (Maybe.map Campaign.encode model.campaign) )
        , ( "paused", Maybe.withDefault Encode.null (Maybe.map (Encode.list PausedSubscription.encode) model.paused) )
        , ( "deliveryAddress", Maybe.withDefault Encode.null (Maybe.map DeliveryAddress.encode model.deliveryAddress) )
        , ( "pendingAddressChanges", Maybe.withDefault Encode.null (Maybe.map (Encode.list PendingAddressChange.encode) model.pendingAddressChanges) )

        ]


