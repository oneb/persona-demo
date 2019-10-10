import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Data.LoginResponse as LoginResponse exposing (LoginResponse)
import Data.LoginData as LoginData exposing (LoginData)
import Data.Address as Address exposing (Address)
import Data.User as User exposing (User)
import Request.Login as Login

import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Pipeline as JDP

import Http



-- MAIN
main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


-- MODEL
type Model
  = TryingToLogin 
      { creds : Creds 
      , errMsg : (Maybe String)
      }
  | WaitingLogin 
      { creds : Creds }
  | WaitingGetUser 
      { loginResponse : LoginResponse
      }
  | ShowingUser 
      { user : User
      , loginResponse : LoginResponse
      }

type TTLStatus 
  = TTLInit { errMsg : Maybe String }
  | TTLWaiting
  | TTLFailed String

type alias Creds =
  { username : String
  , password : String
  }

type alias ZzzSessInfo = 
  { loginResponse : LoginResponse
  }

type ZzzLIStatus 
  = LIWaitingGetUser
  | LIFailedGetUser String
  | LIShowingUser User
  | LIEditingUser User -- +...
  | LIWaitingSetAddr
  | LIFailedSetAddr String

init : () -> (Model, Cmd Msg)
init () = (TryingToLogin TTLInit { username = "", password = ""}, Cmd.none)


-- UPDATE
type Msg
  = ClickLogin 
  | PWEdit String
  | UNEdit String
  | LoginSuccess LoginResponse
  | LoginFailure Http.Error
  | GetUserSuccess User
  | GetUserFailure String
  | SetAddrSuccess
  | SetAddrFailure
-- address ...

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case (msg, model) of
  (ClickLogin, TryingToLogin r) -> 
    (WaitingLogin { creds : r.creds }, tryLogin creds)
  (ClickLogin, _) -> 
    failReset
  (PWEdit s, TryingToLogin r) ->
    let 
      creds_ = r.creds
      creds__ = { creds_ | password = s }
    in
      (TryingToLogin { r | creds = creds__ }, Cmd.none)
  (PWEdit _, _) -> 
    failReset
  (UNEdit s, TryingToLogin r) ->
    let 
      creds_ = r.creds
      creds__ = { creds_ | username = s }
    in
      (TryingToLogin { r | creds = creds__ }, Cmd.none)
  (UNEdit _, _) -> 
    failReset
  (LoginSuccess lr, WaitingLogin _) ->
    ( WaitingGetUser
        { loginResponse = lr 
        }
    , Cmd.none
    )
  (LoginSuccess _, _) -> failReset
  (LoginFailure err, WaitingLogin r) ->
    ( TryingToLogin 
      { creds = r.creds
      , errMsg = loginFailToMsg err
      }
    , Cmd.none
    )
  (LoginFailure _, _) ->
    failReset


-- TODO: More info about failure 
loginFailToMsg : Http.Error -> String
loginFailToMsg err = case err of
  BadStatus 403 -> "Wrong username or password"
  BadStatus n -> "Login failed (" ++ String.fromInt n ++ ")."
  _ -> "Login failed."


-- For states that shouldn't happen
failReset : (Model, Cmd Msg)
failReset = 
  ( TryingToLogin 
      { creds = { username = "", password = "" }
      , errMsg = "Something went wrong."
      }
  , Cmd.none
  )

update2 : Msg -> Model -> (Model, Cmd Msg)
update2 msg model =
  case msg of
    ClickLogin -> 
      case model of
        TryingToLogin st creds -> (TryingToLogin TTLWaiting creds, tryLogin creds) 
        _ -> Debug.todo "zzz"
    PWEdit s -> 
      case model of
        TryingToLogin st creds -> (TryingToLogin st { creds | password = s }, Cmd.none)
        _ -> Debug.todo "zzz"
        -- _ -> (TryingToLogin TTLInit { username = "", password = s}, Cmd.none)
    UNEdit s -> 
      case model of 
        TryingToLogin st creds -> (TryingToLogin st { creds | username = s }, Cmd.none)
        _ -> Debug.todo "zzz"
        -- _ -> (TryingToLogin TTLInit { username = s, password = ""}, Cmd.none)
    LoginSuccess r -> 
      (LoggedIn { stuff = "XXX lol1" }, Cmd.none)
    LoginFailure e -> 
      case model of 
        TryingToLogin st creds -> (TryingToLogin (TTLFailed "zzz hmm") creds, Cmd.none)
        _ -> Debug.todo "zzz"
        -- _ -> (TryingToLogin TTLInit { username = "", password = ""}, Cmd.none)
    GetUserSuccess u ->
      case model of
        LoggedIn _ si -> 
          (LoggedIn (LIShowingUser u) si, Cmd.none)
        _ -> Debug.todo "zzz"
    _ -> Debug.todo "zzz"


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW
view : Model -> Html Msg
view model = 
  case model of
    TryingToLogin st creds -> 
      case st of
        TTLInit -> loginForm creds Nothing -- zzz text 
        TTLWaiting -> zzzJustText "... loading ..."
        TTLFailed s -> loginForm creds (Just s)
    LoggedIn x -> p [] [text <| "dummy" ++ x.stuff]

zzzJustText : String -> Html Msg
zzzJustText s = p [] [text s]

loginForm : Creds -> Maybe String -> Html Msg
loginForm creds errNote =
  let 
    errElem = case errNote of
      Just s -> p [] [text s]
      Nothing -> text ""
  in
    div [ class "container" ]
      [ errElem
      , label [ for "username" ] [ text "username" ]
      , input [ type_ "text", value creds.username, placeholder "Enter username", name "username", onInput (\s -> UNEdit s), required True ] [] 
      , label [ for "password" ] [ text "password" ]
      , input [ type_ "password", value creds.password, placeholder "Enter password", name "password", onInput (\s -> PWEdit s), required True ] [] 
      , button [ onClick ClickLogin ] [ text "login" ]
      ] 


tryLogin : Creds -> Cmd Msg
tryLogin creds = Login.loginPost 
  { onSend = zzz1
  , body = 
    { username = creds.username
    , password = creds.password
    , mergeToken = Nothing
    }
  }


zzz1 : Result Http.Error LoginResponse -> Msg
zzz1 r = 
  case r of
    Err e -> LoginFailure e
    Ok lr -> LoginSuccess lr

tryGetUser : LoginResponse -> Cmd Msg
tryGetUser lr = 
  User.usersUuidGet
    { authorization : Just ('OAuth ' ++ lr.token)
    , cacheControl: Nothing
    }
    { onSend : zzz2
    , uuid : lr.uuid
    }

zzz2 : Result Http.Error User -> Msg
zzz2 r =
  case r of 
    Err e -> GetUserFailure 'XXX failed to get addr etc'
    Ok u -> GetUserSuccess u



