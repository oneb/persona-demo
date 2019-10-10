module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Events exposing (onInput, onClick)

import Data.LoginResponse as LoginResponse exposing (LoginResponse)
import Data.LoginData as LoginData exposing (LoginData)
import Data.Address as Address exposing (Address)
import Data.User as User exposing (User)
import Request.Login as Login
import Request.Users as Users

import Form exposing (Form)
import Form.View

import Tuple
import Http


-- MAIN
main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


-- MODEL
type Model 
  = AtLoginForm
      (Form.View.Model LoginFormValues)
      (Maybe LoginResponse)
      (Maybe Note)
  | ShowingUser 
      User
      LoginResponse
      (Maybe Note)
  | AtAddressForm
      (Form.View.Model AddressFormValues)
      User
      LoginResponse
      (Maybe Note)

type Note 
  = Note String
  | ErrorNote String

init : () -> (Model, Cmd Msg)
init () = 
  ( AtLoginForm 
      (Form.View.idle { email = "", password = ""})
      Nothing
      Nothing
  , Cmd.none
  )
  
-- UPDATE
type Msg
  = LoginFormChanged (Form.View.Model LoginFormValues)
  | ClickLogin LoginFormOutput
  | LoginSuccess LoginResponse
  | LoginFailure Http.Error
  | GetUserSuccess User
  | GetUserFailure Http.Error
  | ClickEditAddress
  | AddressFormChanged (Form.View.Model AddressFormValues)
  | ClickSubmitAddress AddressFormOutput
  | SetAddrSuccess User
  | SetAddrFailure Http.Error
  | ClickLogout


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model) of
    (LoginFormChanged fvm_new, AtLoginForm fvm_old m note) ->
      (AtLoginForm fvm_new m note, Cmd.none)
    (ClickLogin output, AtLoginForm fvm m _) ->
      (AtLoginForm { fvm | state = Form.View.Loading } m Nothing, tryLogin output)
    (LoginSuccess lr, AtLoginForm fvm _ note) ->
      (AtLoginForm fvm (Just lr) note, tryGetUser lr)
    (LoginFailure err, AtLoginForm fvm m _) ->
      ( AtLoginForm { fvm | state = Form.View.Idle } m (Just (ErrorNote (loginFailToMsg err)))
      , Cmd.none
      )
    (GetUserSuccess u, AtLoginForm fvm (Just lr) _) ->
      (ShowingUser u lr Nothing, Cmd.none)
    (GetUserFailure err, AtLoginForm fvm (Just lr) _) ->
      ( AtLoginForm { fvm | state = Form.View.Idle } Nothing (Just (ErrorNote (getUserFailToMsg err)))
      , Cmd.none
      )
    (ClickEditAddress, ShowingUser u lr _) ->
      let
        fvm = Form.View.idle (populateAddressForm u)
      in
        (AtAddressForm fvm u lr Nothing, Cmd.none)
    (ClickSubmitAddress output, AtAddressForm fvm u lr _) -> 
      ( AtAddressForm { fvm | state = Form.View.Loading } u lr Nothing
      , tryPatchUser output lr
      )
    (AddressFormChanged fvm_new, AtAddressForm fvm u lr note) ->
      (AtAddressForm fvm_new u lr note, Cmd.none)
    (SetAddrSuccess u_new, AtAddressForm fvm u lr _) ->
      let 
        nt = "Your address was updated."
      in
        (ShowingUser u_new lr (Just (Note nt)), Cmd.none)
    (SetAddrFailure err, AtAddressForm fvm u lr _) ->
      ( AtAddressForm { fvm | state = Form.View.Idle } u lr (Just (ErrorNote (setAddrFailToMsg err)))
      , Cmd.none
      )
    (ClickLogout, m) ->
      ( AtLoginForm 
          (Form.View.idle { email = "", password = "" })
          Nothing
          (Just (Note "You logged out."))
      , Cmd.none
      )
    (_, _) ->
      failReset


-- TODO: Better info about failures
getUserFailToMsg : Http.Error -> String
getUserFailToMsg err = case err of
  Http.BadStatus n -> "Failed to get user info (" ++ String.fromInt n ++ ")."
  _ -> "Failed to get user info."

loginFailToMsg : Http.Error -> String
loginFailToMsg err = case err of
  Http.BadStatus 403 -> "Wrong username or password."
  Http.BadStatus n -> "Login failed (" ++ String.fromInt n ++ ")."
  _ -> "Login failed."

setAddrFailToMsg : Http.Error -> String
setAddrFailToMsg err = case err of
  Http.BadStatus n -> "Failed to change address (" ++ String.fromInt n ++ ")."
  _ -> "Failed to change address."

-- For states that shouldn't happen
failReset : (Model, Cmd Msg)
failReset = 
  ( AtLoginForm 
      (Form.View.idle { email = "", password = ""})
      Nothing
      (Just (ErrorNote "Something went wrong."))
  , Cmd.none
  )


tryLogin : LoginFormOutput -> Cmd Msg
tryLogin output = 
  let
    resultToMsg r = case r of
      Err e -> LoginFailure e
      Ok lr -> LoginSuccess lr
  in
    Login.loginPost 
      { onSend = resultToMsg
      , body = 
        { username = output.email
        , password = output.password
        , mergeToken = Nothing
        }
      }


tryGetUser : LoginResponse -> Cmd Msg
tryGetUser lr = 
  let
    resultToMsg r = case r of
      Err e -> GetUserFailure e
      Ok u -> GetUserSuccess u
  in
    Users.usersUuidGet
      { authorization = Just ("OAuth " ++ lr.token)
      , cacheControl = Nothing
      }
      { onSend = resultToMsg
      , uuid = lr.uuid
      }

tryPatchUser : AddressFormOutput -> LoginResponse -> Cmd Msg
tryPatchUser output lr =
  let
    resultToMsg r = case r of
      Err e -> SetAddrFailure e
      Ok u -> SetAddrSuccess u
    userUpdate = 
      { firstName = Nothing
      , lastName = Nothing
      , address = Just
        { streetAddress = output.streetAddress
        , zipCode = output.zipCode
        -- It looks like the API can only deal with addresses in Finland?
        , countryCode = "FI"
        }
      }
  in
    Users.usersUuidPatch
      { authorization = Just ("OAuth " ++ lr.token)
      }
      { onSend = resultToMsg
      , uuid = lr.uuid
      , body = userUpdate
      }

stringFromMaybe : Maybe String -> String
stringFromMaybe m = case m of
  Just s -> s
  Nothing -> ""


-- VIEW
view : Model -> Html Msg
view model =
  case model of
    AtLoginForm fvm _ note ->
      let
        form_html =
          Form.View.asHtml
            { onChange = (\fvm_new -> LoginFormChanged fvm_new)
            , action = "Log in"
            , loading = "Loading..."
            , validation = Form.View.ValidateOnSubmit
            }
            (Form.map (\output -> ClickLogin output) loginForm) 
            fvm
      in
        div []
          [ viewNote note
          , form_html
          ]
    ShowingUser u lr note ->
        div [] 
          [ viewNote note
          , viewNameAddress u
          , div [] 
              [ button [ onClick ClickEditAddress ] [ text "Edit address" ] 
              ]
          , logoutButton
          ]
    AtAddressForm fvm u lr note -> 
      let
        form_html = 
          Form.View.asHtml
            { onChange = (\fvm_new -> AddressFormChanged fvm_new)
            , action = "Set new address"
            , loading = "Working..."
            , validation = Form.View.ValidateOnSubmit
            }
            (Form.map (\output -> ClickSubmitAddress output) addressForm) 
            fvm
      in
        div [] 
          [ viewNote note
          , form_html
          , logoutButton
          ]

logoutButton : Html Msg
logoutButton = 
  button [ onClick ClickLogout ] [ text "Log out" ] 

-- debugLogThen : String -> a -> b -> b
-- debugLogThen s a b = Tuple.second (Debug.log s a, b)

viewNote : Maybe Note -> Html Msg
viewNote mn = case mn of
  Nothing -> 
    text ""
  Just (Note s) ->
    div [ class "note noError" ] [ text s ]
  Just (ErrorNote s) ->
    div [ class "note yesError" ] [ text s ]

viewNameAddress : User -> Html Msg
viewNameAddress u = 
  div [] (
    [ h5 [] [text "Name"]
    , text (getName u)
    , br [] []
    ] 
    ++ viewAddressOrMissing u.address
    )

viewAddressOrMissing : Maybe Address -> List (Html Msg)
viewAddressOrMissing m = case m of
  Nothing -> 
    [text "Address not available"]
  Just a -> 
    [h5 [] [text "Address"]]
      ++ simpleLines (addressLines a)

simpleLines : List String -> List (Html Msg)
simpleLines l = case l of
  [] -> 
    []
  (s :: []) ->
    [text s]
  (s :: ss) ->
    [text s, br [] []] ++ (simpleLines ss)

addressLines : Address -> List String
addressLines a = 
  collectJust
    [ Just a.streetAddress
    , a.zipCode
    , a.city
    ]

collectJust : List (Maybe a) -> List a
collectJust l = case l of
  [] -> []
  Nothing :: mxs -> collectJust mxs
  ((Just x) :: mxs) -> x :: (collectJust mxs)

-- TODO: strip spaces
getName : User -> String
getName u = stringFromMaybe u.firstName ++ " " ++ stringFromMaybe u.lastName

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- FORMS
type alias LoginFormValues =
  { email : String
  , password : String
  }

type alias LoginFormOutput =
  { email : String
  , password : String
  }

type alias AddressFormValues = 
  { streetAddress : String
  , zipCode : String
  , city : String
  }

type alias AddressFormOutput =
  { streetAddress : String
  , zipCode : String
  , city : String
  }

loginForm : Form LoginFormValues LoginFormOutput
loginForm =
  let
    emailField =
      Form.textField
        { parser = nonEmpty
        , error = noError
        , value = .email
        , update = \value values -> { values | email = value }
        , attributes =
          { label = "E-Mail"
          , placeholder = ""
          }
        }
    passwordField =
      Form.passwordField
        { parser = nonEmpty
        , error = noError
        , value = .password
        , update = \value values -> { values | password = value }
        , attributes =
          { label = "Password"
          , placeholder = ""
          }
        }
  in
    Form.succeed LoginFormOutput
      |> Form.append emailField
      |> Form.append passwordField

addressForm : Form AddressFormValues AddressFormOutput
addressForm =
  let 
    streetAddressField =
      Form.textField
        { parser = nonEmpty
        , error = noError
        , value = .streetAddress
        , update = \value values -> { values | streetAddress = value }
        , attributes =
          { label = "Street address"
          , placeholder = ""
          }
        }
    zipCodeField = 
      Form.textField
        { parser = nonEmpty
        , error = noError
        , value = .zipCode
        , update = \value values -> { values | zipCode = value }
        , attributes =
          { label = "Zip code"
          , placeholder = ""
          }
        }
    cityField =
      Form.textField
        { parser = nonEmpty
        , error = noError
        , value = .city
        , update = \value values -> { values | city = value }
        , attributes =
          { label = "City "
          , placeholder = ""
          }
        }
    in
      Form.succeed AddressFormOutput
        |> Form.append streetAddressField
        |> Form.append zipCodeField
        |> Form.append cityField

populateAddressForm : User -> AddressFormValues
populateAddressForm u =
  case u.address of
    Nothing ->
      { streetAddress = ""
      , zipCode = ""
      , city = ""
      }
    Just a ->
      { streetAddress = a.streetAddress
      , zipCode = stringFromMaybe a.zipCode
      , city = stringFromMaybe a.city
      }

nonEmpty : String -> Result String String
nonEmpty s = case s of
  "" ->
    Err "Cannot be empty"
  _ ->
    Ok s

noError : a -> Maybe String
noError a = Nothing
