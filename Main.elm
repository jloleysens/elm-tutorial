module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Regex exposing (contains)


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }


type alias Model =
    { int : Int
    , content : String
    , name : String
    , password : String
    , passwordRepeat : String
    , age : String
    , submitted : Bool
    }


model : Model
model =
    { int = 0
    , content = ""
    , name = ""
    , password = ""
    , passwordRepeat = ""
    , age = ""
    , submitted = False
    }


type Msg
    = Increment
    | Decrement
    | Reverse String
    | Name String
    | Password String
    | PasswordRepeat String
    | Age String
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Age age ->
            { model | age = age }

        Increment ->
            { model | int = model.int + 1 }

        Decrement ->
            { model | int = model.int - 1 }

        Reverse newString ->
            { model | content = newString }

        Submit ->
            { model | submitted = True }

        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordRepeat passwordRepeat ->
            { model | passwordRepeat = passwordRepeat }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (toString model.int) ]
        , button [ onClick Increment ] [ text "+" ]
        , br [] []
        , input [ placeholder "Text to reverse", onInput Reverse ] []
        , div [] [ text (String.reverse model.content) ]
        , br [] []
        , input [ type_ "text", placeholder "Name", onInput Name ] []
        , input [ type_ "text", placeholder "Age", onInput Age ] []
        , input [ type_ "text", placeholder "Password", onInput Password ] []
        , input [ type_ "text", placeholder "Repeat Password", onInput PasswordRepeat ] []
        , button [ onClick Submit ] [ text "Submit" ]
        , viewValidation model
        ]


passwordRegex : Regex.Regex
passwordRegex =
    Regex.regex "^(?=.*[A-Z])(?=.*[a-z])(?=.*\\d)[A-Za-z0-9]{8,}$"


checkAgeInput : String -> Bool
checkAgeInput age =
    case String.toInt age of
        Err msg ->
            False

        Ok age ->
            True


viewValidation : Model -> Html msg
viewValidation model =
    if model.submitted then
        let
            ( color, message ) =
                if checkAgeInput model.age /= True then
                    ( "red", "Age must be a number" )
                else if model.password /= model.passwordRepeat then
                    ( "red", "Passwords do not match!" )
                else if String.length model.password < 8 then
                    ( "red", "Password too short! Must be 8 chars!" )
                else if Regex.contains passwordRegex model.password == False then
                    ( "red", "Must contain at least 1 uppercase, 1 lowercase letter and 1 digit." )
                else
                    ( "green", "OK" )
        in
            div [ style [ ( "color", color ) ] ] [ text message ]
    else
        div [] []
