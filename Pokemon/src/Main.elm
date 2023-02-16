module Main exposing (..)

import Browser
import Debug exposing (todo)
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)



---- MODEL ----


type Model
    = LandingPage
    | Loading
    | PokemonList (List PokemonName)
    | PokemonDetails PokemonName
    | ErrorModel String


init : ( Model, Cmd Msg )
init =
    ( LandingPage, Cmd.none )



---- UPDATE ----


type Msg
    = GetPokemon
    | GotPokemon (List PokemonName)
    | GetPokemonDetails PokemonName
    | GotPokemonDetails PokemonName
    | ErrMsg String


type alias PokemonName =
    { name : String
    , url : String
    }


type alias Pokemon =
    { abilities : List Ability
     , back_default : String}


type alias Ability = String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (msg , model) of
        (GetPokemon , LandingPage) -> (Loading , getPokemon)
        (GotPokemon lpn , Loading) -> (PokemonList lpn , Cmd.none)
        (GetPokemonDetails pn , PokemonList lpn) -> (PokemonList lpn, getpokemonDetails pn)
        (GotPokemonDetails pn , PokemonList lpn) -> (PokemonDetails pn , Cmd.none)
        (ErrMsg s, _) -> (ErrorModel s , Cmd.none)
        _ -> todo ""



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }


getPokemon : Cmd Msg
getPokemon = todo ""

getpokemonDetails : PokemonName -> Cmd Msg
getpokemonDetails pn = todo ""