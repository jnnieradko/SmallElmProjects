module Main exposing (..)

import Browser
import Debug exposing (todo)
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Cmd.Extra exposing (..)
import Html.Events exposing (onClick)



---- MODEL ----


type Model
    = LandingPage
    | Loading
    | PokemonList (List PokemonName)
    | PokemonDetails Pokemon
    | ErrorModel String


init : ( Model, Cmd Msg )
init =
    ( LandingPage, Cmd.none )



---- UPDATE ----


type Msg
    = GetPokemonList
    | GotPokemonList (List PokemonName)
    | GetPokemonDetails PokemonName
    | GotPokemonDetails Pokemon
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
    case msg of
        GetPokemonList -> (Loading , getPokemonList)
        GotPokemonList lpn -> (PokemonList lpn , Cmd.none)
        GetPokemonDetails pn -> (model, getpokemonDetails pn)
        GotPokemonDetails p -> (PokemonDetails p , Cmd.none)
        ErrMsg s -> (ErrorModel s , Cmd.none)


---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        LandingPage -> div [] [text "Hello !" , button [onClick GetPokemonList ] [text " Wyświetl listę Pokemonów"]]
        Loading -> div [] [text "laduję dane ... "]
        PokemonList lpn ->  div [] (listOfPokemonNames lpn)
        PokemonDetails p -> div [] [text "dane szczegółowe o Pokemonie" , button [onClick GetPokemonList ] [text "Wróć do listy Pokemonów"]]
        _ -> div [] []



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



getPokemonList : Cmd Msg
getPokemonList = Cmd.Extra.perform (GotPokemonList [{name = "Pikaczu" , url = "e-orzecznik.pl"}])

getpokemonDetails : PokemonName -> Cmd Msg
getpokemonDetails pn = Cmd.Extra.perform (GotPokemonDetails {abilities = [] , back_default = pn.url})

listOfPokemonNames : List PokemonName -> List (Html Msg)
listOfPokemonNames lpn = List.map(\pn -> div [onClick (GetPokemonDetails pn) ] [text pn.name])lpn