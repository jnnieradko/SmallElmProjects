module Main exposing (..)

import Browser
import Cmd.Extra exposing (..)
import Debug exposing (todo)
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Http exposing (..)
import Json.Decode as JD exposing (Decoder)



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
    | GotPokemonList (Result Http.Error (List PokemonName))
    | GetPokemonDetails PokemonNameUrl
    | GotPokemonDetails Pokemon
    | ErrMsg String


type alias PokemonName =
    { name : String
    , url : String
    }


type alias Pokemon =
    { abilities : List Ability
    , back_default : String
    }

type alias PokemonNameUrl = String

type alias Ability =
    String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPokemonList ->
            ( Loading, getPokemonList )

        GotPokemonList relpn ->
            case relpn of
                Err e ->
                    ( ErrorModel "Błąd", Cmd.none )

                Ok v ->
                    ( PokemonList v, Cmd.none )

        GetPokemonDetails pnUrl ->
            ( model, getpokemonDetails pnUrl )

        GotPokemonDetails p ->
            ( PokemonDetails p, Cmd.none )

        ErrMsg s ->
            ( ErrorModel s, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        LandingPage ->
            div [] [ text "Hello !", button [ onClick GetPokemonList ] [ text " Wyświetl listę Pokemonów" ] ]

        Loading ->
            div [] [ text "laduję dane ... " ]

        PokemonList lpn ->
            div [] (listOfPokemonNames lpn )

        PokemonDetails p ->
            div [] [ text "dane szczegółowe o Pokemonie", button [ onClick GetPokemonList ] [ text "Wróć do listy Pokemonów" ]  ]

        _ ->
            div [] []



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



-- Http request Get -- return list of Pokemon's names


getPokemonList : Cmd Msg
getPokemonList =
    Http.get
        { url = "https://pokeapi.co/api/v2/pokemon"
        , expect = Http.expectJson GotPokemonList dekoder
        }


funkcja =
    todo ""


dekoderPok : Decoder PokemonName
dekoderPok = JD.map2 PokemonName
                (JD.field "name" JD.string)
                (JD.field "url" JD.string)


dekoder : Decoder (List PokemonName)
dekoder = JD.at ["results"] (JD.list dekoderPok)






--Cmd.Extra.perform (GotPokemonList [{name = "Pikaczu" , url = "e-orzecznik.pl"}])
-- Http request Get -- return Pokemon's details


getpokemonDetails : PokemonNameUrl -> Cmd Msg
getpokemonDetails pnUrl =
    Cmd.Extra.perform (GotPokemonDetails { abilities = [], back_default = pnUrl })



-- function used in view to display list of pokemon's name


listOfPokemonNames : List PokemonName -> List (Html Msg)
listOfPokemonNames lpn =
    List.map (\pn -> div [ onClick (GetPokemonDetails pn.url) ] [ text pn.name ]) lpn
