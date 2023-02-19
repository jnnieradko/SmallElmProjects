module Main exposing (..)

import Browser
import Cmd.Extra exposing (..)
import Debug exposing (todo)
import Html exposing (Attribute, Html, button, div, h1, img, li, text, ul)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Http exposing (..)
import Json.Decode as JD exposing (Decoder)



---- MODEL ----


type Model
    = LandingPage
    | Loading
    | PokemonList (Maybe String) (Maybe String) (List PokemonName)
    | PokemonDetails String Pokemon
    | ErrorModel String


init : ( Model, Cmd Msg )
init =
    ( LandingPage, Cmd.none )



---- UPDATE ----


type Msg
    = FirstGetPokemonList String
    | GetPokemonList (Maybe String)
    | GotPokemonList (Result Http.Error Pokemons)
    | GetPokemonDetails PokemonName
    | GotPokemonDetails String (Result Http.Error Pokemon)
    | ErrMsg String


type alias Pokemons =
    { next : Maybe String
    , previos : Maybe String
    , allPokemons : List PokemonName
    }


type alias PokemonName =
    { name : String
    , url : String
    }


type alias Pokemon =
    { abilities : List Ability
    , img : String
    }



--


type alias PokemonNameUrl =
    String


type alias Ability =
    String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FirstGetPokemonList s -> (Loading , getPokemonList s)
        GetPokemonList ms ->
            case ms of
                Just x -> ( Loading, getPokemonList x )
                Nothing -> (Loading, getPokemonList "https://pokeapi.co/api/v2/pokemon")


        GotPokemonList relPokeomns ->
            case relPokeomns of
                Err e ->
                    ( ErrorModel "Błąd", Cmd.none )

                Ok v ->
                    ( PokemonList v.next v.previos v.allPokemons, Cmd.none )

        GetPokemonDetails pn ->
            ( model, getpokemonDetails pn )

        GotPokemonDetails s rePok ->
            case rePok of
                Err e ->
                    ( ErrorModel "Błąd", Cmd.none )

                Ok v ->
                    ( PokemonDetails s v, Cmd.none )

        --( PokemonDetails rePok, Cmd.none )
        ErrMsg s ->
            ( ErrorModel s, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        LandingPage ->
            div []
                [ text "Hello !"
                , button
                    [ onClick (FirstGetPokemonList "https://pokeapi.co/api/v2/pokemon" ) ]
                    [ text " Wyświetl listę Pokemonów" ]
                ]

        Loading ->
            div [] [ text "laduję dane ... " ]

        PokemonList next previous lpn ->
            div []
                [ div [] (listOfPokemonNames lpn)
                , div []
                    [ button [onClick (GetPokemonList previous)] [ text "Previous",  text (Maybe.withDefault "" previous) ]
                    , button [onClick (GetPokemonList next)] [ text "Next" , text (Maybe.withDefault "" next) ]
                    ]
                ]

        PokemonDetails s pok ->
            div []
                [ text ("Dane szczegółowe o Pokemonie  " ++ s ++ " ")
                , ul [] (listOfPokemonAbilities pok)
                , img [ pokemonImg pok ] []
                , button
                    [ onClick (FirstGetPokemonList "https://pokeapi.co/api/v2/pokemon") ]
                    [ text "Wróć do listy Pokemonów" ]
                ]

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


getPokemonList : String ->  Cmd Msg
getPokemonList s =
    Http.get
        { url = s
        , expect = Http.expectJson GotPokemonList dekoderPokemons
        }


dekoderPokName : Decoder PokemonName
dekoderPokName =
    JD.map2 PokemonName
        (JD.field "name" JD.string)
        (JD.field "url" JD.string)


dekoderPokemons : Decoder Pokemons
dekoderPokemons =
    JD.map3 Pokemons
        (dekoderPaginationNext)
        (dekoderPaginationPrevious)
        (JD.at [ "results" ] (JD.list dekoderPokName))

dekoderPaginationNext : Decoder (Maybe String)
dekoderPaginationNext = JD.field "next" (JD.maybe JD.string)

dekoderPaginationPrevious : Decoder (Maybe String)
dekoderPaginationPrevious = JD.field "previous" (JD.maybe JD.string)


-- function used in view to display list of pokemon's name


listOfPokemonNames : List PokemonName -> List (Html Msg)
listOfPokemonNames lpn =
    List.map (\pn -> div [ onClick (GetPokemonDetails pn) ] [ text pn.name ]) lpn



-- Http request Get -- return Pokemon's details


getpokemonDetails : PokemonName -> Cmd Msg
getpokemonDetails pn =
    Http.get
        { url =
            "https://pokeapi.co/api/v2/pokemon/" ++ pn.name
        , expect =
            Http.expectJson
                (GotPokemonDetails pn.name)
                dekoderPokemon
        }


dekoderPokemon : Decoder Pokemon
dekoderPokemon =
    JD.map2 Pokemon
        (JD.field "abilities" <|
            JD.list <|
                JD.at [ "ability", "name" ] <|
                    JD.string
        )
        (JD.at [ "sprites", "back_default" ] <|
            JD.string
        )



-- function used in view in tag img - for pokemon's Image


pokemonImg : Pokemon -> Attribute msg
pokemonImg pok =
    src pok.img



-- function used in view to display list of pokemon's abilities


listOfPokemonAbilities : Pokemon -> List (Html Msg)
listOfPokemonAbilities pok =
    List.map (\pa -> li [] [ text pa ]) pok.abilities
