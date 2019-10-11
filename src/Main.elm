module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)



type Poodle
  = Standard
  | Miniature
  | Toy


type Msg
  = FetchPoodle
  | RecievePoodle (Result Http.Error String)


type alias Model =
  { poodleType : Poodle
  , hashAnchor : Maybe String
  , imageSrc : Maybe String
  }


poodleDecoder : Decoder String
poodleDecoder = 
  Decode.field "message" Decode.string


apiUrl : String -> List String
apiUrl breed =
  [ "https://dog.ceo/api/breed/", breed, "/images/random" ]


createApiUrl : Poodle -> String
createApiUrl poodle =
  String.join "" (fromPoodle poodle |> apiUrl)


fetchPoodle : Poodle -> Cmd Msg
fetchPoodle poodleType =
  Http.get
    { url = poodleType |> createApiUrl
    , expect = Http.expectJson RecievePoodle poodleDecoder
    }


init : () -> (Model, Cmd Msg)
init _ =
  ( { poodleType = Miniature
    , hashAnchor = Nothing
    , imageSrc = Nothing
    }
  , fetchPoodle Miniature
  )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    FetchPoodle -> 
      ( model, fetchPoodle model.poodleType )

    RecievePoodle (Ok imageSrc) ->
      ( { model | imageSrc = Just imageSrc }
      , Cmd.none 
      )

    RecievePoodle (Err error) ->
      ( model, Cmd.none )


renderView : Model -> List (Html Msg)
renderView model =
  [ div [ class "container" ]
    [ header [] [ h1 [] [ text "Poodle Button!" ] ]
    , main_ [] 
        [ div [ class "content"] 
            [ div [] 
                [img 
                  [ src (model.imageSrc |> Maybe.withDefault "")
                  , alt (fromPoodle model.poodleType)] 
                  []
                ]
            , div [ style "padding-top" "12px" ] 
                [ button [ onClick FetchPoodle ] [ text "Gimme a Poodle!" ]
                ]
            ]
        ]
    , footer [] 
        [ h2 [ style "color" "rebeccapurple" ] [ text "Acknowledgments!" ]
        , p [] 
            [ text "Poodles provided by ", a [ href "https://dog.ceo/dog-api/" ] [ text "dog.ceo API" ], text "."
            , br [] []
            , text "Inspired by ", a [ href "http://corgi-button.glitch.me" ] [ text "Corgi Button" ], text "."
            , br [] []
            , text "Code and hosting by ", a [ href "https://glitch.com/edit/#!/poodle-button" ] [ text "Glitch" ], text "."
            , br [] []
            , text "Made in 2019"
            ]
        ]
    ]
  ]


view : Model -> Document Msg
view model =
  { title = "Poodle Button!"
  , body = renderView model
  }


main =
    Browser.document
      { init = init
      , view = view
      , update = update
      , subscriptions = (\_ -> Sub.none)
      }  


fromPoodle : Poodle -> String
fromPoodle poodle =
  case poodle of
    Standard ->
      "poodle/standard"

    Miniature ->
      "poodle/miniature"

    Toy ->
      "poodle/toy"
