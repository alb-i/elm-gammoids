module CanonicalBaseIndicatorStrings exposing (main)

-- A text input for reversing text. Very useful!
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/text_fields.html
--

import Browser
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { n : String
    , -- cardinality of matroid edge set
      rank : String
    , -- rank of the matroid
      basisIndicator : String -- basis indicator
    }


init : Model
init =
    { n = "6", rank = "3", basisIndicator = "XXXXXX..XXX.XX.XXXXX" }



-- UPDATE


type Msg
    = ChangeRank String
    | ChangeN String
    | ChangeBasisIndicator String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeRank newRank ->
            { model | rank = newRank }

        ChangeN newN ->
            { model | n = newN }

        ChangeBasisIndicator newIndicator ->
            { model | basisIndicator = newIndicator }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "Number of elements", input [ placeholder "matroid edge count", value model.n, onInput ChangeN ] [] ]
        , div []
            [ text "Matroid rank"
            , input [ placeholder "matroid rank", value model.rank, onInput ChangeRank ] []
            ]
        , div []
            [ text "Basis Indicator String"
            , input [ placeholder "basis indicator", value model.basisIndicator, onInput ChangeBasisIndicator ] []
            ]
        , div [] [ text "XX" ]
        ]
