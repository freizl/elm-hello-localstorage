module Main where
{-|
-}

import Json.Decode (..)
import Json.Encode
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Html.Lazy (lazy, lazy2)
import Json.Decode as Json
import List
import Maybe
import Signal
import Signal(..)
import String
import Window
import Result(..)
import Date(..)

---- MODEL ----

-- The full application state of our todo app.

type alias Model = 
    { cacheEntry : Maybe Bool
    , value      : EntryValue
    , expire     : Date
    , isPast     : Bool
    , key        : String
    , selected   : Maybe Bool
    }

type EntryValue = EV1 Json.Value 
                  | EV2 (List String) 
                  | EV3 Offer
                  -- | EV4 Manifest
                  -- | EV5 Info
                  -- | EV6 Json.Value

type alias Info = {ip : String, country : String, region : String}

type alias Offer = { price : Int }

type alias Manifest = 
    { pub : String
    , pubZone : String
    }

type Action
    = NoOp
    | RowClick String  -- selected row with its key passed


entryDecoder : Decoder Model
entryDecoder = Json.object6 Model
               (maybe ("cacheEntry"    := bool))
               ("value"         := valueDecoder)
               ("expire"        := (Json.map fromTime float))
               ("isPast"        := Json.bool)
               ("key"           := string)
               (maybe ("selected"      := succeed False))

toDate : String -> Maybe Date
toDate str = case fromString str of
               Ok v -> Just v
               Err _ -> Nothing

valueDecoder : Decoder EntryValue
valueDecoder = oneOf [ Json.object1 EV2 (Json.list string)
                     , Json.object1 EV3 offerDecoder
                     , Json.object1 EV1 Json.value
                     -- , Json.object1 EV4 manifestDecoder
                     --, Json.object1 EV5 infoDecoder
                     -- , Json.object1 EV6 Json.value
                     ]


offerDecoder : Decoder Offer
offerDecoder = Json.object1 Offer 
               ("price" := Json.int)

manifestDecoder : Decoder Manifest
manifestDecoder = Json.object2 Manifest 
               ("pub" := string)
               ("pub_zone" := string)

infoDecoder : Decoder Info
infoDecoder = Json.object3 Info 
               ("ip" := string)
               ("country" := string)
               ("region" := string)

-- How we update our Model on a given Action?

update : Action -> List Model -> List Model
update action xs = case action of
  RowClick key -> let updateSelection t = if t.key == key then { t | selected <- Just True } else t
                  in List.map updateSelection xs
  _            -> xs


---- VIEW ----

view : List Model -> Html
view items = 
  let xs = items
      s = List.filter isSelected xs
  in
  div 
    [ class "row main" ]
    <|
    [ div [class ""]
          [ btn "Refresh" (Signal.send toLS "refresh")
          , btn "Clear" (Signal.send toLS "clear") 
          ]
    , table [ class "collection responsive-table striped" ] 
            (tHeader :: (List.map makeLi xs))
    , div [class "stick-bottom"] 
          (List.map viewSelected s)
    ]
  
      
isSelected : Model -> Bool
isSelected m = m.selected == Just True

viewSelected : Model -> Html
viewSelected m = 
  div [class "card-panel teal"]
      [ span [class "white-text"]
             [vToHtml m.value Nothing]
      ]


tHeader : Html
tHeader = tr [class "collection"] 
          [ th [] [text "Key"]
          , th [] [text "Value"]
          , th [] [text "Expire"]
          ]

makeLi : Model -> Html
makeLi model = tr 
  [ recordStyle model 
  , onClick (Signal.send updates (RowClick model.key))
  ]
  [ td [] [text model.key]
  , td [class "hide-on-small-only"] [vToHtml model.value (Just 100)]
  , td [] [isPastFlag model.isPast, text (formatDate model.expire)]
  ]

isPastFlag : Bool -> Html
isPastFlag b = case b of 
                True -> text "✘"
                False -> text "✓"

recordStyle : Model -> Attribute
recordStyle item = case item.selected of
                     Just True -> class "collection-item active"
                     _         -> class "collection-item"

btn : String -> Message -> Html
btn name handler = a 
      [ id name
      , class "waves-effect waves-light btn-large"
      , onClick (Signal.send toLS name)
      ]
      [ text name ]

vToHtml : EntryValue -> Maybe Int -> Html
vToHtml v takes = 
  case v of
    EV2 xs -> ul [] (List.map liKey xs)
    EV3 offer -> text ("price: " ++ toString offer.price)
    EV1 v -> let str = Json.Encode.encode 2 v
                 len = String.length str
                 n = Maybe.withDefault len takes
             in
                text (String.left n str)
    _ -> text "unknown value"

liKey : String -> Html
liKey str = li [] [text str]

formatDate : Date -> String
formatDate d = String.join ":"
               <| [String.join "-"
                             [ toString (year d)
                             , toString (month d) 
                             , toString (day d) 
                             ]
                 , String.join "-" 
                           [ toString (hour d) 
                           , toString (minute d) 
                           , toString (second d) 
                           ]
                  ]

---- INPUTS ----

-- wire the entire application together
main : Signal Html
main = Signal.map view model

-- manage the model of our application over time
model : Signal (List Model)
model = Signal.map2 update (Signal.subscribe updates) initialModel

initialModel : Signal (List Model)
initialModel = Signal.map decodeValues getStorage

decodeValues : String -> List Model
decodeValues str = case Json.decodeString (Json.list entryDecoder) str of
                     Ok v -> v
                     Err _ -> []

-- updates from user input
updates : Signal.Channel Action
updates = Signal.channel NoOp

toLS : Signal.Channel String
toLS = Signal.channel ""

-- get all entries as array in one json string
port getStorage : Signal String

port refreshStorage : Signal String
port refreshStorage = Signal.subscribe toLS
