module Retro where

import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Json

---- MODEL ----

happy = ":)"
meh = ":|"
sad = ":("

type alias Model =
  { happy: Column
  , meh: Column
  , sad: Column
  , newItem: String
  , newItemHeading: String
  }

type alias Column =
  { heading: String
  , items: List String
  }

emptyModel =
  { newItem = ""
  , newItemHeading = happy
  , happy = {heading = happy, items = []}
  , meh = {heading = meh, items = []}
  , sad = {heading = sad, items = []}
  }

---- UPDATE ----

type Action
  = AddItem
  | UpdateNewItem String
  | UpdateNewItemHeading String

update : Action -> Model -> Model
update action model =
  case action of
    AddItem ->
      if model.newItemHeading == happy then
        updateHappy model
      else if model.newItemHeading == meh then
        updateMeh model
      else if model.newItemHeading == sad then
        updateSad model
      else model
    UpdateNewItem str ->
      {model | newItem = str}
    UpdateNewItemHeading str ->
      {model | newItemHeading = str}

-- oh god how do i not duplicate all this?
updateHappy model =
  let
    old = model.happy
    new = {old | items = model.newItem :: old.items}
  in {model |
    newItem = "",
    happy = new
  }

updateMeh model =
  let
    old = model.meh
    new = {old | items = model.newItem :: old.items}
  in {model |
    newItem = "",
    meh = new
  }

updateSad model =
  let
    old = model.sad
    new = {old | items = model.newItem :: old.items}
  in {model |
    newItem = "",
    sad = new
  }

---- VIEW ----

view : Signal.Address Action -> Model -> Html
view address model =
  div [] [columnViews model, itemEntry address model.newItem]

columnViews : Model -> Html
columnViews model =
  div [] (List.map (\i -> columnView i) [model.happy, model.meh, model.sad])

columnView : Column -> Html
columnView column =
  div [] [text column.heading, itemList column.items]

itemList : List String -> Html
itemList items =
  ul [] (List.map (\i -> li [] [text i]) items)

itemEntry : Signal.Address Action -> String -> Html
itemEntry address item =
  div [] [itemHeadingSelector address item, itemTextField address item]

itemHeadingSelector : Signal.Address Action -> String -> Html
itemHeadingSelector address heading =
  select
    [ Events.on "input" Events.targetValue
        (Signal.message address << UpdateNewItemHeading)]
    [ option [] [text happy]
    , option [] [text meh]
    , option [] [text sad]
    ]

itemTextField : Signal.Address Action -> String -> Html
itemTextField address item =
  input
    [ Attributes.placeholder "wagwan?"
    , onEnter address AddItem
    , Attributes.value item
    , Events.on "input" Events.targetValue
        (Signal.message address << UpdateNewItem)
    ]
    []

isEnter : Int -> Result String ()
isEnter code =
  if code == 13
    then Ok ()
    else Err "key code is not enter"

onEnter : Signal.Address a -> a -> Attribute
onEnter address value =
  Events.on "keydown"
    (Json.customDecoder Events.keyCode isEnter)
    (\_ -> Signal.message address value)
