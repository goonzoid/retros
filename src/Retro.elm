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
  , newItemID: Int
  , newItemText: String
  , newItemHeading: String
  }

type alias Column =
  { heading: String
  , items: List Item
  }

type alias Item =
  { id: Int
  , text: String
  , crossedOff: Bool
  }

emptyModel =
  { newItemID = 1
  , newItemText = ""
  , newItemHeading = happy
  , happy = {heading = happy, items = []}
  , meh = {heading = meh, items = []}
  , sad = {heading = sad, items = []}
  }

newItem =
  { id = 0
  , text = ""
  , crossedOff = False
  }

---- UPDATE ----

type Action
  = AddItem
  | UpdateNewItem String
  | UpdateNewItemHeading String
  | CrossOff Int

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
      {model | newItemText = str}
    UpdateNewItemHeading str ->
      {model | newItemHeading = str}
    CrossOff id ->
      updateModel model id

updateModel : Model -> Int -> Model
updateModel model itemID =
  {model |
    happy = updateColumn model.happy itemID,
    meh = updateColumn model.meh itemID,
    sad = updateColumn model.sad itemID
  }

updateColumn : Column -> Int -> Column
updateColumn column itemID =
  {column | items = (List.map (\i -> updateItem i itemID) column.items)}

updateItem : Item -> Int -> Item
updateItem item itemID =
  if item.id == itemID
  then {item | crossedOff = (not item.crossedOff)}
  else item

-- oh god how do i not duplicate all this?
updateHappy model =
  let
    old = model.happy
    item = {newItem | id = model.newItemID, text = model.newItemText}
    new = {old | items = item :: old.items}
  in {model |
    newItemID = model.newItemID + 1,
    newItemText = "",
    happy = new
  }

updateMeh model =
  let
    old = model.meh
    item = {newItem | id = model.newItemID, text = model.newItemText}
    new = {old | items = item :: old.items}
  in {model |
    newItemID = model.newItemID + 1,
    newItemText = "",
    meh = new
  }

updateSad model =
  let
    old = model.sad
    item = {newItem | id = model.newItemID, text = model.newItemText}
    new = {old | items = item :: old.items}
  in {model |
    newItemID = model.newItemID + 1,
    newItemText = "",
    sad = new
  }

---- VIEW ----

view : Signal.Address Action -> Model -> Html
view address model =
  div [] [columnViews address model, itemEntry address model.newItemText]

columnViews : Signal.Address Action -> Model -> Html
columnViews address model =
  div [] (List.map (\i -> columnView address i) [model.happy, model.meh, model.sad])

columnView : Signal.Address Action -> Column -> Html
columnView address column =
  div [] [text column.heading, itemList address column.items]

itemList : Signal.Address Action -> List Item -> Html
itemList address items =
  ul [] (List.map (\i -> itemListEntry address i) items)

itemListEntry : Signal.Address Action -> Item -> Html
itemListEntry address item =
  let styles =
    if item.crossedOff
    then [("text-decoration", "line-through")]
    else []
  in
    li
      [ Events.onClick address (CrossOff item.id)
      , Attributes.style styles
      ]
      [text item.text]

itemEntry : Signal.Address Action -> String -> Html
itemEntry address itemText =
  div [] [itemHeadingSelector address, itemTextField address itemText]

itemHeadingSelector : Signal.Address Action -> Html
itemHeadingSelector address =
  select
    [ Events.on "input" Events.targetValue
        (Signal.message address << UpdateNewItemHeading)]
    [ option [] [text happy]
    , option [] [text meh]
    , option [] [text sad]
    ]

itemTextField : Signal.Address Action -> String -> Html
itemTextField address itemText =
  input
    [ Attributes.placeholder "wagwan?"
    , onEnter address AddItem
    , Attributes.value itemText
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
