module Retro (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Json.Decode as Json
import String exposing (isEmpty)


---- MODEL ----


happy : String
happy =
  ":)"


meh : String
meh =
  ":|"


sad : String
sad =
  ":("


type alias Model =
  { happy : Column
  , meh : Column
  , sad : Column
  , newItemID : Int
  , newItemText : String
  , newItemHeading : String
  }


type alias Column =
  { heading : String
  , items : List Item
  }


type alias Item =
  { id : Int
  , text : String
  , crossedOff : Bool
  }


emptyModel : Model
emptyModel =
  { newItemID = 1
  , newItemText = ""
  , newItemHeading = happy
  , happy = { heading = happy, items = [] }
  , meh = { heading = meh, items = [] }
  , sad = { heading = sad, items = [] }
  }


newItem : Item
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
      if String.isEmpty model.newItemText then
        model
      else
        addItemToModel model

    UpdateNewItem str ->
      { model | newItemText = str }

    UpdateNewItemHeading str ->
      { model | newItemHeading = str }

    CrossOff id ->
      crossOffItemInModel model id


addItemToModel : Model -> Model
addItemToModel model =
  let
    item =
      { newItem | id = model.newItemID, text = model.newItemText }

    happyColumn =
      addItemToColumn model.happy model.newItemHeading item

    mehColumn =
      addItemToColumn model.meh model.newItemHeading item

    sadColumn =
      addItemToColumn model.sad model.newItemHeading item
  in
    { model
      | newItemID = model.newItemID + 1
      , newItemText = ""
      , happy = happyColumn
      , meh = mehColumn
      , sad = sadColumn
    }


addItemToColumn : Column -> String -> Item -> Column
addItemToColumn column heading item =
  if column.heading == heading then
    { column | items = column.items ++ [ item ] }
  else
    column


crossOffItemInModel : Model -> Int -> Model
crossOffItemInModel model itemID =
  { model
    | happy = crossOffItemInColumn model.happy itemID
    , meh = crossOffItemInColumn model.meh itemID
    , sad = crossOffItemInColumn model.sad itemID
  }


crossOffItemInColumn : Column -> Int -> Column
crossOffItemInColumn column itemID =
  { column | items = (List.map (\i -> crossOffItem i itemID) column.items) }


crossOffItem : Item -> Int -> Item
crossOffItem item itemID =
  if item.id == itemID then
    { item | crossedOff = (not item.crossedOff) }
  else
    item



---- VIEW ----


view : Signal.Address Action -> Model -> Html
view address model =
  div [] [ columnViews address model, br [] [], itemInput address model.newItemText ]


columnViews : Signal.Address Action -> Model -> Html
columnViews address model =
  div
    [ class "row" ]
    (List.map
      (\i -> columnView address i)
      [ model.happy, model.meh, model.sad ]
    )


columnView : Signal.Address Action -> Column -> Html
columnView address column =
  div
    [ class "columns"
    , style [ ( "text-align", "center" ) ]
    ]
    [ (h2 [] [ text column.heading ])
    , itemList address column.items
    ]


itemList : Signal.Address Action -> List Item -> Html
itemList address items =
  ul
    [ style [ ( "text-align", "left" ) ] ]
    (List.map (\i -> itemListEntry address i) items)


itemListEntry : Signal.Address Action -> Item -> Html
itemListEntry address item =
  let
    styles =
      if item.crossedOff then
        [ ( "text-decoration", "line-through" ) ]
      else
        []
  in
    li
      [ Events.onClick address (CrossOff item.id)
      , style styles
      ]
      [ text item.text ]


itemInput : Signal.Address Action -> String -> Html
itemInput address itemText =
  div
    [ class "row align-center" ]
    [ itemHeadingSelector address
    , itemTextField address itemText
    , itemAddButton address
    ]


itemHeadingSelector : Signal.Address Action -> Html
itemHeadingSelector address =
  select
    [ Events.on
        "input"
        Events.targetValue
        (Signal.message address << UpdateNewItemHeading)
    , class "columns"
    , class "small-1"
    ]
    [ option [] [ text happy ]
    , option [] [ text meh ]
    , option [] [ text sad ]
    ]


itemTextField : Signal.Address Action -> String -> Html
itemTextField address itemText =
  div
    [ class "columns"
    , class "small-4"
    ]
    [ input
        [ placeholder "wagwan?"
        , type' "text"
        , value itemText
        , onEnter address AddItem
        , Events.on
            "input"
            Events.targetValue
            (Signal.message address << UpdateNewItem)
        ]
        []
    ]


isEnter : Int -> Result String ()
isEnter code =
  if code == 13 then
    Ok ()
  else
    Err "key code is not enter"


onEnter : Signal.Address a -> a -> Attribute
onEnter address value =
  Events.on
    "keydown"
    (Json.customDecoder Events.keyCode isEnter)
    (\_ -> Signal.message address value)


itemAddButton : Signal.Address Action -> Html
itemAddButton address =
  div
    [ class "columns"
    , class "small-1"
    ]
    [ button
        [ Events.onClick address AddItem
        , class "button"
        ]
        [ text "Add" ]
    ]
