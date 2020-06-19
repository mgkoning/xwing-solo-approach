module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Maybe
import Html.Attributes exposing (checked, type_, name)
import Html.Events exposing (onCheck)

main : Program () Model Msg
main = Browser.element
  { init = init
  , subscriptions = always Sub.none
  , view = view
  , update = update
  }

type alias Model = 
  { arc: Maybe TallyArc
  , defense: Maybe DefenseDie
  , attack: Maybe AttackDie
  , attitude: Maybe String
  , approach: Maybe String
  }

type TallyArc = Bullseye | Front | Side | Rear
showArc a = case a of Bullseye -> "Bullseye"
                      Front -> "Front"
                      Side -> "Side"
                      Rear -> "Rear"
type DefenseDie = Evade | DFocus | DBlank
showDefense d = case d of Evade  -> "Evade"
                          DFocus   -> "Focus"
                          DBlank -> "Blank"
type AttackDie = Hit | Crit | AFocus | ABlank
showAttack a = case a of Hit -> "Hit"
                         Crit -> "Crit"
                         AFocus -> "Focus"
                         ABlank -> "Blank"
type Msg = Arc TallyArc | Defense DefenseDie | Attack AttackDie | Reset

init : () -> (Model, Cmd Msg)
init _ = (emptyModel, Cmd.none)

emptyModel : Model
emptyModel = Model Nothing Nothing Nothing Nothing Nothing

view : Model -> Html Msg
view m = div [] 
  [ text "Approach Helper for x-wing solo rules"
  , br [] []
  , div []
    (text "Tally arc:" :: List.map (\a -> label [] [input [type_ "radio", name "tally-arc", checked (maybeEquals a m.arc), onCheck (always (Arc a))] [], text (showArc a)]) [Bullseye, Front, Side, Rear])
  , div []
    (text "Defense die:" :: List.map (\d -> label [] [input [type_ "radio", name "defense-die", checked (maybeEquals d m.defense), onCheck (always (Defense d))] [], text (showDefense d)]) [Evade, DFocus, DBlank])
  , div []
    (text "Attack die:" :: List.map (\a -> label [] [input [type_ "radio", name "attack-die", checked (maybeEquals a m.attack), onCheck (always (Attack a))] [], text (showAttack a)]) [Hit, Crit, AFocus, ABlank])
  , Maybe.withDefault (div [] []) (Maybe.map (\a -> div [] [text "Approach: The solo ship selects\u{00A0}", text a]) m.approach)
  , Maybe.withDefault (div [] []) (Maybe.map (\a -> div [] [text "Attitude:\u{00A0}", text a]) m.attitude)
  , button [onClick Reset] [text "Reset"]
  ]

maybeEquals : t -> Maybe t -> Bool
maybeEquals desired actual = Maybe.withDefault False (Maybe.map ((==)desired) actual)

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  let newModel = case message of
                   Reset -> emptyModel
                   Arc a -> { model | arc = Just a }
                   Defense d -> { model | defense = Just d }
                   Attack a -> { model | attack = Just a }
  in (result newModel, Cmd.none)

result : Model -> Model
result m =
  let toAttitude d = case d of Evade  -> "Defensive"
                               DFocus -> "Balanced"
                               DBlank -> "Offensive"
      
  in { m | attitude = Maybe.map toAttitude m.defense
         , approach = Maybe.map3 fromApproachChart m.arc m.defense m.attack }

fromApproachChart : TallyArc -> DefenseDie -> AttackDie -> String
fromApproachChart arc defense attack = 
  case (arc, defense, attack) of
    (Bullseye, Evade,  Crit  ) -> "its slowest blue bank maneuver away from the nearest obstacle."
    (Bullseye, Evade,  Hit   ) -> "its slowest straight, stationary, or reverse maneuver."
    (Bullseye, Evade,  AFocus  ) -> "its slowest blue bank maneuver away from the nearest obstacle."
    (Bullseye, Evade,  ABlank) -> "its fastest turn maneuver towards the nearest obstacle."
    (Bullseye, DFocus, Crit  ) -> "its fastest blue straight maneuver."
    (Bullseye, DFocus, Hit   ) -> "its average blue straight maneuver."
    (Bullseye, DFocus, AFocus  ) -> "its slowest blue straight maneuver."
    (Bullseye, DFocus, ABlank) -> "its slowest blue straight maneuver."
    (Bullseye, DBlank, Crit  ) -> "its fastest advanced maneuver."
    (Bullseye, DBlank, Hit   ) -> "its fastest straight maneuver."
    (Bullseye, DBlank, AFocus  ) -> "its average straight maneuver."
    (Bullseye, DBlank, ABlank) -> "its 2-speed straight maneuver."
    (Front,    Evade,  Crit  ) -> "its slowest blue bank maneuver away from the nearest obstacle."
    (Front,    Evade,  Hit   ) -> "its slowest straight, stationary, or reverse maneuver."
    (Front,    Evade,  AFocus  ) -> "its slowest bank maneuver away from the nearest obstacle."
    (Front,    Evade,  ABlank) -> "its fastest bank maneuver towards the nearest obstacle."
    (Front,    DFocus, Crit  ) -> "its fastest blue bank or turn maneuver towards its Tally."
    (Front,    DFocus, Hit   ) -> "its slowest blue bank or turn maneuver towards its Tally."
    (Front,    DFocus, AFocus  ) -> "its average blue straight maneuver."
    (Front,    DFocus, ABlank) -> "its slowest blue straight maneuver."
    (Front,    DBlank, Crit  ) -> "its fastest advanced maneuver away from its Tally."
    (Front,    DBlank, Hit   ) -> "its fastest turn maneuver towards its Tally."
    (Front,    DBlank, AFocus  ) -> "its slowest turn maneuver towards its Tally."
    (Front,    DBlank, ABlank) -> "its 2-speed straight maneuver."
    (Side,     Evade,  Crit  ) -> "its fastest advanced maneuver away from the nearest obstacle."
    (Side,     Evade,  Hit   ) -> "its slowest turn maneuver towards its Tally."
    (Side,     Evade,  AFocus  ) -> "its slowest turn maneuver towards its Tally."
    (Side,     Evade,  ABlank) -> "its fastest bank maneuver towards its Tally."
    (Side,     DFocus, Crit  ) -> "its fastest blue bank or turn maneuver towards its Tally."
    (Side,     DFocus, Hit   ) -> "its average blue bank or turn maneuver towards its Tally."
    (Side,     DFocus, AFocus  ) -> "its slowest blue bank or turn maneuver towards its Tally."
    (Side,     DFocus, ABlank) -> "its slowest blue bank or turn maneuver towards its Tally."
    (Side,     DBlank, Crit  ) -> "its fastest advanced maneuver towards its Tally."
    (Side,     DBlank, Hit   ) -> "its slowest advanced maneuver away from its Tally."
    (Side,     DBlank, AFocus  ) -> "its fastest turn maneuver towards its Tally."
    (Side,     DBlank, ABlank) -> "its 2-speed straight maneuver."
    (Rear,     Evade,  Crit  ) -> "its slowest turn maneuver towards the nearest obstacle."
    (Rear,     Evade,  Hit   ) -> "its fastest turn maneuver towards the nearest obstacle."
    (Rear,     Evade,  AFocus  ) -> "its fastest turn maneuver away from the nearest obstacle."
    (Rear,     Evade,  ABlank) -> "its fastest straight maneuver."
    (Rear,     DFocus, Crit  ) -> "its slowest blue bank or turn maneuver away from its Tally."
    (Rear,     DFocus, Hit   ) -> "its slowest blue bank or turn maneuver towards its Tally."
    (Rear,     DFocus, AFocus  ) -> "its fastest blue bank or turn maneuver away from its Tally."
    (Rear,     DFocus, ABlank) -> "its fastest blue bank or turn maneuver towards its Tally."
    (Rear,     DBlank, Crit  ) -> "its slowest advanced maneuver towards its Tally."
    (Rear,     DBlank, Hit   ) -> "its fastest advanced maneuver towards its Tally."
    (Rear,     DBlank, AFocus  ) -> "its fastest advanced maneuver towards its Tally."
    (Rear,     DBlank, ABlank) -> "its 2-speed straight maneuver."

