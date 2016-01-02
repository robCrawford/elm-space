import Mouse
import Window
import Keyboard
import Time exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


-- MODEL
type alias State = {
    player: Player
  }

type alias Player = {
    x: Int,
    y: Int,
    fire: Bool,
    visible: Bool
  }

initialState: State
initialState = {
    player = {
      x = 50,
      y = 50,
      fire = False,
      visible = False
    }
  }

model: Signal State
model =
  Signal.foldp update initialState uiEvents


-- UPDATE
type UIEvents = MouseMove (Int, Int)
  | MouseButton Bool

uiEvents: Signal UIEvents
uiEvents = Signal.merge
    (Signal.map MouseMove Mouse.position)
    (Signal.map MouseButton Mouse.isDown)

update: UIEvents -> State -> State
update uiEvents state =
  case uiEvents of
    MouseMove (x, y) ->
      { state | player = playerPos (x, y) state.player }
    MouseButton isDown ->
      { state | player = playerFire isDown state.player }

playerPos: (Int, Int) -> Player -> Player
playerPos (x, y) player = 
  { player | x = x, y = y, visible = True }

playerFire: Bool -> Player -> Player
playerFire isDown player = 
  { player | fire = isDown }

  
-- VIEW
view: (Int, Int) -> State -> Html 
view (winW, winH) state =
  let player = state.player
  in
    div [
      style
        [ ("width", (toString winW) ++ "px")
        , ("height", (toString winH) ++ "px")
        , ("background", "#111")
        ]
    ] [ 
      img [
         src "x.png"
        -- if player.fire then (src "x.gif") else (src "x.png")
      , style
          [ ("position", "absolute")
          , ("margin-left", "-128px")
          , ("margin-top", "-128px")
          , ("left", toString player.x ++ "px")
          , ("top", toString player.y ++ "px")
          , ("cursor", "none")
          , (if player.visible then ("display", "block") else ("display", "none"))
          ]
      ] [
      ]
      , span [
          style
            [ ("color", "#fff") ]
      ] [ 
        text (
          toString state
        )
      ]
    ]

main = Signal.map2 view Window.dimensions model
