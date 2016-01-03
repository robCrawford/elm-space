import Mouse
import Window
import Keyboard
import Time exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


-- MODEL
spriteXStart = -12544
spriteXFrame = 256

type alias State = {
    player: Player
  }

type alias Player = {
    visible: Bool,
    x: Int,
    y: Int,
    spriteX: Int
  }

initialState: State
initialState = {
    player = {
      visible = False,
      x = 50,
      y = 50,
      spriteX = spriteXStart
    }
  }

model: Signal State
model =
  Signal.foldp update initialState uiEvents


-- UPDATE
type UIEvents = MouseMove (Int, Int)
  | MouseButton Bool
  | Frame Float

uiEvents: Signal UIEvents
uiEvents = Signal.mergeMany [
    Signal.map MouseMove Mouse.position,
    Signal.map MouseButton Mouse.isDown,
    Signal.map Frame (fps 25)
  ]

update: UIEvents -> State -> State
update uiEvents state =
  case uiEvents of
    MouseMove (x, y) ->
      { state | player = playerPos (x, y) state.player }
    MouseButton isDown ->
      { state | player = playerSpin isDown state.player }
    Frame delta ->
      { state | player = timeline delta state.player }

playerPos: (Int, Int) -> Player -> Player
playerPos (x, y) player = 
  { player | x = x, y = y, visible = True }

playerSpin: Bool -> Player -> Player
playerSpin isDown player =
    { player |
        spriteX = 
          if player.spriteX == spriteXStart && isDown == True then 
            spriteXStart + spriteXFrame
          else player.spriteX
    }

timeline: Float -> Player -> Player
timeline delta player =
    { player |
        spriteX = 
          if player.spriteX == 0 then 
            spriteXStart
          else if player.spriteX > spriteXStart then 
            player.spriteX + spriteXFrame
          else player.spriteX
    }

  
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
      div [
        style
          [ ("position", "absolute")
          , ("margin-left", "-128px")
          , ("margin-top", "-128px")
          , ("left", toString player.x ++ "px")
          , ("top", toString player.y ++ "px")
          , ("background", "url(player.png) no-repeat " ++ (toString player.spriteX) ++ "px 0px")
          , ("width", "256px")
          , ("height", "256px")
          , ("overflow", "hidden")
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
