import Window
import Mouse
import Signal exposing(..)
import Time exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(..)


-- MODEL
spriteWidth = 12800
spriteXFrame = 256
spriteXStart = spriteXFrame - spriteWidth

type alias State = {
    player: Player
}

type alias Player = {
    score: Int,
    x: Int,
    y: Int,
    spriteX: Int
}

initialState: State
initialState = {
        player = {
            score = 0,
            x = -1,
            y = -1,
            spriteX = spriteXStart
        }
    }

model: Signal State
model =
    Signal.foldp update initialState
        (Signal.merge envAction gameAction.signal)


-- UPDATE
type Action =
    MouseMove (Int, Int) |
    MouseButton Bool |
    Frame Float |
    PowerUp |
    Noop

envAction: Signal Action
envAction =
    Signal.mergeMany [
        Signal.map MouseMove Mouse.position,
        Signal.map MouseButton Mouse.isDown,
        Signal.map Frame (fps 25)
    ]

gameAction: Mailbox Action
gameAction =
    Signal.mailbox Noop

update: Action -> State -> State
update action state =
    case action of
        MouseMove (x, y) ->
            { state | player = playerPos (x, y) state.player }
        MouseButton isDown ->
            { state | player = playerScore 1 state.player }
        Frame delta ->
            { state | player = timeline delta state.player }
        PowerUp ->
            { state | player = playerSpin True state.player }
        Noop ->
            state

playerScore: Int -> Player -> Player
playerScore n player =
    { player | score = player.score + n }

playerPos: (Int, Int) -> Player -> Player
playerPos (x, y) player =
    { player | x = x, y = y }

playerSpin: Bool -> Player -> Player
playerSpin isDown player =
    { player | spriteX =
        if player.spriteX == spriteXStart && isDown == True
            then spriteXStart + spriteXFrame
        else player.spriteX
    }

timeline: Float -> Player -> Player
timeline delta player =
    { player |
        spriteX =
        if player.spriteX == 0
            then spriteXStart
        else if player.spriteX > spriteXStart
            then player.spriteX + spriteXFrame
        else player.spriteX
    }

main =
    Signal.map2
        (view gameAction.address)
        Window.dimensions model


-- VIEW
view: Address Action -> (Int, Int) -> State -> Html
view gameAction (winW, winH) state =
  let player = state.player
  in
    div [
        style [
            ("width", (toString winW) ++ "px"),
            ("height", (toString winH) ++ "px"),
            ("background", "#111"),
            ("overflow", "hidden")
        ]
    ] [
        div [
            style [
                ("position", "absolute"),
                ("margin-left", "-128px"),
                ("margin-top", "-128px"),
                (if player.x > -1 then ("left", toString player.x ++ "px") else ("left", "50%")),
                (if player.y > -1 then ("top", toString player.y ++ "px") else ("top", "80%")),
                ("background", "url(player.png) no-repeat " ++ (toString player.spriteX) ++ "px 0px"),
                ("width", "256px"),
                ("height", "256px"),
                ("overflow", "hidden"),
                ("z-index", "1"),
                ("cursor", "none")
            ]
        ] [
        ],
        div [
            style [
                ("position", "absolute"),
                ("left", "150px"),
                ("top", "80%"),
                ("margin-left", "-30px"),
                ("margin-top", "-30px"),
                ("width", "60px"),
                ("height", "60px"),
                ("line-height", "80px"),
                ("text-align", "center"),
                ("border", "10px solid #fff"),
                ("border-radius", "100px"),
                ("z-index", "10"),
                ("cursor", "move")
            ],
            onClick gameAction PowerUp
        ] [
        ],
        span [
            style [
                ("color", "#fff")
            ]
        ] [
            text (toString state)
        ]
    ]
