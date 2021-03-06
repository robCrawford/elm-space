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
    player: Player,
    orb: Orb
}

type alias Player = {
    score: Int,
    x: Int,
    y: Int,
    spriteX: Int,
    isSpinning: Bool,
    highlightFrames: Int
}

type alias Orb = {
    x: Int,
    y: Int
}

initialState: State
initialState = {
        player = {
            score = 0,
            x = -100,
            y = -100,
            spriteX = spriteXStart,
            isSpinning = False,
            highlightFrames = 0
        },
        orb = {
            x = 200,
            y = 200
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
    OrbHit |
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
            { state | player = playerMD state.player }
        Frame delta ->
            { state | player =
                playerTimeline state.player,
                orb = orbTimeline state.orb state.player
            }
        OrbHit ->
            { state | player =
                if state.player.isSpinning == False
                    then playerSpin state.player
                else state.player
            }
        Noop ->
            state

playerMD: Player -> Player
playerMD player =
    { player | highlightFrames = 5 }

playerPos: (Int, Int) -> Player -> Player
playerPos (x, y) player =
    { player |
        x = x,
        y = y
    }

playerSpin: Player -> Player
playerSpin player =
    { player |
        spriteX = spriteXStart + spriteXFrame,
        isSpinning = True,
        score = player.score + 10
    }

playerTimeline: Player -> Player
playerTimeline player =
    { player |
        spriteX =
            if player.spriteX == 0
                then spriteXStart
            else if player.spriteX > spriteXStart
                then player.spriteX + spriteXFrame
            else player.spriteX,

        highlightFrames =
            if player.highlightFrames > 0
                then player.highlightFrames-1
            else 0,

        isSpinning =
            if player.spriteX /= spriteXStart
                then True
            else False
    }

orbTimeline: Orb -> Player -> Orb
orbTimeline orb player =
    let incr = player.score // 10
    in { orb |
            x = orb.x + incr,
            y = orb.y + incr
        }

main =
    Signal.map2 (view gameAction.address)
        Window.dimensions model


-- VIEW
css : String -> Html
css path =
    node "link" [
        rel "stylesheet",
        href path
    ] []

view: Address Action -> (Int, Int) -> State -> Html
view gameAction (winW, winH) state =
    let
        player = state.player
        orb = state.orb
        level = player.score // 100

    in div [
            class "main",
            style [
                ("width", (toString (winW-2)) ++ "px"),
                ("height", (toString (winH-2)) ++ "px")
            ]
        ] [
            -- STYLESHEET
            css "style.css",
            -- PLAYER
            div [
                class "player",
                style [
                    ("background-position", (toString player.spriteX) ++ "px 0px"),
                    if player.x > -1
                        then ("left", toString player.x ++ "px")
                    else ("left", "50%"),
                    if player.y > -1
                        then ("top", toString player.y ++ "px")
                    else ("top", "80%"),
                    if player.highlightFrames > 0
                        then ("box-shadow", "inset 0px 0px 60px 10px rgba(255, 255, 255, 0.8)")
                    else ("box-shadow", "none")
                ]
            ] [],
            -- ORB
            div [
                class "orb",
                style [
                    ("left", toString (orb.x % winW) ++ "px"),
                    ("top", toString (orb.y % winH) ++ "px"),
                    if (level > 0)
                        then ("width", "40px")
                    else ("width", "60px"),
                    if (level > 0)
                        then ("height", "40px")
                    else ("height", "60px"),
                    if (level > 0)
                        then ("background", "#fff")
                    else ("background", "none")
                ],
                onClick gameAction OrbHit
            ] [],
            -- SCORE
            div [
                class "score"
            ] [
                text ( "SCORE: " ++ (toString state.player.score))
            ],
            -- DEBUG
            span [
                class "debug"
            ] [
                text (toString state)
            ]
        ]
