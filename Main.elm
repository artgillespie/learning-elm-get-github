import Http
import Html exposing (..)
import Html.Attributes exposing (type', placeholder, src, class)
import Html.Events exposing (onClick, on, targetValue, keyCode, onKeyDown)
import StartApp
import Effects exposing (Effects)
import Json.Decode as Json
import Json.Decode exposing((:=))
import Task

---------- MODEL

type alias Model =
  {
    name: String,
    avatarUrl: String,
    repos: List GHRepo,
    query: String
  }

---------- UPDATE

type Action =
  NoOp
  | GetUser
  | EnterText String
  | ReceiveUser (Maybe GHUser)
  | ReceiveRepos (Maybe (List GHRepo))

init : (Model, Effects Action)
init =
  ( Model "" "" [] ""
  , Effects.none )

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model, Effects.none)
    GetUser ->
      (model, getUser model.query)
    EnterText newText ->
      ({ model | query = newText }, Effects.none)
    ReceiveUser user ->
      let
          u = Maybe.withDefault (GHUser "Not Found" "Not Found") user
      in
        ({ model |
          name = u.login,
          avatarUrl = u.avatarUrl }, getRepos u.login)
    ReceiveRepos repos ->
      let
          r = Maybe.withDefault [] repos
      in
        ( { model | repos = r }, Effects.none )

---------- VIEWS

view : Signal.Address Action -> Model -> Html.Html
view address model =
  div [] [ h1[] [text "Hello, Elm!"]
  , input [ placeholder "GitHub Username"
          , on "input" targetValue (Signal.message address << EnterText)
          , class "input-field"
          -- want the user to be able to hit 'return' to GetUser
          , onKeyDown address (\code -> if code == 13 then GetUser else NoOp) 
          ] []
  , button [ onClick address GetUser ] [ text "Do It!" ]
  , userView model
  , reposView model.repos
  ]

userView : Model -> Html.Html
userView model = 
  if model.name /= "" then
    div [] [ div [] [text model.name]
           , img [src model.avatarUrl] []
           ]
   else
    div [] []

reposView : List GHRepo -> Html.Html
reposView repos =
  if List.length repos > 0 then
    div [] [ h3 [] [text ("Number of Repos: " ++ (toString (List.length repos)))]
           , table [] (List.map repoView (List.sortBy .language repos))
           ]
  else
    div [] []

repoView : GHRepo -> Html.Html
repoView repo =
  tr [] [ td [] [text repo.name]
        , td [] [text repo.language]
        ]

---------- EFFECTS

getUser : String -> Effects Action
getUser username =
  Http.get userDecoder ("https://api.github.com/users/" ++ username)
  |> Task.toMaybe
  |> Task.map ReceiveUser
  |> Effects.task

type alias GHUser = { login: String, avatarUrl: String}

userDecoder : Json.Decoder GHUser
userDecoder =
  Json.object2 GHUser
    ("login" := Json.string)
    ("avatar_url" := Json.string)

getRepos : String -> Effects Action
getRepos username =
  Http.get reposDecoder ("https://api.github.com/users/" ++ username ++ "/repos")
  |> Task.toMaybe
  |> Task.map ReceiveRepos
  |> Effects.task

-- this is similar to Go where you define the type of the subset of the json
-- you care about and de-serialize into that

type alias GHRepo = { name: String, language: String }

repoDecoder : Json.Decoder GHRepo
repoDecoder =
  Json.object2 GHRepo
    ("name" := nullDecoder)
    ("language" := nullDecoder)

-- Interesting: Composable Json decoders.
reposDecoder : Json.Decoder (List GHRepo)
reposDecoder =
  Json.list repoDecoder

nullDecoder : Json.Decoder String
nullDecoder =
-- neat, JSON null's cause the whole shebang to fail, but you can force them
-- into strings
  Json.oneOf
  [ Json.null "None"
  , Json.string
  ]

app = StartApp.start { init = init, update = update, view = view, inputs = [] }

main =
  app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
