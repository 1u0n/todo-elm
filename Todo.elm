port module Todo exposing (..)

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Json
import String
import Task
import Material
import Material.Layout as Layout
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.List as MaterialList
import Material.Footer as Footer
import Material.Toggles as Toggles


main : Program (Maybe PortableModel) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : PortableModel -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage (getPortableModel newModel), cmds ]
        )



-- MODEL


-- The full application state of our todo app.
type alias Model =
    { entries : List Entry
    , field : String
    , categorySelect : String
    , uid : Int
    , visibility : String
    , mdl : Material.Model
    }


type alias Entry =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    , category : String
    }


-- the part of the model we'll pass to localStorage
type alias PortableModel =
    { entries : List Entry
    , field : String
    , categorySelect : String
    , uid : Int
    , visibility : String
    }

getPortableModel: Model -> PortableModel
getPortableModel model =
    { entries = model.entries
    , field = model.field
    , categorySelect = model.categorySelect
    , uid = model.uid
    , visibility = model.visibility
    }

makeModelFromPortable: PortableModel -> Model
makeModelFromPortable portable =
    { entries = portable.entries
    , field = portable.field
    , categorySelect = portable.categorySelect
    , uid = portable.uid
    , visibility = portable.visibility
    , mdl = Material.model
    }



emptyModel : Model
emptyModel =
    { entries = []
    , visibility = "All"
    , field = ""
    , categorySelect = "Work"
    , uid = 0
    , mdl = Material.model
    }

emptyPortableModel : PortableModel
emptyPortableModel =
    { entries = []
    , visibility = "All"
    , field = ""
    , categorySelect = "Work"
    , uid = 0
    }


newEntry : String -> String -> Int -> Entry
newEntry desc cat id =
    { description = desc
    , category = cat
    , completed = False
    , editing = False
    , id = id
    }


init : Maybe PortableModel -> ( Model, Cmd Msg )
init savedModel =
    makeModelFromPortable (Maybe.withDefault emptyPortableModel savedModel) ! []



-- UPDATE


type Msg
    = NoOp
    | UpdateField String
    | UpdateCategory String
    | EditingEntry Int Bool
    | UpdateEntryDescription Int String
    | UpdateEntryCategory Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String
    | Mdl (Material.Msg Msg)



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Add ->
            { model
                | uid = model.uid + 1
                , field = ""
                , categorySelect = "Work"
                , entries =
                    if String.isEmpty model.field then
                        model.entries
                    else
                        model.entries ++ [ newEntry model.field model.categorySelect model.uid ]
            }
                ! []

        UpdateField str ->
            { model | field = str }
                ! []

        UpdateCategory str ->
            { model | categorySelect = str }
                ! []

        EditingEntry id isEditing ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | editing = isEditing }
                    else
                        t

                focus =
                    Dom.focus ("todo-" ++ toString id)
            in
                { model | entries = List.map updateEntry model.entries }
                    ! [ Task.attempt (\_ -> NoOp) focus ]

        UpdateEntryDescription id task ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | description = task }
                    else
                        t
            in
                { model | entries = List.map updateEntry model.entries }
                    ! []

        UpdateEntryCategory id cat ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | category = cat }
                    else
                        t
            in
                { model | entries = List.map updateEntry model.entries }
                    ! []

        Delete id ->
            { model | entries = List.filter (\t -> t.id /= id) model.entries }
                ! []

        DeleteComplete ->
            { model | entries = List.filter (not << .completed) model.entries }
                ! []

        Check id isCompleted ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | completed = isCompleted }
                    else
                        t
            in
                { model | entries = List.map updateEntry model.entries }
                    ! []

        CheckAll isCompleted ->
            let
                updateEntry t =
                    { t | completed = isCompleted }
            in
                { model | entries = List.map updateEntry model.entries }
                    ! []

        ChangeVisibility visibility ->
            { model | visibility = visibility }
                ! []


        Mdl msg_ ->
            Material.update Mdl msg_ model


-- VIEW


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    Layout.render Mdl  model.mdl
        [ Layout.fixedHeader
        ]
        { header = [  h1 [] [ text "todos" ] ]
        , drawer = []
        , tabs = ( [], [] )
        , main = [
            div
                [ class "todomvc-wrapper"
                , style [ ( "visibility", "hidden" ) ]
                ]
                [ section
                    [ class "todoapp" ]
                    [ lazy2 viewInput model.field model.categorySelect
                    , lazy3 viewEntries model.visibility model.entries model.mdl
                    , lazy viewControls model
                    ]
                ]
            , infoFooter
            ]
        }



viewInput : String -> String -> Html Msg
viewInput task category =
    header
        [ class "header" ]
        [ input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , autofocus True
            , value task
            , name "newTodo"
            , onInput UpdateField
            , onEnter Add
            ]
            []
        , select
            [class "new-todo-type"
            , value category
            , name "newTodoType"
            , multiple False
            , size 1
            , onInput UpdateCategory
            , onEnter Add
            ]
            [option
                [value "Work"
                , selected True]
                [text "Work"],
            option
                [value "Studies"]
                [text "Studies"],
            option
                [value "Shopping"]
                [text "Shopping"]
            ]
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)


onEnterOrEscape : Msg -> Attribute Msg
onEnterOrEscape msg =
    let
        isEnterOrEscape code =
            if code == 27 || code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER nor ESCAPE"
    in
        on "keydown" (Json.andThen isEnterOrEscape keyCode)


-- the 'id' field might or not exist, and if exists might or not be null, so we use oneOf with Json.succeed as backup
relatedTargetDecoder : Json.Decoder String
relatedTargetDecoder =
     Json.field "relatedTarget" (Json.oneOf [ Json.field "id" Json.string, Json.succeed "1" ])
    --explicitOriginalTarget
     
-- only sends update message if blur happened due to click on something that ISN'T what the user is editing
onBlurSmart : Msg -> Attribute Msg
onBlurSmart msg =
    let
        isEditingElement id =
            if String.startsWith "todo-" (Debug.log "id" id) then
                Json.fail "fail"
            else
                Json.succeed msg
    in
        on "blur" (Json.andThen isEditingElement relatedTargetDecoder)


-- VIEW ALL ENTRIES


viewEntries : String -> List Entry -> Mdl -> Html Msg
viewEntries visibility entries mdl =
    let
        isVisible todo =
            case visibility of
                "Completed" ->
                    todo.completed

                "Active" ->
                    not todo.completed

                _ ->
                    True

        allCompleted =
            List.all .completed entries

        cssVisibility =
            if List.isEmpty entries then
                "hidden"
            else
                "visible"
    in
        section
            [ class "main"
            , style [ ( "visibility", cssVisibility ) ]
            ]
            [ input
                [ class "toggle-all"
                , type_ "checkbox"
                , name "toggle"
                , checked allCompleted
                , onClick (CheckAll (not allCompleted))
                ]
                []
            , label
                [ for "toggle-all" ]
                [ text "Mark all as complete" ]
            , MaterialList.ul [ Options.cs "todo-list" ] <|
                List.indexedMap (viewEntry mdl) (List.filter isVisible entries)
            ]



-- VIEW INDIVIDUAL ENTRIES


viewEntry : Mdl -> Int -> Entry -> Html Msg
viewEntry mdl index todo =
    MaterialList.li
        [ if todo.completed == True then Options.cs "completed" else Options.cs ""
        , if todo.editing == True then Options.cs "editing" else Options.cs ""
        , Options.onDoubleClick (EditingEntry todo.id True)
        ]
        [ MaterialList.content
            []
            [ div
            [ class "view" ]
            [ Toggles.checkbox Mdl [index + 4 ] mdl
                [ Options.cs "toggle" 
                , Options.onToggle (Check todo.id (not todo.completed))
                , Toggles.ripple
                , Toggles.value todo.completed
                ]
                []
            , label
                [ class "todo-description"
                , onDoubleClick (EditingEntry todo.id True) ]
                [ text todo.description ]
            , MaterialList.icon
                (if todo.category == "Work" then "business_center"
                else if todo.category == "Studies" then "import_contacts"
                else "local_grocery_store")
                [ Options.onDoubleClick (EditingEntry todo.id True)
                , Options.cs "category" ]
            , button
                [ class "destroy"
                , onClick (Delete todo.id)
                ]
                []
            ]
        , div
            [ class "edit" ]
            [input
                [ value todo.description
                , name "title"
                , id ("todo-" ++ toString todo.id)
                , onInput (UpdateEntryDescription todo.id)
                , onEnterOrEscape (EditingEntry todo.id False)
                , onBlurSmart (EditingEntry todo.id False)
                ]
                []
            , select
                [ class "new-todo-type"
                , value todo.category
                , name "newTodoType"
                , id ("todo-select" ++ toString todo.id)
                , multiple False
                , size 1
                , onInput (UpdateEntryCategory todo.id)
                , onEnterOrEscape (EditingEntry todo.id False)
                , onBlurSmart (EditingEntry todo.id False)
                ]
                [option
                    [ value "Work"
                    , selected True]
                    [text "Work"],
                option
                    [ value "Studies"]
                    [ text "Studies"],
                option
                    [ value "Shopping"]
                    [ text "Shopping"]
                ]
            ]
        ]
        ]


-- VIEW CONTROLS AND FOOTER


viewControls : Model -> Html Msg
viewControls model =
    let
        entriesCompleted =
            List.length (List.filter .completed model.entries)

        entriesLeft =
            List.length model.entries - entriesCompleted
    in
        footer
            [ class "footer"
            , hidden (List.isEmpty model.entries)
            ]
            [ lazy viewControlsCount entriesLeft
            , lazy viewControlsFilters model
            , lazy viewControlsClear model
            ]


viewControlsCount : Int -> Html Msg
viewControlsCount entriesLeft =
    let
        item_ =
            if entriesLeft == 1 then
                " item"
            else
                " items"
    in
        span
            [ class "todo-count" ]
            [ strong [] [ text (toString entriesLeft) ]
            , text (item_ ++ " left")
            ]


viewControlsFilters : Model -> Html Msg
viewControlsFilters model =
    ul
        [ class "filters" ]
        [ visibilitySwap 0 "All" model
        , text " "
        , visibilitySwap 1 "Active" model
        , text " "
        , visibilitySwap 2 "Completed" model
        ]


visibilitySwap : Int -> String -> Model -> Html Msg
visibilitySwap index visibility model =
    li
        [ onClick (ChangeVisibility visibility) ]
        [ Button.render Mdl [ index ] model.mdl
            [ Button.raised
            , Button.ripple
            , if visibility == model.visibility then Button.disabled else Button.colored
            , Options.onClick (ChangeVisibility visibility) ]
            [ text visibility ]
        ]


viewControlsClear : Model -> Html Msg
viewControlsClear model =
    let
        entriesCompleted =
            List.length (List.filter .completed model.entries)
    in
    Button.render Mdl [ 3 ] model.mdl
        [ if entriesCompleted == 0 then Button.disabled else Button.colored
        , Button.ripple
        , Options.cs "clear-completed"
        , Options.onClick DeleteComplete ]
        [ text ("Clear completed (" ++ toString entriesCompleted ++ ")") ]


infoFooter : Html msg
infoFooter =
    Footer.mini
        [ Options.cs "page-footer"]
        { left =
            Footer.left []
            [ Footer.logo
                []
                [ Footer.html <| text "Double-click to edit a todo" ]
            ]
        , right = Footer.right [] []
        }