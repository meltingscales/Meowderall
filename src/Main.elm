port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Json.Decode as Decode
import Task


-- PORTS


port saveAdminSession : Bool -> Cmd msg


port loadAdminSession : (Bool -> msg) -> Sub msg


main : Program Bool Model Msg
main =
    Browser.element
        { init = initWithFlags
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


initWithFlags : Bool -> ( Model, Cmd Msg )
initWithFlags isAdmin =
    ( { init | isAdmin = isAdmin }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    loadAdminSession LoadAdminSession



-- MODEL


type alias DailyRecord =
    { date : String
    , amInitials : String
    , pmInitials : String
    , comments : String
    }


type alias MedicationForm =
    { catName : String
    , diagnosis : String
    , treatment : String
    , careContact : String
    , vetInfo : String
    , startDate : String
    , endDate : String
    , dailyRecords : List DailyRecord
    }


type alias EditingCell =
    { date : String
    , field : CellField
    }


type CellField
    = AMInitials
    | PMInitials
    | Comments


type alias Model =
    { form : MedicationForm
    , editing : Maybe EditingCell
    , editValue : String
    , isAdmin : Bool
    , showPinModal : Bool
    , pinInput : String
    , pinError : Bool
    , pendingEdit : Maybe EditingCell
    }


adminPin : String
adminPin =
    "1234"


init : Model
init =
    { form = sampleForm
    , editing = Nothing
    , editValue = ""
    , isAdmin = False
    , showPinModal = False
    , pinInput = ""
    , pinError = False
    , pendingEdit = Nothing
    }


sampleForm : MedicationForm
sampleForm =
    { catName = "Saffy P017"
    , diagnosis = "Wound infection - elbow wound"
    , treatment = "Clavamox 1mL 2X/day for 14 days, med in refrigerator, continue e-collar"
    , careContact = "Green"
    , vetInfo = ""
    , startDate = "2026-01-12"
    , endDate = "2026-01-26"
    , dailyRecords =
        [ { date = "2026-01-12", amInitials = "", pmInitials = "TM", comments = "Easy Peasy" }
        , { date = "2026-01-13", amInitials = "JR", pmInitials = "QR", comments = "" }
        , { date = "2026-01-14", amInitials = "KT", pmInitials = "KP", comments = "" }
        , { date = "2026-01-15", amInitials = "Kolu", pmInitials = "NB", comments = "" }
        , { date = "2026-01-16", amInitials = "CL", pmInitials = "MO", comments = "" }
        , { date = "2026-01-17", amInitials = "KT", pmInitials = "", comments = "" }
        , { date = "2026-01-18", amInitials = "", pmInitials = "", comments = "" }
        , { date = "2026-01-19", amInitials = "", pmInitials = "", comments = "" }
        , { date = "2026-01-20", amInitials = "", pmInitials = "", comments = "" }
        , { date = "2026-01-21", amInitials = "", pmInitials = "", comments = "" }
        , { date = "2026-01-22", amInitials = "", pmInitials = "", comments = "" }
        , { date = "2026-01-23", amInitials = "", pmInitials = "", comments = "" }
        , { date = "2026-01-24", amInitials = "", pmInitials = "", comments = "" }
        , { date = "2026-01-25", amInitials = "", pmInitials = "", comments = "" }
        , { date = "2026-01-26", amInitials = "", pmInitials = "", comments = "" }
        ]
    }


isProtectedField : CellField -> Bool
isProtectedField field =
    case field of
        AMInitials ->
            True

        PMInitials ->
            True

        Comments ->
            False



-- UPDATE


type Msg
    = NoOp
    | StartEdit String CellField String
    | UpdateEditValue String
    | SaveEdit
    | CancelEdit
    | UpdateFormField FormField String
    | RequestPinEntry EditingCell String
    | PinDigit String
    | PinBackspace
    | PinClear
    | SubmitPin
    | CancelPin
    | Logout
    | FocusResult (Result Dom.Error ())
    | LoadAdminSession Bool


type FormField
    = CatName
    | Diagnosis
    | Treatment
    | CareContact
    | VetInfo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartEdit date field currentValue ->
            if isProtectedField field && not model.isAdmin then
                ( { model
                    | pendingEdit = Just { date = date, field = field }
                    , editValue = currentValue
                    , showPinModal = True
                    , pinInput = ""
                    , pinError = False
                  }
                , Cmd.none
                )

            else
                ( { model
                    | editing = Just { date = date, field = field }
                    , editValue = currentValue
                  }
                , focusElement "edit-input"
                )

        UpdateEditValue value ->
            ( { model | editValue = value }, Cmd.none )

        SaveEdit ->
            case model.editing of
                Just editingCell ->
                    ( { model
                        | form = updateDailyRecord model.form editingCell model.editValue
                        , editing = Nothing
                        , editValue = ""
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        CancelEdit ->
            ( { model | editing = Nothing, editValue = "" }, Cmd.none )

        UpdateFormField field value ->
            ( { model | form = updateFormField model.form field value }, Cmd.none )

        RequestPinEntry editingCell currentValue ->
            ( { model
                | pendingEdit = Just editingCell
                , editValue = currentValue
                , showPinModal = True
                , pinInput = ""
                , pinError = False
              }
            , Cmd.none
            )

        PinDigit digit ->
            if String.length model.pinInput < 4 then
                let
                    newPin =
                        model.pinInput ++ digit
                in
                if String.length newPin == 4 then
                    -- Auto-submit when 4 digits entered
                    if newPin == adminPin then
                        ( { model
                            | isAdmin = True
                            , showPinModal = False
                            , pinInput = ""
                            , pinError = False
                            , editing = model.pendingEdit
                            , pendingEdit = Nothing
                          }
                        , Cmd.batch [ saveAdminSession True, focusElement "edit-input" ]
                        )

                    else
                        ( { model | pinInput = "", pinError = True }, Cmd.none )

                else
                    ( { model | pinInput = newPin, pinError = False }, Cmd.none )

            else
                ( model, Cmd.none )

        PinBackspace ->
            ( { model | pinInput = String.dropRight 1 model.pinInput, pinError = False }, Cmd.none )

        PinClear ->
            ( { model | pinInput = "", pinError = False }, Cmd.none )

        SubmitPin ->
            if model.pinInput == adminPin then
                ( { model
                    | isAdmin = True
                    , showPinModal = False
                    , pinInput = ""
                    , pinError = False
                    , editing = model.pendingEdit
                    , pendingEdit = Nothing
                  }
                , Cmd.batch [ saveAdminSession True, focusElement "edit-input" ]
                )

            else
                ( { model | pinInput = "", pinError = True }, Cmd.none )

        CancelPin ->
            ( { model
                | showPinModal = False
                , pinInput = ""
                , pinError = False
                , pendingEdit = Nothing
                , editValue = ""
              }
            , Cmd.none
            )

        Logout ->
            ( { model | isAdmin = False }, saveAdminSession False )

        FocusResult _ ->
            ( model, Cmd.none )

        LoadAdminSession isAdmin ->
            ( { model | isAdmin = isAdmin }, Cmd.none )


focusElement : String -> Cmd Msg
focusElement id =
    Task.attempt FocusResult (Dom.focus id)


updateFormField : MedicationForm -> FormField -> String -> MedicationForm
updateFormField form field value =
    case field of
        CatName ->
            { form | catName = value }

        Diagnosis ->
            { form | diagnosis = value }

        Treatment ->
            { form | treatment = value }

        CareContact ->
            { form | careContact = value }

        VetInfo ->
            { form | vetInfo = value }


updateDailyRecord : MedicationForm -> EditingCell -> String -> MedicationForm
updateDailyRecord form editingCell newValue =
    let
        updateRecord record =
            if record.date == editingCell.date then
                case editingCell.field of
                    AMInitials ->
                        { record | amInitials = newValue }

                    PMInitials ->
                        { record | pmInitials = newValue }

                    Comments ->
                        { record | comments = newValue }

            else
                record
    in
    { form | dailyRecords = List.map updateRecord form.dailyRecords }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ viewHeader model.isAdmin
        , viewFormInfo model.form
        , viewScheduleTable model
        , viewEditModal model
        , viewPinModal model
        ]


viewHeader : Bool -> Html Msg
viewHeader isAdmin =
    header [ class "header" ]
        [ div [ class "header-top" ]
            [ h1 [] [ text "Meowderall" ]
            , if isAdmin then
                button [ class "btn btn-small btn-logout", onClick Logout ] [ text "Logout" ]

              else
                text ""
            ]
        , p [ class "tagline" ] [ text "Cat medication tracking for shelters" ]
        , p [ class "notice" ] [ text "Data is stored locally in your browser only." ]
        , if isAdmin then
            p [ class "admin-badge" ] [ text "Admin Mode" ]

          else
            text ""
        ]


viewFormInfo : MedicationForm -> Html Msg
viewFormInfo form =
    div [ class "card form-info" ]
        [ div [ class "form-grid" ]
            [ viewEditableField "Cat Name" form.catName CatName
            , viewEditableField "Diagnosis" form.diagnosis Diagnosis
            , viewEditableField "Treatment" form.treatment Treatment
            , viewEditableField "CARE Contact" form.careContact CareContact
            , viewEditableField "Vet/Hospital" form.vetInfo VetInfo
            , viewDateRange form.startDate form.endDate
            ]
        ]


viewEditableField : String -> String -> FormField -> Html Msg
viewEditableField label value field =
    div [ class "field" ]
        [ Html.label [] [ text label ]
        , input
            [ type_ "text"
            , Html.Attributes.value value
            , onInput (UpdateFormField field)
            , placeholder ("Enter " ++ String.toLower label)
            ]
            []
        ]


viewDateRange : String -> String -> Html Msg
viewDateRange startDate endDate =
    div [ class "field date-range" ]
        [ Html.label [] [ text "Treatment Period" ]
        , div [ class "date-display" ]
            [ span [] [ text (formatDate startDate) ]
            , span [ class "date-separator" ] [ text " → " ]
            , span [] [ text (formatDate endDate) ]
            ]
        ]


formatDate : String -> String
formatDate dateStr =
    case String.split "-" dateStr of
        [ _, month, day ] ->
            month ++ "/" ++ day

        _ ->
            dateStr


viewScheduleTable : Model -> Html Msg
viewScheduleTable model =
    div [ class "card schedule-section" ]
        [ h2 [] [ text "Daily Medication Log" ]
        , div [ class "table-wrapper" ]
            [ table [ class "schedule-table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Date" ]
                        , th [ class "protected-header" ] [ text "A.M." ]
                        , th [ class "protected-header" ] [ text "P.M." ]
                        , th [] [ text "Comments" ]
                        ]
                    ]
                , tbody [] (List.map (viewDailyRow model) model.form.dailyRecords)
                ]
            ]
        ]


viewDailyRow : Model -> DailyRecord -> Html Msg
viewDailyRow model record =
    let
        isEditing field =
            case model.editing of
                Just e ->
                    e.date == record.date && e.field == field

                Nothing ->
                    False
    in
    tr []
        [ td [ class "date-cell" ] [ text (formatDate record.date) ]
        , viewTappableCell record.date AMInitials record.amInitials (isEditing AMInitials) model.isAdmin
        , viewTappableCell record.date PMInitials record.pmInitials (isEditing PMInitials) model.isAdmin
        , viewTappableCell record.date Comments record.comments (isEditing Comments) True
        ]


viewTappableCell : String -> CellField -> String -> Bool -> Bool -> Html Msg
viewTappableCell date field value isEditing hasAccess =
    let
        baseClass =
            if String.isEmpty value then
                "tappable"

            else
                "tappable filled"

        cellClass =
            if isProtectedField field && not hasAccess then
                baseClass ++ " locked"

            else
                baseClass

        displayValue =
            if String.isEmpty value then
                "—"

            else
                value
    in
    td
        [ class cellClass
        , onClick (StartEdit date field value)
        ]
        [ text displayValue
        , if isProtectedField field && not hasAccess then
            span [ class "lock-icon" ] [ text "🔒" ]

          else
            text ""
        ]


viewEditModal : Model -> Html Msg
viewEditModal model =
    case model.editing of
        Just editingCell ->
            let
                fieldLabel =
                    case editingCell.field of
                        AMInitials ->
                            "A.M. Initials"

                        PMInitials ->
                            "P.M. Initials"

                        Comments ->
                            "Comments"
            in
            div [ class "modal-overlay", onClick CancelEdit ]
                [ div
                    [ class "modal"
                    , onClickStopPropagation NoOp
                    ]
                    [ h2 [] [ text fieldLabel ]
                    , p [ class "modal-date" ] [ text (formatDate editingCell.date) ]
                    , input
                        [ type_ "text"
                        , class "modal-input"
                        , Html.Attributes.value model.editValue
                        , onInput UpdateEditValue
                        , placeholder ("Enter " ++ String.toLower fieldLabel)
                        , id "edit-input"
                        ]
                        []
                    , div [ class "modal-buttons" ]
                        [ button [ class "btn btn-secondary", onClick CancelEdit ] [ text "Cancel" ]
                        , button [ class "btn btn-primary", onClick SaveEdit ] [ text "Save" ]
                        ]
                    ]
                ]

        Nothing ->
            text ""


viewPinModal : Model -> Html Msg
viewPinModal model =
    if model.showPinModal then
        div [ class "modal-overlay", onClick CancelPin ]
            [ div
                [ class "modal pin-modal"
                , onClickStopPropagation NoOp
                ]
                [ h2 [] [ text "Enter Admin PIN" ]
                , p [ class "pin-subtitle" ] [ text "4-digit PIN required to edit initials" ]
                , div [ class "pin-display" ]
                    [ span [ class "pin-dots" ]
                        (List.range 1 4
                            |> List.map
                                (\i ->
                                    span
                                        [ class
                                            (if i <= String.length model.pinInput then
                                                "pin-dot filled"

                                             else
                                                "pin-dot"
                                            )
                                        ]
                                        []
                                )
                        )
                    ]
                , if model.pinError then
                    p [ class "pin-error" ] [ text "Incorrect PIN. Try again." ]

                  else
                    text ""
                , div [ class "pin-pad" ]
                    [ button [ class "btn pin-btn", onClick (PinDigit "1") ] [ text "1" ]
                    , button [ class "btn pin-btn", onClick (PinDigit "2") ] [ text "2" ]
                    , button [ class "btn pin-btn", onClick (PinDigit "3") ] [ text "3" ]
                    , button [ class "btn pin-btn", onClick (PinDigit "4") ] [ text "4" ]
                    , button [ class "btn pin-btn", onClick (PinDigit "5") ] [ text "5" ]
                    , button [ class "btn pin-btn", onClick (PinDigit "6") ] [ text "6" ]
                    , button [ class "btn pin-btn", onClick (PinDigit "7") ] [ text "7" ]
                    , button [ class "btn pin-btn", onClick (PinDigit "8") ] [ text "8" ]
                    , button [ class "btn pin-btn", onClick (PinDigit "9") ] [ text "9" ]
                    , button [ class "btn pin-btn pin-clear", onClick PinClear ] [ text "C" ]
                    , button [ class "btn pin-btn", onClick (PinDigit "0") ] [ text "0" ]
                    , button [ class "btn pin-btn pin-back", onClick PinBackspace ] [ text "⌫" ]
                    ]
                , div [ class "modal-buttons" ]
                    [ button [ class "btn btn-secondary", onClick CancelPin ] [ text "Cancel" ]
                    ]
                ]
            ]

    else
        text ""


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    stopPropagationOn "click" (Decode.succeed ( msg, True ))
