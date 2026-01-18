port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, stopPropagationOn, on, keyCode)
import Json.Decode as Decode
import Json.Encode as Encode
import Task


-- PORTS


port saveAdminSession : Bool -> Cmd msg


port saveFormData : Encode.Value -> Cmd msg


port downloadCsv : Encode.Value -> Cmd msg


port downloadPdf : Encode.Value -> Cmd msg


port loadAdminSession : (Bool -> msg) -> Sub msg


type alias Flags =
    { isAdmin : Bool
    , formData : Maybe Decode.Value
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = initWithFlags
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


initWithFlags : Flags -> ( Model, Cmd Msg )
initWithFlags flags =
    let
        appData =
            case flags.formData of
                Just json ->
                    case Decode.decodeValue appDataDecoder json of
                        Ok loaded ->
                            loaded

                        Err _ ->
                            { forms = [ emptyForm 1 ], activeIndex = 0, nextId = 2 }

                Nothing ->
                    { forms = [ emptyForm 1 ], activeIndex = 0, nextId = 2 }
    in
    ( { forms = appData.forms
      , activeFormIndex = appData.activeIndex
      , nextFormId = appData.nextId
      , editing = Nothing
      , editValue = ""
      , isAdmin = flags.isAdmin
      , showPinModal = False
      , pinInput = ""
      , pinError = False
      , pendingEdit = Nothing
      , confirmDelete = Nothing
      }
    , Cmd.none
    )


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
    { id : Int
    , catName : String
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
    { forms : List MedicationForm
    , activeFormIndex : Int
    , nextFormId : Int
    , editing : Maybe EditingCell
    , editValue : String
    , isAdmin : Bool
    , showPinModal : Bool
    , pinInput : String
    , pinError : Bool
    , pendingEdit : Maybe EditingCell
    , confirmDelete : Maybe Int
    }


adminPin : String
adminPin =
    "1234"


emptyForm : Int -> MedicationForm
emptyForm id =
    { id = id
    , catName = ""
    , diagnosis = ""
    , treatment = ""
    , careContact = ""
    , vetInfo = ""
    , startDate = ""
    , endDate = ""
    , dailyRecords = []
    }


getActiveForm : Model -> Maybe MedicationForm
getActiveForm model =
    List.head (List.drop model.activeFormIndex model.forms)


updateActiveForm : (MedicationForm -> MedicationForm) -> Model -> Model
updateActiveForm fn model =
    let
        updateAt index forms =
            List.indexedMap
                (\i form ->
                    if i == index then
                        fn form

                    else
                        form
                )
                forms
    in
    { model | forms = updateAt model.activeFormIndex model.forms }


isProtectedField : CellField -> Bool
isProtectedField field =
    case field of
        AMInitials ->
            True

        PMInitials ->
            True

        Comments ->
            False



-- JSON ENCODERS


encodeAppData : Model -> Encode.Value
encodeAppData model =
    Encode.object
        [ ( "forms", Encode.list encodeForm model.forms )
        , ( "activeIndex", Encode.int model.activeFormIndex )
        , ( "nextId", Encode.int model.nextFormId )
        ]


encodeForm : MedicationForm -> Encode.Value
encodeForm form =
    Encode.object
        [ ( "id", Encode.int form.id )
        , ( "catName", Encode.string form.catName )
        , ( "diagnosis", Encode.string form.diagnosis )
        , ( "treatment", Encode.string form.treatment )
        , ( "careContact", Encode.string form.careContact )
        , ( "vetInfo", Encode.string form.vetInfo )
        , ( "startDate", Encode.string form.startDate )
        , ( "endDate", Encode.string form.endDate )
        , ( "dailyRecords", Encode.list encodeDailyRecord form.dailyRecords )
        ]


encodeDailyRecord : DailyRecord -> Encode.Value
encodeDailyRecord record =
    Encode.object
        [ ( "date", Encode.string record.date )
        , ( "amInitials", Encode.string record.amInitials )
        , ( "pmInitials", Encode.string record.pmInitials )
        , ( "comments", Encode.string record.comments )
        ]



-- JSON DECODERS


type alias AppData =
    { forms : List MedicationForm
    , activeIndex : Int
    , nextId : Int
    }


appDataDecoder : Decode.Decoder AppData
appDataDecoder =
    Decode.map3 AppData
        (Decode.field "forms" (Decode.list formDecoder))
        (Decode.field "activeIndex" Decode.int)
        (Decode.field "nextId" Decode.int)


formDecoder : Decode.Decoder MedicationForm
formDecoder =
    Decode.succeed MedicationForm
        |> decodeAndMap (Decode.field "id" Decode.int)
        |> decodeAndMap (Decode.field "catName" Decode.string)
        |> decodeAndMap (Decode.field "diagnosis" Decode.string)
        |> decodeAndMap (Decode.field "treatment" Decode.string)
        |> decodeAndMap (Decode.field "careContact" Decode.string)
        |> decodeAndMap (Decode.field "vetInfo" Decode.string)
        |> decodeAndMap (Decode.field "startDate" Decode.string)
        |> decodeAndMap (Decode.field "endDate" Decode.string)
        |> decodeAndMap (Decode.field "dailyRecords" (Decode.list dailyRecordDecoder))


decodeAndMap : Decode.Decoder a -> Decode.Decoder (a -> b) -> Decode.Decoder b
decodeAndMap =
    Decode.map2 (|>)


dailyRecordDecoder : Decode.Decoder DailyRecord
dailyRecordDecoder =
    Decode.map4 DailyRecord
        (Decode.field "date" Decode.string)
        (Decode.field "amInitials" Decode.string)
        (Decode.field "pmInitials" Decode.string)
        (Decode.field "comments" Decode.string)



-- UPDATE


type Msg
    = NoOp
    | SwitchTab Int
    | AddNewForm
    | RequestDeleteForm Int
    | ConfirmDeleteForm
    | CancelDeleteForm
    | DeleteForm Int
    | StartEdit String CellField String
    | UpdateEditValue String
    | SaveEdit
    | CancelEdit
    | UpdateFormField FormField String
    | UpdateStartDate String
    | UpdateEndDate String
    | GenerateDates
    | AddDayBefore
    | AddDayAfter
    | RemoveFirstDay
    | RemoveLastDay
    | RequestAdminLogin
    | RequestPinEntry EditingCell String
    | PinDigit String
    | PinBackspace
    | PinClear
    | SubmitPin
    | CancelPin
    | Logout
    | FocusResult (Result Dom.Error ())
    | LoadAdminSession Bool
    | ExportCsv
    | ExportPdf


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

        SwitchTab index ->
            let
                newModel =
                    { model | activeFormIndex = index, editing = Nothing, editValue = "" }
            in
            ( newModel, saveFormData (encodeAppData newModel) )

        AddNewForm ->
            let
                newForm =
                    emptyForm model.nextFormId

                newModel =
                    { model
                        | forms = model.forms ++ [ newForm ]
                        , activeFormIndex = List.length model.forms
                        , nextFormId = model.nextFormId + 1
                    }
            in
            ( newModel, saveFormData (encodeAppData newModel) )

        RequestDeleteForm index ->
            ( { model | confirmDelete = Just index }, Cmd.none )

        ConfirmDeleteForm ->
            case model.confirmDelete of
                Just index ->
                    update (DeleteForm index) { model | confirmDelete = Nothing }

                Nothing ->
                    ( model, Cmd.none )

        CancelDeleteForm ->
            ( { model | confirmDelete = Nothing }, Cmd.none )

        DeleteForm index ->
            if List.length model.forms <= 1 then
                -- Don't delete the last form
                ( model, Cmd.none )

            else
                let
                    newForms =
                        List.take index model.forms ++ List.drop (index + 1) model.forms

                    newActiveIndex =
                        if model.activeFormIndex >= List.length newForms then
                            List.length newForms - 1

                        else if model.activeFormIndex > index then
                            model.activeFormIndex - 1

                        else
                            model.activeFormIndex

                    newModel =
                        { model | forms = newForms, activeFormIndex = newActiveIndex }
                in
                ( newModel, saveFormData (encodeAppData newModel) )

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
                    let
                        newModel =
                            updateActiveForm (updateDailyRecord editingCell model.editValue) model
                                |> (\m -> { m | editing = Nothing, editValue = "" })
                    in
                    ( newModel, saveFormData (encodeAppData newModel) )

                Nothing ->
                    ( model, Cmd.none )

        CancelEdit ->
            ( { model | editing = Nothing, editValue = "" }, Cmd.none )

        UpdateFormField field value ->
            let
                newModel =
                    updateActiveForm (updateFormField field value) model
            in
            ( newModel, saveFormData (encodeAppData newModel) )

        UpdateStartDate value ->
            let
                newModel =
                    updateActiveForm (\form -> { form | startDate = value }) model
            in
            ( newModel, saveFormData (encodeAppData newModel) )

        UpdateEndDate value ->
            let
                newModel =
                    updateActiveForm (\form -> { form | endDate = value }) model
            in
            ( newModel, saveFormData (encodeAppData newModel) )

        GenerateDates ->
            let
                newModel =
                    updateActiveForm generateDailyRecords model
            in
            ( newModel, saveFormData (encodeAppData newModel) )

        AddDayBefore ->
            let
                newModel =
                    updateActiveForm addDayBefore model
            in
            ( newModel, saveFormData (encodeAppData newModel) )

        AddDayAfter ->
            let
                newModel =
                    updateActiveForm addDayAfter model
            in
            ( newModel, saveFormData (encodeAppData newModel) )

        RemoveFirstDay ->
            let
                newModel =
                    updateActiveForm removeFirstDay model
            in
            ( newModel, saveFormData (encodeAppData newModel) )

        RemoveLastDay ->
            let
                newModel =
                    updateActiveForm removeLastDay model
            in
            ( newModel, saveFormData (encodeAppData newModel) )

        RequestAdminLogin ->
            ( { model
                | showPinModal = True
                , pinInput = ""
                , pinError = False
                , pendingEdit = Nothing
              }
            , Cmd.none
            )

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

        ExportCsv ->
            case getActiveForm model of
                Just form ->
                    ( model, downloadCsv (encodeForm form) )

                Nothing ->
                    ( model, Cmd.none )

        ExportPdf ->
            case getActiveForm model of
                Just form ->
                    ( model, downloadPdf (encodeForm form) )

                Nothing ->
                    ( model, Cmd.none )


focusElement : String -> Cmd Msg
focusElement id =
    Task.attempt FocusResult (Dom.focus id)


updateFormField : FormField -> String -> MedicationForm -> MedicationForm
updateFormField field value form =
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


updateDailyRecord : EditingCell -> String -> MedicationForm -> MedicationForm
updateDailyRecord editingCell newValue form =
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


addDayBefore : MedicationForm -> MedicationForm
addDayBefore form =
    case List.head form.dailyRecords of
        Just firstRecord ->
            let
                prevDate =
                    getPreviousDay firstRecord.date

                newRecord =
                    { date = prevDate, amInitials = "", pmInitials = "", comments = "" }
            in
            { form
                | dailyRecords = newRecord :: form.dailyRecords
                , startDate = prevDate
            }

        Nothing ->
            form


addDayAfter : MedicationForm -> MedicationForm
addDayAfter form =
    case List.reverse form.dailyRecords |> List.head of
        Just lastRecord ->
            let
                nextDate =
                    getNextDay lastRecord.date

                newRecord =
                    { date = nextDate, amInitials = "", pmInitials = "", comments = "" }
            in
            { form
                | dailyRecords = form.dailyRecords ++ [ newRecord ]
                , endDate = nextDate
            }

        Nothing ->
            form


removeFirstDay : MedicationForm -> MedicationForm
removeFirstDay form =
    case form.dailyRecords of
        _ :: rest ->
            { form
                | dailyRecords = rest
                , startDate =
                    List.head rest
                        |> Maybe.map .date
                        |> Maybe.withDefault form.startDate
            }

        [] ->
            form


removeLastDay : MedicationForm -> MedicationForm
removeLastDay form =
    let
        newRecords =
            List.take (List.length form.dailyRecords - 1) form.dailyRecords
    in
    { form
        | dailyRecords = newRecords
        , endDate =
            List.reverse newRecords
                |> List.head
                |> Maybe.map .date
                |> Maybe.withDefault form.endDate
    }


getPreviousDay : String -> String
getPreviousDay dateStr =
    case parseDate dateStr of
        Just ( y, m, d ) ->
            if d > 1 then
                formatDateTuple ( y, m, d - 1 )

            else if m > 1 then
                formatDateTuple ( y, m - 1, daysInMonth y (m - 1) )

            else
                formatDateTuple ( y - 1, 12, 31 )

        Nothing ->
            dateStr


getNextDay : String -> String
getNextDay dateStr =
    case parseDate dateStr of
        Just ( y, m, d ) ->
            let
                maxD =
                    daysInMonth y m
            in
            if d < maxD then
                formatDateTuple ( y, m, d + 1 )

            else if m < 12 then
                formatDateTuple ( y, m + 1, 1 )

            else
                formatDateTuple ( y + 1, 1, 1 )

        Nothing ->
            dateStr


parseDate : String -> Maybe ( Int, Int, Int )
parseDate str =
    case String.split "-" str of
        [ y, m, d ] ->
            Maybe.map3 (\year month day -> ( year, month, day ))
                (String.toInt y)
                (String.toInt m)
                (String.toInt d)

        _ ->
            Nothing


formatDateTuple : ( Int, Int, Int ) -> String
formatDateTuple ( y, m, d ) =
    String.fromInt y
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt m)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt d)


daysInMonth : Int -> Int -> Int
daysInMonth year month =
    if List.member month [ 1, 3, 5, 7, 8, 10, 12 ] then
        31

    else if List.member month [ 4, 6, 9, 11 ] then
        30

    else if modBy 4 year == 0 && (modBy 100 year /= 0 || modBy 400 year == 0) then
        29

    else
        28


generateDailyRecords : MedicationForm -> MedicationForm
generateDailyRecords form =
    if String.isEmpty form.startDate || String.isEmpty form.endDate then
        form

    else
        let
            dates =
                generateDateRange form.startDate form.endDate

            existingByDate =
                List.map (\r -> ( r.date, r )) form.dailyRecords
                    |> List.foldl (\( d, r ) acc -> ( d, r ) :: acc) []

            findExisting date =
                List.filter (\( d, _ ) -> d == date) existingByDate
                    |> List.head
                    |> Maybe.map Tuple.second

            newRecords =
                List.map
                    (\date ->
                        case findExisting date of
                            Just existing ->
                                existing

                            Nothing ->
                                { date = date, amInitials = "", pmInitials = "", comments = "" }
                    )
                    dates
        in
        { form | dailyRecords = newRecords }


generateDateRange : String -> String -> List String
generateDateRange start end =
    let
        nextDayTuple ( y, m, d ) =
            let
                maxD =
                    daysInMonth y m
            in
            if d < maxD then
                ( y, m, d + 1 )

            else if m < 12 then
                ( y, m + 1, 1 )

            else
                ( y + 1, 1, 1 )

        compareDates ( y1, m1, d1 ) ( y2, m2, d2 ) =
            compare ( y1, m1, d1 ) ( y2, m2, d2 )

        generateHelper current endDate acc =
            if compareDates current endDate == GT then
                List.reverse acc

            else
                generateHelper (nextDayTuple current) endDate (formatDateTuple current :: acc)
    in
    case ( parseDate start, parseDate end ) of
        ( Just s, Just e ) ->
            generateHelper s e []

        _ ->
            []



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ viewHeader model.isAdmin
        , viewTabBar model
        , case getActiveForm model of
            Just form ->
                div []
                    [ viewFormInfo form
                    , viewScheduleTable model form
                    , viewExportButtons
                    ]

            Nothing ->
                p [] [ text "No form selected" ]
        , viewEditModal model
        , viewPinModal model
        , viewDeleteConfirmModal model
        ]


viewHeader : Bool -> Html Msg
viewHeader isAdmin =
    header [ class "header" ]
        [ div [ class "header-top" ]
            [ h1 [] [ text "Meowderall" ]
            , if isAdmin then
                button [ class "btn btn-small btn-admin-toggle btn-logout", onClick Logout ] [ text "Logout Admin" ]

              else
                button [ class "btn btn-small btn-admin-toggle", onClick RequestAdminLogin ] [ text "Admin Login" ]
            ]
        , p [ class "tagline" ] [ text "Cat medication tracking for shelters" ]
        , p [ class "notice" ] [ text "Data is stored locally in your browser only." ]
        , if isAdmin then
            p [ class "admin-badge" ] [ text "Admin Mode" ]

          else
            text ""
        ]


viewTabBar : Model -> Html Msg
viewTabBar model =
    div [ class "tab-bar" ]
        [ div [ class "tabs" ]
            (List.indexedMap (viewTab model.activeFormIndex (List.length model.forms > 1)) model.forms
                ++ [ button [ class "tab tab-add", onClick AddNewForm ] [ text "+" ] ]
            )
        ]


viewTab : Int -> Bool -> Int -> MedicationForm -> Html Msg
viewTab activeIndex canDelete index form =
    let
        tabName =
            if String.isEmpty form.catName then
                "Cat " ++ String.fromInt form.id

            else
                form.catName

        isActive =
            index == activeIndex
    in
    div
        [ class
            (if isActive then
                "tab active"

             else
                "tab"
            )
        ]
        [ span [ class "tab-name", onClick (SwitchTab index) ] [ text tabName ]
        , if canDelete && isActive then
            button
                [ class "tab-close"
                , onClick (RequestDeleteForm index)
                ]
                [ text "×" ]

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
            , viewDateInputs form.startDate form.endDate
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


viewDateInputs : String -> String -> Html Msg
viewDateInputs startDate endDate =
    div [ class "field date-inputs" ]
        [ Html.label [] [ text "Treatment Period" ]
        , div [ class "date-input-row" ]
            [ input
                [ type_ "date"
                , Html.Attributes.value startDate
                , onInput UpdateStartDate
                , class "date-input"
                ]
                []
            , span [ class "date-separator" ] [ text "to" ]
            , input
                [ type_ "date"
                , Html.Attributes.value endDate
                , onInput UpdateEndDate
                , class "date-input"
                ]
                []
            , button [ class "btn btn-small", onClick GenerateDates ] [ text "Generate" ]
            ]
        ]


formatDate : String -> String
formatDate dateStr =
    case String.split "-" dateStr of
        [ _, month, day ] ->
            month ++ "/" ++ day

        _ ->
            dateStr


viewScheduleTable : Model -> MedicationForm -> Html Msg
viewScheduleTable model form =
    if List.isEmpty form.dailyRecords then
        div [ class "card schedule-section" ]
            [ h2 [] [ text "Daily Medication Log" ]
            , p [ class "empty-message" ] [ text "Set start and end dates above, then click \"Generate\" to create the schedule." ]
            ]

    else
        div [ class "card schedule-section" ]
            [ h2 [] [ text "Daily Medication Log" ]
            , viewDayControls True
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
                    , tbody [] (List.map (viewDailyRow model) form.dailyRecords)
                    ]
                ]
            , viewDayControls False
            ]


viewDayControls : Bool -> Html Msg
viewDayControls isTop =
    div [ class "day-controls" ]
        (if isTop then
            [ button
                [ class "btn btn-small btn-day", onClick AddDayBefore ]
                [ text "+ Add day before" ]
            , button
                [ class "btn btn-small btn-day btn-danger", onClick RemoveFirstDay ]
                [ text "− Remove first day" ]
            ]

         else
            [ button
                [ class "btn btn-small btn-day btn-danger", onClick RemoveLastDay ]
                [ text "− Remove last day" ]
            , button
                [ class "btn btn-small btn-day", onClick AddDayAfter ]
                [ text "+ Add day after" ]
            ]
        )


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


viewExportButtons : Html Msg
viewExportButtons =
    div [ class "export-section" ]
        [ button [ class "btn btn-export", onClick ExportCsv ] [ text "Download CSV" ]
        , button [ class "btn btn-export", onClick ExportPdf ] [ text "Download PDF" ]
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
                        , onEnter SaveEdit
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


viewDeleteConfirmModal : Model -> Html Msg
viewDeleteConfirmModal model =
    case model.confirmDelete of
        Just index ->
            let
                catName =
                    List.drop index model.forms
                        |> List.head
                        |> Maybe.map
                            (\form ->
                                if String.isEmpty form.catName then
                                    "Cat " ++ String.fromInt form.id

                                else
                                    form.catName
                            )
                        |> Maybe.withDefault "this cat"
            in
            div [ class "modal-overlay", onClick CancelDeleteForm ]
                [ div
                    [ class "modal delete-modal"
                    , onClickStopPropagation NoOp
                    ]
                    [ h2 [] [ text "Delete Form?" ]
                    , p [ class "delete-message" ]
                        [ text "OK to delete '"
                        , strong [] [ text catName ]
                        , text "' form?"
                        ]
                    , p [ class "delete-warning" ] [ text "This cannot be undone." ]
                    , div [ class "modal-buttons" ]
                        [ button [ class "btn btn-secondary", onClick CancelDeleteForm ] [ text "Cancel" ]
                        , button [ class "btn btn-delete", onClick ConfirmDeleteForm ] [ text "Yes, Delete" ]
                        ]
                    ]
                ]

        Nothing ->
            text ""


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    stopPropagationOn "click" (Decode.succeed ( msg, True ))


onEnter : msg -> Attribute msg
onEnter msg =
    on "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Enter" then
                        Decode.succeed msg

                    else
                        Decode.fail "not enter"
                )
        )
