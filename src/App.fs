module App

open System
open Elmish
open Elmish.React
open Feliz
open System
open Zanaptak.TypedCssClasses

type Bulma = CssClasses<"https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.4/css/bulma.min.css", Naming.PascalCase>
type FA = CssClasses<"https://stackpath.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css", Naming.PascalCase>

type Filter =
  | All
  | Completed
  | Incomplete

type Todo = {
  Id : Guid
  Description : string
  Completed : bool
  BeingEdited: bool
  EditDescription: string
}

type State = {
  TodoList: Todo list
  NewTodo : string
  Filter: Filter
}

type Msg =
  | SetNewTodo of string
  | AddNewTodo
  | DeleteTodo of Guid
  | ToggleCompleted of Guid
  | CancelEdit of Guid
  | ApplyEdit of Guid
  | StartEditingTodo of Guid
  | SetEditedDescription of Guid*string
  | SetFilter of Filter


let init() = {
  TodoList = [
    { Id = Guid.NewGuid(); Description = "Learn F#"; Completed = false; BeingEdited = false; EditDescription = "" }
    { Id = Guid.NewGuid(); Description = "Learn Elmish"; Completed = true; BeingEdited = false; EditDescription = "" }
  ]
  NewTodo = ""
  Filter =  Filter.All
}

let modifyList todoId (state: State) todoFunc = 
    let nextTodoList =
        state.TodoList
        |> List.map (fun todo -> if todoId = todo.Id then todoFunc todo else todo)
      
    { state with TodoList = nextTodoList }

let update (msg: Msg) (state: State) =
  match msg with
  | SetNewTodo desc ->
      { state with NewTodo = desc }

  | AddNewTodo when String.IsNullOrWhiteSpace state.NewTodo ->
      state

  | AddNewTodo ->
      let nextTodo =
        { Id = Guid.NewGuid()
          Description = state.NewTodo
          Completed = false
          EditDescription = ""
          BeingEdited = false }

      { state with
          NewTodo = ""
          TodoList = List.append state.TodoList [nextTodo] }

  | DeleteTodo todoId ->
      let nextTodoList =
        state.TodoList
        |> List.filter (fun todo -> todo.Id <> todoId)

      { state with TodoList = nextTodoList }

  | ToggleCompleted todoId ->
      modifyList todoId state (fun todo -> { todo with Completed = not todo.Completed })

  | StartEditingTodo todoId ->
      modifyList todoId state (fun todo -> {todo with BeingEdited = true; EditDescription = todo.Description})

  | CancelEdit todoId ->
      modifyList todoId state (fun todo -> {todo with BeingEdited = false})

  | ApplyEdit todoId->
      modifyList todoId state (fun todo -> {todo with BeingEdited = false; Description = todo.EditDescription})

  | SetEditedDescription (todoId, newText) ->
      modifyList todoId state (fun todo -> {todo with EditDescription = newText})
  
  | SetFilter newFilter ->
      { state with Filter = newFilter }

// Helper function to easily construct div with only classes and children
let div (classes: string list) (children: ReactElement list) =
    Html.div [
        prop.classes classes
        prop.children children
    ]

let appTitle =
    Html.p [
      prop.className "title"
      prop.text "Elmish To-Do List"
    ]

let inputField (state: State) (dispatch: Msg -> unit) =
  div [ Bulma.Field; Bulma.HasAddons ] [
    div [ Bulma.Control; Bulma.IsExpanded ] [
      Html.input [
        prop.classes [ Bulma.Input; Bulma.IsMedium ]
        prop.valueOrDefault state.NewTodo
        prop.onTextChange (SetNewTodo >> dispatch)
      ]
    ]

    div [ Bulma.Control ] [
      Html.button [
        prop.classes [ Bulma.Button; Bulma.IsPrimary; Bulma.IsMedium ]
        prop.onClick (fun _ -> dispatch AddNewTodo)
        prop.children [
          Html.i [ prop.classes [ FA.Fa; FA.FaPlus ] ]
        ]
      ]
    ]
  ]

let renderTodo (todo: Todo) (dispatch: Msg -> unit) =
  div [ Bulma.Box ] [
    div [ Bulma.Columns; Bulma.IsMobile; Bulma.IsCentered ] [
      div [ Bulma.Column; Bulma.Subtitle] [
        Html.p [
          prop.className Bulma.Subtitle
          prop.text todo.Description
        ]
      ]

      div [ Bulma.Column; Bulma.IsNarrow ] [
        div [ Bulma.Buttons ] [
          Html.button [
            prop.className [ true, Bulma.Button; todo.Completed, Bulma.IsSuccess]
            prop.onClick (fun _ -> dispatch (ToggleCompleted todo.Id))
            prop.children [
              Html.i [ prop.classes [ FA.Fa; FA.FaCheck ] ]
            ]
          ]

          Html.button [
            prop.classes [ Bulma.Button; Bulma.IsPrimary ]
            prop.onClick (fun _ -> dispatch (StartEditingTodo todo.Id))
            prop.children [
              Html.i [ prop.classes [ FA.Fa; FA.FaEdit ] ]
            ]
          ]

          Html.button [
            prop.classes [ Bulma.Button; Bulma.IsDanger ]
            prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
            prop.children [
              Html.i [ prop.classes [ FA.Fa; FA.FaTimes ] ]
            ]
          ]
        ]
      ]
    ]
  ]

let renderEditForm (todoBeingEdited: Todo) (dispatch: Msg -> unit) =
  let applyClass =
    if todoBeingEdited.EditDescription = todoBeingEdited.Description
    then Bulma.IsOutlined
    else Bulma.IsPrimary
  
  div [ Bulma.Box ] [
    div [ Bulma.Field; Bulma.IsGrouped ] [
      div [ Bulma.Control; Bulma.IsExpanded ] [
        Html.input [
          prop.classes [ Bulma.Input; Bulma.IsMedium ]
          prop.valueOrDefault todoBeingEdited.EditDescription;
          prop.onTextChange (fun text -> dispatch (SetEditedDescription (todoBeingEdited.Id,text)))
        ]
      ]

      div [ Bulma.Control; Bulma.Buttons ] [
        Html.button [
          prop.classes [ Bulma.Button; applyClass]
          prop.onClick (fun _ -> dispatch (ApplyEdit todoBeingEdited.Id))
          prop.children [
            Html.i [ prop.classes [FA.Fa; FA.FaSave ] ]
          ]
        ]

        Html.button [
          prop.classes [Bulma.Button; Bulma.IsWarning]
          prop.onClick (fun _ -> dispatch (CancelEdit todoBeingEdited.Id))
          prop.children [
            Html.i [ prop.classes [FA.Fa; FA.FaArrowRight] ]
          ]
        ]
      ]
    ]
  ]

let todoList (state: State) (dispatch: Msg -> unit) =
  let todosToRender = match state.Filter with
    | All -> state.TodoList
    | Completed ->
      state.TodoList
        |> List.filter (fun todo -> todo.Completed = true)
    | Incomplete ->
      state.TodoList
        |> List.filter (fun todo -> todo.Completed = false)

  Html.ul [
    prop.children [
      for todo in todosToRender ->
        match todo.BeingEdited with
        | true ->
            renderEditForm todo dispatch
        | false ->
            renderTodo todo dispatch
    ]
  ]

let renderTab (sendType: Filter) (displayText: string) (state: State) (dispatch: Msg -> unit) =

  let classname = 
    if state.Filter = sendType
    then Bulma.IsActive
    else ""

  Html.li [
        prop.className classname
        prop.children [
          Html.a [
            prop.text displayText
            prop.onClick (fun _ -> dispatch (SetFilter sendType))
          ]
        ]
      ]


let renderFilterTabs (state: State) (dispatch: Msg -> unit) =
  div [ Bulma.Tabs; Bulma.IsToggle; Bulma.IsFullwidth ] [
    Html.ul [
      renderTab Filter.All "All" state dispatch
      renderTab Filter.Completed "Completed" state dispatch
      renderTab Filter.Incomplete "Not Completed" state dispatch
    ]
  ]

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      appTitle
      inputField state dispatch
      renderFilterTabs state dispatch
      todoList state dispatch
    ]
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
