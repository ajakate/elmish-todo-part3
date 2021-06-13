module App

open System
open Elmish
open Elmish.React
open Feliz
open System

type Filter =
  | All
  | Completed
  | Incomplete

type Todo = {
  Id : Guid
  Description : string
  Completed : bool
}

type TodoBeingEdited = {
  Id: Guid
  Description: string
}

type State = {
  TodoList: Todo list
  NewTodo : string
  TodoBeingEdited : TodoBeingEdited option
  Filter: Filter
}

type Msg =
  | SetNewTodo of string
  | AddNewTodo
  | DeleteTodo of Guid
  | ToggleCompleted of Guid
  | CancelEdit
  | ApplyEdit
  | StartEditingTodo of Guid
  | SetEditedDescription of string
  | SetFilter of Filter


let init() = {
  TodoList = [
    { Id = Guid.NewGuid(); Description = "Learn F#"; Completed = false }
    { Id = Guid.NewGuid(); Description = "Learn Elmish"; Completed = true }
  ]
  NewTodo = ""
  TodoBeingEdited = None
  Filter =  Filter.All
}

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
          Completed = false }

      { state with
          NewTodo = ""
          TodoList = List.append state.TodoList [nextTodo] }

  | DeleteTodo todoId ->
      let nextTodoList =
        state.TodoList
        |> List.filter (fun todo -> todo.Id <> todoId)

      { state with TodoList = nextTodoList }

  | ToggleCompleted todoId ->
      let nextTodoList =
        state.TodoList
        |> List.map (fun todo ->
           if todo.Id = todoId
           then { todo with Completed = not todo.Completed }
           else todo)

      { state with TodoList = nextTodoList }

  | StartEditingTodo todoId ->
      let nextEditModel =
        state.TodoList
        |> List.tryFind (fun todo -> todo.Id = todoId)
        |> Option.map (fun todo -> { Id = todoId; Description = todo.Description })

      { state with TodoBeingEdited = nextEditModel }

  | CancelEdit ->
      { state with TodoBeingEdited = None }

  | ApplyEdit ->
      match state.TodoBeingEdited with
      | None -> state
      | Some todoBeingEdited when todoBeingEdited.Description = "" -> state
      | Some todoBeingEdited ->
          let nextTodoList =
            state.TodoList
            |> List.map (fun todo ->
                if todo.Id = todoBeingEdited.Id
                then { todo with Description = todoBeingEdited.Description }
                else todo)

          { state with TodoList = nextTodoList; TodoBeingEdited = None }

  | SetEditedDescription newText ->
      let nextEditModel =
        state.TodoBeingEdited
        |> Option.map (fun todoBeingEdited -> { todoBeingEdited with Description = newText })

      { state with TodoBeingEdited = nextEditModel }
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
  div [ "field"; "has-addons" ] [
    div [ "control"; "is-expanded" ] [
      Html.input [
        prop.classes [ "input"; "is-medium" ]
        prop.valueOrDefault state.NewTodo
        prop.onTextChange (SetNewTodo >> dispatch)
      ]
    ]

    div [ "control" ] [
      Html.button [
        prop.classes [ "button"; "is-primary"; "is-medium" ]
        prop.onClick (fun _ -> dispatch AddNewTodo)
        prop.children [
          Html.i [ prop.classes [ "fa"; "fa-plus" ] ]
        ]
      ]
    ]
  ]

let renderTodo (todo: Todo) (dispatch: Msg -> unit) =
  div [ "box" ] [
    div [ "columns"; "is-mobile"; "is-vcentered" ] [
      div [ "column"; "subtitle"] [
        Html.p [
          prop.className "subtitle"
          prop.text todo.Description
        ]
      ]

      div [ "column"; "is-narrow" ] [
        div [ "buttons" ] [
          Html.button [
            prop.className [ true, "button"; todo.Completed, "is-success"]
            prop.onClick (fun _ -> dispatch (ToggleCompleted todo.Id))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-check" ] ]
            ]
          ]

          Html.button [
            prop.classes [ "button"; "is-primary" ]
            prop.onClick (fun _ -> dispatch (StartEditingTodo  todo.Id))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-edit" ] ]
            ]
          ]

          Html.button [
            prop.classes [ "button"; "is-danger" ]
            prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-times" ] ]
            ]
          ]
        ]
      ]
    ]
  ]

let renderEditForm (originalText: string) (todoBeingEdited: TodoBeingEdited) (dispatch: Msg -> unit) =
  let applyClass =
    if todoBeingEdited.Description = originalText
    then "is-outlined"
    else "is-primary"
  
  div [ "box" ] [
    div [ "field is-grouped" ] [
      div [ "control is-expanded" ] [
        Html.input [
          prop.classes [ "input"; "is-medium" ]
          prop.valueOrDefault todoBeingEdited.Description;
          prop.onTextChange (SetEditedDescription >> dispatch)
        ]
      ]

      div [ "control"; "buttons" ] [
        Html.button [
          prop.classes [ "button"; applyClass]
          prop.onClick (fun _ -> dispatch ApplyEdit)
          prop.children [
            Html.i [ prop.classes ["fa"; "fa-save" ] ]
          ]
        ]

        Html.button [
          prop.classes ["button"; "is-warning"]
          prop.onClick (fun _ -> dispatch CancelEdit)
          prop.children [
            Html.i [ prop.classes ["fa"; "fa-arrow-right"] ]
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
        match state.TodoBeingEdited with
        | Some todoBeingEdited when todoBeingEdited.Id = todo.Id ->
            renderEditForm todo.Description todoBeingEdited dispatch
        | otherwise ->
            renderTodo todo dispatch
    ]
  ]

let renderTab (sendType: Filter) (displayText: string) (state: State) (dispatch: Msg -> unit) =

  let classname = 
    if state.Filter = sendType
    then "is-active"
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
  div [ "tabs"; "is-toggle"; "is-fullwidth" ] [
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