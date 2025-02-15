module App

open Sutil
open Sutil.CoreElements
open Browser.Types
open Browser
open Thoth.Json

type Request = {
    url: string
    method: string
    body: string }

type Response = {
    statusCode: int
    body: string }

type Deferred<'t> =
  | HasNotStartedYet
  | InProgress
  | Resolved of 't

type AsyncOpStatus<'t> =
  | Started
  | Finished of 't

let sampleJson = """
{
  "message": {
    "affenpinscher": [],
    "african": [],
    "airedale": [],
    "akita": [],
    "appenzeller": [],
    "australian": [
      "kelpie",
      "shepherd"
    ]
  }
}
"""

type Breed =
    { name: string
      subBreeds: string list }

type Breeds = Breed list

type Message =
    { message : Map<string, list<string>> }

let transformJson (json: string) =
    match Decode.Auto.fromString<Message> json with
    | Ok parsedMap ->
        parsedMap.message
            |> Map.toList
            |> List.map (fun (k, v) ->
                         { name = k; subBreeds = v })
    | Error err -> failwithf "Decoding error: %s" err

type State = {
    dogBreedsList : Deferred<Result<Breeds, string>>
    //DogBreedDetails : Deferred<Result<string, string>>
    countedNumber : int }

let dogBreedsListUrl =
    "https://dog.ceo/api/breeds/list/all"

let getCounter (m: State) =
    m.countedNumber

let getDogBreedsList (m: State) =
    m.dogBreedsList

type Msg =
    | Increment
    | Decrement
    | LoadDogBreedsList of AsyncOpStatus<Result<Breeds, string>>
    //| LoadDogBreedsDetails of AsyncOperationStatus<Result<string, string>>

let init () : State * Cmd<Msg> =
    { dogBreedsList = HasNotStartedYet
      //DogBreedDetails = HasNotStartedYet
      countedNumber = 0 },
      Cmd.ofMsg (LoadDogBreedsList Started)

let httpRequest
    (req: Request) (resph: Response -> 'Msg) : Cmd<'Msg> =
    let command (dispatch: 'Msg -> unit) =
        let xhr = XMLHttpRequest.Create()
        xhr.``open``(method=req.method, url=req.url)
        xhr.onreadystatechange <- fun _ ->
            if xhr.readyState = ReadyState.Done
            then
                let response = {
                    statusCode = xhr.status
                    body = xhr.responseText }
                let messageToDispatch =  resph response
                dispatch messageToDispatch
        xhr.send(req.body)
    Cmd.ofEffect command

let update (msg : Msg) (state : State) : State * Cmd<Msg> =
    match msg with
    | Increment ->
        { state with countedNumber = state.countedNumber + 1 }
        , Cmd.none
    | Decrement ->
        { state with countedNumber = state.countedNumber - 1 }
        , Cmd.none
    | LoadDogBreedsList Started ->
        let nextState = {
            state with dogBreedsList = InProgress }
        let request = {
            url = dogBreedsListUrl
            method = "GET"; body = "" }
        let responseMapper (response: Response) =
            if response.statusCode = 200
            then LoadDogBreedsList (Finished (Ok (transformJson response.body)))
            else LoadDogBreedsList (Finished (Error "Could not load the content"))
        nextState, httpRequest request responseMapper
    | LoadDogBreedsList (Finished result) ->
        let nextState = { state with dogBreedsList = Resolved result }
        nextState, Cmd.none

let renderCounter n =
    text $"Counter = {n}"

let renderBreed (breed: Breed) =
    let subBreedsStr =
        match breed.subBreeds with
            | [] -> ""
            | _ -> "(" + String.concat ", " breed.subBreeds + ")"
    Html.div [
        text $"{breed.name} {subBreedsStr}" ]

let renderDogBreedsList (x: Deferred<Result<Breeds, string>>) =
    match x with
        | Resolved (Ok breeds) ->
            Html.ul [
                for breed in breeds do
                Html.li [ renderBreed breed ] ]
        | _ -> text $""

let view() =
    let state, dispatch =
        () |> Store.makeElmish init update ignore
    Html.div [
        disposeOnUnmount [ state ]
        Bind.el (state |> Store.map getDogBreedsList, renderDogBreedsList)
        Html.br []
        Bind.el (state |> Store.map getCounter, renderCounter)
        Html.div [
            Html.button [
                Ev.onClick (fun _ -> dispatch Decrement)
                text "-" ]
            Html.button [
                Ev.onClick (fun _ -> dispatch Increment)
                text "+" ] ] ]

view() |> Program.mount
