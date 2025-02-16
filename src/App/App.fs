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

type BreedName = string

type ImageUrl = string

type Breed =
    { name: BreedName
      subBreeds: string list }

type Breeds = Breed list

type BreedsListRespBody =
    { message : Map<string, list<string>> }

type BreedDetailRespBody =
    { message: ImageUrl list }

let transBreedsListJson (json: string) =
    match Decode.Auto.fromString<BreedsListRespBody> json with
    | Ok parsedMap ->
        parsedMap.message
            |> Map.toList
            |> List.map (fun (k, v) ->
                         { name = k; subBreeds = v })
    | Error err -> failwithf "Decoding error: %s" err

let transBreedDetailJson (json: string) =
    match Decode.Auto.fromString<BreedDetailRespBody> json with
    | Ok parsedMap -> parsedMap.message
    | Error err -> failwithf "Decoding error: %s" err

type Page
    = BreedsListPage
    | BreedDetailPage of BreedName

type State = {
    dogBreedsList : Deferred<Result<Breeds, string>>
    //DogBreedDetails : Deferred<Result<string, string>>
    countedNumber : int
    currentPage : Page }

let dogBreedsListUrl =
    "https://dog.ceo/api/breeds/list/all"

let getCounter (m: State) =
    m.countedNumber

let getDogBreedsList (m: State) =
    m.dogBreedsList

let getCurrentPage (m: State) =
    m.currentPage

type Msg =
    | Increment
    | Decrement
    | ChangePage of Page
    | LoadDogBreedsList of AsyncOpStatus<Result<Breeds, string>>
    //| LoadDogBreedsDetails of AsyncOperationStatus<Result<string, string>>

let init () : State * Cmd<Msg> =
    { dogBreedsList = HasNotStartedYet
      //DogBreedDetails = HasNotStartedYet
      countedNumber = 0
      currentPage = BreedsListPage },
      Cmd.ofMsg (LoadDogBreedsList Started)

let httpRequest
    (req: Request) (handler: Response -> 'Msg) : Cmd<'Msg> =
    let command (dispatch: 'Msg -> unit) =
        let xhr = XMLHttpRequest.Create()
        xhr.``open``(method=req.method, url=req.url)
        xhr.onreadystatechange <- fun _ ->
            if xhr.readyState = ReadyState.Done
            then
                let response = {
                    statusCode = xhr.status
                    body = xhr.responseText }
                let messageToDispatch = handler response
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
    | ChangePage newPage ->
        { state with currentPage = newPage }, Cmd.none
    | LoadDogBreedsList Started ->
        let nextState = {
            state with dogBreedsList = InProgress }
        let request = {
            url = dogBreedsListUrl
            method = "GET"; body = "" }
        let responseMapper (response: Response) =
            if response.statusCode = 200
            then LoadDogBreedsList (Finished (Ok (transBreedsListJson response.body)))
            else LoadDogBreedsList (Finished (Error "Could not load the content"))
        nextState, httpRequest request responseMapper
    | LoadDogBreedsList (Finished result) ->
        let nextState = { state with dogBreedsList = Resolved result }
        nextState, Cmd.none

let renderCounter n =
    text $"Counter = {n}"

let renderBreed dispatch (breed: Breed) =
    let subBreedsStr =
        match breed.subBreeds with
            | [] -> ""
            | _ -> "(" + String.concat ", " breed.subBreeds + ")"
    let hf _ = dispatch (ChangePage (BreedDetailPage breed.name))
    Html.div [
        Html.a [
            Ev.onClick hf
            text $"{breed.name} {subBreedsStr}" ] ]

let renderBreedsListPage dispatch (x: Deferred<Result<Breeds, string>>) =
    match x with
        | Resolved (Ok breeds) ->
            Html.ul [
                for breed in breeds do
                Html.li [ renderBreed dispatch breed ] ]
        | _ -> text $""

let paginate (stateStore: IStore<State>) dispatch =
    let f (state: State) =
        match state.currentPage with
        | BreedsListPage -> renderBreedsListPage dispatch state.dogBreedsList
        | BreedDetailPage v -> text $"detail page for {v}"
    Bind.el(stateStore,f)

let view() =
    let state, dispatch =
        Store.makeElmish init update ignore ()
    Html.div [
        disposeOnUnmount [ state ]
        paginate state dispatch ]

view() |> Program.mount
