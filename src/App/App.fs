module App

open Browser
open Browser.Types
open Sutil
open Sutil.CoreElements
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

type ImageUrls = ImageUrl list

type Breed =
    { name: BreedName
      subBreeds: string list }

type Breeds = Breed list

type BreedsListApiRespBody =
    { message : Map<string, list<string>> }

type BreedImageUrlsApiRespBody =
    { message: string list }

type PageNumber = int

type Page
    = BreedsListPage
    | BreedDetailPage of BreedName * PageNumber

type BreedsInfoListState =
    Deferred<Result<Breeds, string>>

type BreedImageUrlsListState =
    Deferred<Result<ImageUrls, string>>

type State = {
    breedsInfoListState : BreedsInfoListState
    breedImageUrlsListState : BreedImageUrlsListState
    currentPage : Page }

let parseBreedsListJson (json: string) : Breeds =
    match Decode.Auto.fromString<BreedsListApiRespBody> json with
    | Ok parsedMap ->
        parsedMap.message
            |> Map.toList
            |> List.map (fun (k, v) ->
                         { name = k; subBreeds = v })
    | Error err -> failwithf "Decoding error: %s" err

let parseBreedImageUrlsJson (json: string) : ImageUrls =
    match Decode.Auto.fromString<BreedImageUrlsApiRespBody> json with
    | Ok parsedMap -> parsedMap.message
    | Error err -> failwithf "Decoding error: %s" err

let breedsListApiUrl =
    "https://dog.ceo/api/breeds/list/all"

let breedImagesApiUrl (breedName: BreedName) =
    $"https://dog.ceo/api/breed/{breedName}/images"

let getDogBreedsList (m: State) =
    m.breedsInfoListState

let getCurrentPage (m: State) =
    m.currentPage

type Msg =
    | ChangePage of Page
    | LoadDogBreedsList of AsyncOpStatus<Result<Breeds, string>>
    | LoadBreedImages of BreedName * AsyncOpStatus<Result<ImageUrls, string>>
    | TurnSubPage of PageNumber

let init () : State * Cmd<Msg> =
    { breedsInfoListState = HasNotStartedYet
      breedImageUrlsListState = HasNotStartedYet
      currentPage = BreedsListPage },
      Cmd.ofMsg (LoadDogBreedsList Started)

let httpRequest (req: Request) (handler: Response -> 'Msg) : Cmd<'Msg> =
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
    | ChangePage newPage ->
        let cmd : Cmd<Msg> =
            match newPage with
                | BreedsListPage -> Cmd.ofMsg (LoadDogBreedsList Started)
                | BreedDetailPage (breedName, _) ->
                    Cmd.ofMsg (LoadBreedImages (breedName, Started))
        { state with currentPage = newPage }, cmd
    | LoadDogBreedsList Started ->
        let nextState = {
            state with breedsInfoListState = InProgress }
        let request = {
            url = breedsListApiUrl
            method = "GET"; body = "" }
        let responseMapper (response: Response) =
            if response.statusCode = 200
            then LoadDogBreedsList (Finished (Ok (parseBreedsListJson response.body)))
            else LoadDogBreedsList (Finished (Error "Could not load the content"))
        nextState, httpRequest request responseMapper
    | LoadDogBreedsList (Finished result) ->
        let nextState = { state with breedsInfoListState = Resolved result }
        nextState, Cmd.none
    | LoadBreedImages (breedName, Started) ->
        let nextState = {
            state with breedImageUrlsListState = InProgress }
        let request = {
            url = breedImagesApiUrl breedName
            method = "GET"; body = "" }
        let responseMapper (response: Response) =
            if response.statusCode = 200
            then LoadBreedImages (breedName, (Finished (Ok (parseBreedImageUrlsJson response.body))))
            else LoadBreedImages (breedName, (Finished (Error "Could not load the content")))
        nextState, httpRequest request responseMapper
    | LoadBreedImages (breedName, Finished result) ->
        let nextState = { state with breedImageUrlsListState = Resolved result }
        nextState, Cmd.none
    | TurnSubPage newPageNumber ->
        match state.currentPage with
            | BreedsListPage -> state, Cmd.none
            | BreedDetailPage (breedName, pageNumber) ->
                if newPageNumber = pageNumber
                then state, Cmd.none
                else let newPage = BreedDetailPage (breedName, newPageNumber)
                     { state with currentPage = newPage }, Cmd.none

let renderBreed dispatch (breed: Breed) =
    let subBreedsStr =
        match breed.subBreeds with
            | [] -> ""
            | _ -> "(" + String.concat ", " breed.subBreeds + ")"
    let hf _ = dispatch (ChangePage (BreedDetailPage (breed.name, 0)))
    Html.div [
        Html.a [
            Ev.onClick hf
            text $"{breed.name} {subBreedsStr}" ] ]

let renderBreedsListPage (dispatch: Dispatch<Msg>)
    (data: BreedsInfoListState) =
    match data with
        | Resolved (Ok breeds) ->
            Html.ul [
                for breed in breeds do
                Html.li [ renderBreed dispatch breed ] ]
        | InProgress -> text $"Loading..."
        | _ -> text $"Try again"

let renderImage (imageUrl: ImageUrl) =
    Html.img [
        Attr.src imageUrl
        Attr.style [
            Css.height 150
            Css.width 150 ] ]

let paginationNumbering
    (dispatch: Dispatch<Msg>)
    (imageUrls: ImageUrls)
    (pageNumber: PageNumber) =
    let numOfSubPages: int = (List.length imageUrls) / 20
    Html.div [
        Html.label [ text $"Pages: " ]
        for n in [0..numOfSubPages] do
            Html.a [
                text $" {n+1} "
                Ev.onClick (fun _ -> dispatch (Msg.TurnSubPage n))
                Attr.style [
                    if n = pageNumber
                    then Css.fontWeightBold
                    else Css.fontWeightNormal ] ] ]

let frameImages (imageUrls: ImageUrls) (pageNumber: PageNumber) =
    let endIndex =
        min ((pageNumber + 1) * 20) (List.length imageUrls - 1)
    let startIndex =
         pageNumber * 20
    let selectedImageUrls: ImageUrls =
        imageUrls |> List.take endIndex
        |> List.skip startIndex
    Html.div [
       Attr.style [
           Css.displayFlex
           Css.flexWrapWrap
           Css.flexDirectionRow ]
       for imageUrl: ImageUrl in selectedImageUrls do
           renderImage imageUrl ]

let renderBreedImages (dispatch: Dispatch<Msg>)
    (data: BreedImageUrlsListState)
    (pageNumber: PageNumber) =
    match data with
        | Resolved (Ok imageUrls) ->
            match imageUrls with
                | [] ->
                    text $"No image for this breed"
                | _ ->
                    Html.div [
                        paginationNumbering dispatch imageUrls pageNumber
                        frameImages imageUrls pageNumber ]
        | InProgress -> text $"Loading... "
        | _ -> text $"Try again"

let detailPage (dispatch: Dispatch<Msg>)
    (imageUrlsState: BreedImageUrlsListState)
    (pageNumber: PageNumber)  =
    let hf _ = dispatch (ChangePage BreedsListPage)
    Html.div [
        Html.button [
            text $"<- Back to breed list"
            Ev.onClick hf ]
        Html.br []
        renderBreedImages dispatch imageUrlsState pageNumber ]

let paginate (stateStore: IStore<State>) (dispatch: Dispatch<Msg>) =
    let f (state: State) =
        match state.currentPage with
        | BreedsListPage ->
            renderBreedsListPage dispatch state.breedsInfoListState
        | BreedDetailPage (_, pn) ->
            detailPage dispatch state.breedImageUrlsListState pn
    Bind.el(stateStore,f)

let view() =
    let (stateStore: IStore<State>), (dispatch: Dispatch<Msg>) =
        Store.makeElmish init update ignore ()
    Html.div [
        disposeOnUnmount [ stateStore ]
        paginate stateStore dispatch ]

view() |> Program.mount
