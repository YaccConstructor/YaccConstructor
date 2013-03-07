open Google.GData.Documents
open Google.GData.Client
open Google.GData.Client.ResumableUpload
open Google.GData.Documents
open System.IO
open System

let mutable private service = Unchecked.defaultof<DocumentsService>
let mutable private  lastUploadEntry = Unchecked.defaultof<AtomEntry>
let mutable private loggedIn = false

let GetFolder fName = 
    let folderQuery = new FolderQuery()
    let folderFeed = service.Query(folderQuery)
    folderFeed.Entries |> Seq.find(fun x -> x.Title.Text = fName) :?> DocumentEntry

let CHUNK_SIZE = 1

let UploadFile (targetCollection:DocumentEntry) fileName =    
    if not loggedIn
    then failwith "Need to be logged in to upload documents."
    else                
        let ru = new ResumableUploader(CHUNK_SIZE)
        let info = new FileInfo(fileName)
        let entry = new DocumentEntry()
        entry.MediaSource <- new MediaFileSource(fileName, "application/octet-stream")
        entry.Title <- new AtomTextConstruct(AtomTextConstructElementType.Title,info.Name)        
        let createUploadUrl = new Uri(ResumableUploader.GetResumableCreateUri(targetCollection.Links).AbsoluteUri)
        let link = new AtomLink(createUploadUrl.AbsoluteUri)
        link.Rel <- ResumableUploader.CreateMediaRelation
        entry.Links.Add(link)
        let cla = new ClientLoginAuthenticator("YC-Builder", ServiceNames.Documents,service.Credentials)
        ru.Insert(cla, entry) |> ignore

let Login username password =
    if loggedIn
    then failwith "Already logged in."
    else
        try
            service <- new DocumentsService("DocListUploader")
            (service.RequestFactory :?> GDataRequestFactory).KeepAlive <- false
            service.setUserCredentials(username, password)
            let query = new DocumentsListQuery()
            query.NumberToRetrieve <- 1
            service.Query query
            |> ignore
            loggedIn <- true
        with
        | e ->
            loggedIn <- false
            service <- null
            raise e

let Logout () =
    loggedIn <- false    
    service <- null

let main () =
    let pwd= ref ""
    let user = ref ""
    let targetCollection = ref ""
    let file = ref ""
    let commandLineSpecs =
            ["-l", ArgType.String (fun s -> user := s), "Login."
             "-p", ArgType.String (fun s -> pwd := s), "Password."
             "-c", ArgType.String (fun s -> targetCollection := s), "Target collection."
             "-f", ArgType.String (fun s -> file := s), "File for upload."
             ] |> List.map (fun (shortcut, argtype, description) -> ArgInfo(shortcut, argtype, description))
    ArgParser.Parse commandLineSpecs
    Login !user !pwd
    let x = GetFolder !targetCollection
    UploadFile x !file
    Logout()

do main ()