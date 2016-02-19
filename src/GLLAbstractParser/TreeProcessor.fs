module TreeProcessor
open Yard.Generators.Common.ASTGLL
#nowarn "40"

type Message(node : NonTerminalNode, name) =
    member this.Node = node
    member this.Name = name
    
type TreeProcessor () =
    member this.printerAgent = MailboxProcessor<Message>.Start(fun inbox -> 
        let rec messageLoop = async{
            let! msg = inbox.Receive()
            printfn "Name is: %s" msg.Name
            return! messageLoop  
            }

        messageLoop 
        )

