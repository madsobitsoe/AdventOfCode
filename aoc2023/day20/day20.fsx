open System.IO

type Pulse = High | Low
type Module =
    | Broadcaster of string list
    | FlipFlop of bool * string list
    | Conjunction of Map<string,Pulse> * string list
    | Generic of string list

type Configuration =
    Map<string,Module>


let parseModule (ms:string) =
    let ms' = ms.Split " -> "
    let outputs = ms'.[1].Split ", " |> List.ofArray
    match ms'.[0] with
        | "broadcaster" -> "broadcaster", Broadcaster outputs
        | x ->
            match x.[0] with
                | '%' -> x.[1..], FlipFlop (false, outputs)
                | '&' -> x.[1..], Conjunction (Map.empty, outputs)
                | _ -> x, Generic outputs
        | _ -> failwith <| sprintf "Module not supported: %s" ms
    
let readFile file =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map parseModule



let sendPulse (p:Pulse) (source:string) (destination:string) (config:Configuration) =
    match Map.tryFind destination config with
        | None -> failwith <| sprintf "Destination module not found!\nPulse: %A\nSource: %s\nDestination: %s" p source destination
        | Some m ->
            printfn "Sending %A pulse from %s to %s" p source destination
    
    config

let receivePulse (p:Pulse) (source:string) (destination:string) (config:Configuration) =
    match Map.tryFind destination config with
        | None ->
            config,[]
            // failwith <| sprintf "Destination module not found!\nPulse: %A\nSource: %s\nDestination: %s" p source destination
        | Some m ->
            match m with
                | Broadcaster dests -> config, List.map (fun x -> p,destination,x) dests
                | FlipFlop (state,dests) ->
                    if p = High then config,[]
                    else
                        let config' = Map.add destination (FlipFlop ((not state),dests)) config
                        let p' = if state then Low else High
                        config', List.map (fun x -> p',destination,x) dests
                | Conjunction (map, dests) ->
                    let map' = Map.add source p map
                    let p' = if Map.forall (fun k v -> v = High) map' then Low else High
                    config, List.map (fun x -> p',destination,x) dests
                | _ -> config,[]


let rec sendReceive (config:Configuration) lowSent highSent acc toSend =
    printfn "toSend: %A" toSend
    printfn "acc: %A" acc
    match toSend with
        | [] ->
            match acc with
                | [] ->
                    printfn "lowSent: %A, highSent: %A" lowSent highSent 
                    lowSent,highSent,config
                | acc -> sendReceive config lowSent highSent [] acc
        | x::xs ->
            let (p,s,d) = x
            let config',newSends = receivePulse p s d config
            let ls,hs = if p = High then lowSent,highSent+1L else lowSent+1L,highSent
            sendReceive config' ls hs (acc@newSends) xs
            

let rec pushButton times sent (config:Configuration) =
    match times with
        | 0 -> sent,config
        | n ->
            let ls,hs = sent
            let ls',hs',config' = sendReceive config 0L 0L [] [Low,"Button","broadcaster"]
            pushButton (times-1) (ls+ls',hs+hs') config'



// let data = readFile "test.txt"
let data = readFile "test2.txt"
// let data = readFile "input.txt"
let config = Map.ofList data
// Find all modules that sends to a Conjunction module
// Then update the maps of all Conjunction modules

pushButton 1000 (0L,0L) config
