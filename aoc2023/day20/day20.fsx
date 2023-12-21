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

let receivePulse (p:Pulse) (source:string) (destination:string) (config:Configuration) (updatedConfig:Configuration) =
    // printfn "%s -%A- %s" source p destination
    match Map.tryFind destination config with
        | None -> updatedConfig,[]
        | Some m ->
            match m with
                | Broadcaster dests -> updatedConfig, List.map (fun x -> p,destination,x) dests
                | FlipFlop (state,dests) ->
                    if p = High then updatedConfig,[]
                    else
                        let config' = Map.add destination (FlipFlop ((not state),dests)) updatedConfig
                        let p' = if state then Low else High
                        config', List.map (fun x -> p',destination,x) dests
                | Conjunction (map, dests) ->
                    let map' = Map.add source p map
                    let p' = if Map.forall (fun k v -> v = High) map' then Low else High
                    let config' = Map.add destination (Conjunction (map', dests)) updatedConfig
                    config', List.map (fun x -> p',destination,x) dests
                | _ -> updatedConfig,[]

let rec sendReceive (config:Configuration) (updatedConfig:Configuration) lowSent highSent acc toSend =
    match toSend with
        | [] ->
            match acc with
                | [] -> lowSent,highSent,updatedConfig
                | acc -> sendReceive updatedConfig updatedConfig lowSent highSent [] acc
        | x::xs ->
            let (p,s,d) = x
            let config',newSends = receivePulse p s d config updatedConfig
            let ls,hs = if p = High then lowSent,highSent+1L else lowSent+1L,highSent
            sendReceive config config' ls hs (acc@newSends) xs

let rec pushButton times sent (config:Configuration) =
    match times with
        | 0 -> sent,config
        | n ->
            let ls,hs = sent
            let ls',hs',config' = sendReceive config config 0L 0L [] [Low,"Button","broadcaster"]
            pushButton (times-1) (ls+ls',hs+hs') config'



//let data = readFile "test.txt"
//let data = readFile "test2.txt"
let data = readFile "input.txt"

let config = Map.ofList data

// Find names of all Conjunction modules
let cons = Map.filter (fun k v -> match v with | Conjunction _ -> true | _ -> false) config |> Map.toList |> List.map fst

// Find all modules that sends to a Conjunction module
let consSends map name =
    let matchF _ v =
        match v with
            | Broadcaster dests -> List.contains name dests
            | FlipFlop (_,dests) -> List.contains name dests
            | Conjunction (_,dests) -> List.contains name dests
            | Generic _ -> false
    Map.filter matchF map
    |> Map.toList
    |> List.map fst
    |> (fun x -> name,x)

let updateMap map ((k:string),v) =
    let (Conjunction (ins,outs)) = Map.find k map
    let newIns = List.map (fun x -> x,Low) v |> Map.ofList
    Map.add k (Conjunction (newIns,outs)) map

// Then update the maps of all Conjunction modules
let config' =
    List.map (consSends config) cons
    |> List.fold (fun acc x -> updateMap acc x) config

pushButton 1000 (0L,0L) config'
|> fst
|> (fun (a,b) -> a * b)
|> printfn "part 1: %A"



// Part 2

// Looking for when these modules will send a High pulse
let rxInput =
    consSends config' "rx"
    |> snd |> List.head
    |> consSends config'
    |> snd



