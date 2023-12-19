open System.IO

let readFile file =
    File.ReadAllText file
    |> (fun s -> s.Split "\n\n")
    |> (fun x -> Array.head x, (Array.tail >> Array.head) x)
    |> (fun (r,i) -> r.Split "\n", i |> Seq.filter (fun x -> x <> '{' && x <> '}') |> Seq.fold (fun acc c -> acc + (string c)) "" |> (fun x -> x.Split "\n"))
    |> (fun (r,i) -> Array.filter ((<>) "") r, Array.filter ((<>) "") i)
    |> (fun (r,i) -> List.ofArray r, List.ofArray i)



type Item = { x:int64; m:int64; a:int64; s:int64 }
let defaultItem = { x = 0L; m = 0L; a = 0L; s = 0L }



let parseItem (i:string) =
    let rec p ii acc =
        match ii with
            | [] -> acc
            | ["x";v]::ii -> p ii { acc with x = (int64 v) }
            | ["m";v]::ii -> p ii { acc with m = (int64 v) }
            | ["a";v]::ii -> p ii { acc with a = (int64 v) }
            | ["s";v]::ii -> p ii { acc with s = (int64 v) }
            | ii -> failwith <| sprintf "Unexpected item: %A" ii
    let items = i.Split "," |> Array.map (fun s -> s.Split "=") |> List.ofArray |> List.map List.ofArray
    p items defaultItem



let parseCmp (cmp:char) : (int64 -> int64 -> bool) =
    match cmp with
    | '<' -> (<)
    | '>' -> (>)
    | _ -> failwith "Unexpected comp!"



let getIdx idx (r:Item) =
    match idx with
        | 'x' -> r.x
        | 'm' -> r.m
        | 'a' -> r.a
        | 's' -> r.s
        | _ -> failwith <| sprintf "Unexpected idx: %A" idx

let parseRule (r:string) =
    let rec workFlow rs x =
        match rs with
            | [] -> failwith "No more rules :("
            | r::rs ->
                match r x with
                    | Some dst -> dst
                    | None -> workFlow rs x
    let parseSubRule (sr:string) =
        if (not (sr.Contains ":")) then (fun _ -> Some sr)
        else
        let idx = sr.[0] |> getIdx
        let cmp = sr.[1] |> parseCmp
        let v,dst = sr.[2..] |> (fun s -> s.Split ":") |> (fun a -> Array.head a, (Array.tail >> Array.head) a)
        (fun x -> if cmp (idx x) (int64 v) then Some dst else None)

        
    let name,rule =
        r.Split "{"
        |> (fun x -> Array.head x, (Array.tail >> Array.head) x |> (fun r -> r.Replace( "}", "")))
        |> (fun (n,r) -> n, r.Split ",")
    let subRules = Array.map parseSubRule rule |> List.ofArray
    name,workFlow subRules


//let rules',items' = readFile "test.txt"
let rules',items' = readFile "input.txt"
let rules = List.map parseRule rules' |> Map.ofList
let items = List.map parseItem items'



let rec applyRule ruleMap rule item =
    match Map.tryFind rule ruleMap with
        | None -> failwith <| sprintf "Rule %s not found!" rule
        | Some rule ->
            match rule item with
                | "A" -> "A"
                | "R" -> "R"
                | dst -> applyRule ruleMap dst item


let sumItem (i:Item) =
    i.x + i.m + i.a + i.s
    
List.filter (fun i -> applyRule rules "in" i = "A") items
|> List.fold (fun acc x -> acc + sumItem x) 0L
|> printfn "Part 1: %A"

// Part 2

type IItem = { x:(int64*int64); m:(int64*int64); a:(int64*int64); s:(int64*int64);}
let defaultIItem = { x = (0L,4000L); m = (0L,4000L); a = (0L,4000L); s = (0L,4000L); }

let splitInterval cmp (l,h) at =
    match cmp with
        | '<' ->
            let low =
                if at <= l then None
                else Some (l, at-1L)
            let high =
                Some (max at l, h)
            low,high
        | '>' ->
            let low =
                Some (l, at)
            let high =
                if at >= h then None
                else Some (at+1L,h)
            low,high



splitInterval '<' (0L,4000L) 1351L
splitInterval '>' (0L,4000L) 1351L

let getIdx' idx (r:IItem) =
    match idx with
        | 'x' -> r.x
        | 'm' -> r.m
        | 'a' -> r.a
        | 's' -> r.s
        | _ -> failwith <| sprintf "Unexpected idx: %A" idx

let setIdx' idx (r:IItem) v =
    match idx with
        | 'x' -> { r with x = v }
        | 'm' -> { r with m = v }
        | 'a' -> { r with a = v }
        | 's' -> { r with s = v }
        | _ -> failwith <| sprintf "Unexpected idx: %A" idx


let splitIItem (cmp:char) (at:int64) idx iitem =
    let v = getIdx' idx iitem
    match splitInterval cmp v at with
        | None,None -> None,None
        | Some l,None ->
            Some (setIdx' idx iitem l),None
        | None, Some h ->
            None, Some (setIdx' idx iitem h)
        | Some l, Some h ->
            Some (setIdx' idx iitem h), Some (setIdx' idx iitem l)


splitIItem '<' 1351L 's' defaultIItem



