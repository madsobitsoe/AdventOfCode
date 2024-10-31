open System.IO

let toTriples : string[] -> int*int*int = function
    | [|x;y;z|] -> int x, int y, int z
    | x -> failwith <| sprintf "Unexpected input: %A" x

let splitter (s:string) =
    s.Split "~"
    |> Array.map (fun s -> s.Split ",")
    |> (fun x -> Array.head x, (Array.tail >> Array.head) x)
    ||> (fun a b -> toTriples a, toTriples b)

let genName' i =
    let iDiv = i / 26
    let iMod = i % 26
    if iDiv = 0 then string ([|'A'..'Z'|].[iMod])
    else string ([|'A'..'Z'|].[iMod]) + string iDiv

let genName = Seq.initInfinite genName' |> Seq.cache


let xz' (x,_,z) = x,z
let yz' (_,y,z) = y,z

let xz = function
    | start,stop -> xz' start, xz' stop
let yz = function
    | start,stop -> yz' start, yz' stop


let readFile file =
    File.ReadAllLines file
    |> Array.map splitter
    |> List.ofArray
    |> List.mapi (fun i x -> Seq.item i genName,x)


let splitZ (input:string * ((int*int)*(int*int))) =
    let (name,((c0,z0),(c1,z1))) = input
    if z0 = z1 then [input]
    else
        List.map (fun z -> (name,((c0,z),(c1,z)))) [z1.. -1 ..z0] 

let view f data =
        data
        |> List.map (fun (n,elm) -> n, f elm)
        |> List.collect splitZ
        |> List.groupBy (snd >> snd >> snd)
        |> List.sortByDescending fst
        |> Map.ofList
        
    
    
let xView = view xz data
let yView = view yz data

let rec drawRow n len row acc =
    if n = len then acc
    else
        match row with
            | [] -> drawRow (n+1) len [] (acc + ".")
            | (name,((x0,_),(x1,_)))::xs ->
                if n < x0 then
                    drawRow (n+1) len row (acc + ".")
                else if n <= x1 then
                    drawRow (n+1) len row (acc + name)
                else
                    drawRow n len xs acc
        
let rec drawView n len data =
    if n = 0 then [0..len-1] |> List.fold (fun acc _ -> acc + "-") "" |> printfn "%s 0"
    else
    match Map.tryFind n data with
        | Some xs ->
            let xs' = List.sortBy (snd >> fst) xs
            drawRow 0 len xs' "" |> printfn "%s %d" <| n
            drawView (n-1) len data
            

        | None ->
            List.iter (fun _ -> printf ".") [1..len]
            printfn " %d" n
            drawView (n-1) len data


xView
yView

let xLen = Map.maxKeyValue xView |> fst
drawView xLen 3 xView

let yLen = Map.maxKeyValue yView |> fst
drawView yLen 3 yView


let data = readFile "test.txt"
let data = readFile "input.txt"

