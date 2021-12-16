open System.IO

let explode (s:string) = [for c in s do yield c]
let parseEntry (s:string) : ((char * char) * char) =
    let pair::newElm::_ = s.Split(" -> ") |> Array.toList 
    let a,b = pair.[0],pair.[1]
    (a,b),newElm.[0]


let readFile filename =
    File.ReadAllText filename
    |> (fun (s:string) -> s.Split("\n\n"))
    |> Array.toList
    |> (fun (x::y::_) -> x, y.Split("\n"))
    |> (fun (x,y) -> x, Array.toList y |> List.filter ((<>) ""))
    |> (fun (x,y) -> x,List.map parseEntry y)
    |> (fun (x,y) -> explode x,Map.ofList y)


let test = readFile "test.txt"
let data = readFile "input.txt"

let start,m = data
let addLast = List.last start

let addPair pair (c:int64) map =
    match Map.tryFind pair map with
        | None -> Map.add pair c map
        | Some c' -> Map.add pair (c' + c) map

let pairToPairs (map:Map<char*char,char>) pairs acc pair (c:int64) =
    let a,b = pair
    let n = Map.find pair map
    addPair (a,n) c acc
    |> addPair (n,b) c

let rec step (map: Map<char*char,char>) cp =
    Map.fold (pairToPairs map cp) (Map []) cp
    
let steps (map: Map<char*char,char>) start n =
    let mutable pairs = List.pairwise start |> List.countBy id |> List.map (fun (x,c) -> x,int64 c) |> Map
    for i = 1 to n do
        pairs <- step map pairs
    pairs


let rec join (acc:(char * int64) list) = function
    | [] -> acc
    | [x] -> x::acc
    | x::y::xs ->
        let a,c = x
        let b,c' = y
        if a = b then join acc ((b,c+c')::xs)
        else join (x::acc) (y::xs)




steps m start 10
|> Map.toList
|> List.map (fun ((a,_),c) -> a,c)
|> List.append [(addLast, 1L)]
|> List.sortBy fst
|> join []
|> List.map (fun (_,c) -> c)
|> (fun x -> List.max x - List.min x)
|> printfn "Solution part 1: %d"

steps m start 40
|> Map.toList
|> List.map (fun ((a,_),c) -> a,c)
|> List.append [(addLast, 1L)]
|> List.sortBy fst
|> join []
|> List.map (fun (_,c) -> c)
|> (fun x -> List.max x - List.min x)
|> printfn "Solution part 2: %d"

