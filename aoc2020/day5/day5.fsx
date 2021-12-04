open System.IO

let explode (s:string) : char list = [for c in s do yield c]

let readFile file  =
    File.ReadAllLines file
    |> List.ofArray

let rec getRow rl ru = function
    | 'F'::[] ->
        rl - 1
    | 'B'::[] ->
        ru - 1
    | 'F'::xs ->
        let range = ru - rl        
        getRow rl (rl + range / 2) xs
    | 'B'::xs ->
        let range = ru - rl        
        getRow (rl + range / 2 + 1) ru xs
    | _ -> failwith "bad data"

let rec getSeat sl su = function
    | 'R'::[] -> su - 1
    | 'L'::[] -> sl - 1
    | 'R'::xs ->
        let range = su - sl
        getSeat (sl + range / 2 + 1) su xs
    | 'L'::xs ->
        let range = su - sl
        getSeat sl (sl + range / 2) xs
    | _ -> failwith "bad data"
    
let getRowAndSeat boardingPass =
    let rowPart = List.take 7 boardingPass
    let seatPart = List.skip 7 boardingPass
    let row = getRow 1 128 rowPart
    let seat = getSeat 1 8 seatPart
    row,seat,row * 8 + seat
    
let getSeatId (_,_,id) = id
let idmin (s1, s2) = min (getSeatId s1) (getSeatId s2)
let findNeighborIds ((_,_,id1),(_,_,id2)) = abs (id1 - id2) |> (=) 2
let data = readFile "input.txt"

List.map (explode >> getRowAndSeat >> getSeatId) data
|> List.max
|> printfn "Part 1 solution: %d"


List.map (explode >> getRowAndSeat) data
|> List.sortBy (fun (_,_,id) -> id)
|> List.pairwise
|> List.filter findNeighborIds
|> List.map idmin
|> List.head
|> (+) 1
|> printfn "Solution part 2: %d"
