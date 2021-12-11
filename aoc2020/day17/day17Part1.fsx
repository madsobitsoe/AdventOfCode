open System.IO
// Don't try this at home.
// You should really use Array3D instead, it's faster by a billion
// Alternatively, don't use F# but something cool like C

type State = Active | Inactive

let toState = function
    | '#' -> Active
    | '.' -> Inactive
    | c -> failwith <| sprintf "Unknown char in input: %c" c
    
let explode s = [for c in s do yield toState c]


let to3dmap xs : (Map<int*int*int,State>)  =
    let map = Map []
    let startCoord = List.length xs / 2 |> (-) 0
    let rec helper xCoord xs map =
        match xs with
        | [] -> map
        | x::xs ->
            List.mapi (fun i x -> i,x) x
            |> List.fold (fun acc (y,elm) ->
                              if elm = Active then
                                  Map.add (xCoord,y,0) elm acc
                              else acc) map
            |> helper (xCoord + 1) xs 
    helper startCoord xs map


let readFile filename =
    File.ReadAllLines filename
    |> Array.toList
    |> List.map explode
    |> to3dmap

let neighbors (x,y,z) =
    seq { for i in x - 1 .. x + 1 do
          for j in y - 1 .. y + 1 do
          for k in z - 1 .. z + 1 do
          if (i,j,k) <> (x,y,z) then yield (i,j,k)
        }

let activeNeighbors pos map =
    neighbors pos |>  Seq.filter (fun x -> Map.containsKey x map && Map.find x map = Active) |> Seq.length

let flipState minmax cubes =
    // We need to grow the field of cubes by 1 in all dimensions
    // Then iterate over each coordinate
    // And flip according to the rules :
    // Active -> flip if <2 or >3 neighbors are active
    // Inactive -> flip if 3 neighbors are active
    let range = [-minmax - 1 .. minmax + 1]
    let mutable map = Map []
    for x in range do
        for y in range do
            for z in range do
                let n = activeNeighbors (x,y,z) cubes                
                if Map.containsKey (x,y,z) cubes then
                    if n = 2 || n = 3 then
                        map <- Map.add (x,y,z) Active map
                else
                    if n = 3 then
                        map <- Map.add (x,y,z) Active map                    
    map


let rec runCycles cycles minmax cubes =
    match cycles with
        | 0 -> cubes
        | n -> flipState minmax cubes |>  runCycles (n-1) (minmax+1)
        
let test = readFile "test.txt"
let data = readFile "input.txt"


runCycles 6 7 data
|> Map.count
|> printfn "Solution part 1: %d"
