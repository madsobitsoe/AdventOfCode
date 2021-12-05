open System.IO

let explode (s:string) : char list = [for c in s do yield c]

let readFile file  =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map explode
    
let data = readFile "input.txt"
let test = readFile "test.txt" 

type color = White | Black
type dir = East | West | NorthEast | NorthWest | SouthEast | SouthWest
type tile = (int * int ) * color
type floor = tile list

let rec parseDirections acc = function
    | [] -> List.rev acc
    | 's'::'e'::xs -> parseDirections (SouthEast::acc) xs
    | 'n'::'e'::xs -> parseDirections (NorthEast::acc) xs    
    | 's'::'w'::xs -> parseDirections (SouthWest::acc) xs
    | 'n'::'w'::xs -> parseDirections (NorthWest::acc) xs    
    | 'w'::xs -> parseDirections (West::acc) xs
    | 'e'::xs -> parseDirections (East::acc) xs
    | _ -> failwith "bad data"

let rec findTile (x,y) = function
    | [] -> (x,y)
    | West::xs ->      findTile (x-2,y) xs
    | East::xs ->      findTile (x+2,y) xs
    | NorthWest::xs -> findTile (x-1,y+1) xs
    | NorthEast::xs -> findTile (x+1,y+1) xs
    | SouthWest::xs -> findTile (x-1,y-1) xs
    | SouthEast::xs -> findTile (x+1,y-1) xs
    

let flip = function
    | (pos,White) -> (pos,Black)
    | (pos,Black) -> (pos,White)

let flipTile pos (tiles:tile list)  =
    match List.tryFind (fun ((tpos,_):tile) -> pos = tpos) tiles with
        | None -> (pos,Black)::tiles
        | Some tile ->
            List.filter ((<>) tile) tiles |> (fun tiles' -> (flip tile)::tiles')

let flipTile' (tilesToFlip:tile list) ((pos,c):tile)   =
    match List.tryFind (fun ((tpos,_):tile) -> pos = tpos) tilesToFlip with
        // tile should not be flipped
        | None -> (pos,c)
        // Tile should be flipped
        | Some tile -> flip tile



let intitalTilesToFlip = List.map (parseDirections [] >> findTile (0,0)) data

let initialTiles = [((0,0),White)]
let flipTiles = List.foldBack flipTile intitalTilesToFlip initialTiles
List.filter (fun (_,color) -> color = Black) flipTiles |> List.length
|> printfn "Solution part 1: %d" 


// Part 2
// Game of life on hexagonal grid, wowza

// The tile floor in the lobby is meant to be a living art exhibit. Every day, the tiles are all flipped according to the following rules:
// Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
// Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.
let addTile pos (tiles:tile list)  =
    match List.tryFind (fun ((tpos,_):tile) -> pos = tpos) tiles with
        | None -> (pos,White)::tiles
        | Some tile -> tiles 

let addNeighbors (((x,y),c):tile) (tiles: floor) : floor =
    // We only need to add neighbors if the tile is black
    match c with
        | White -> tiles
        | Black -> 
            let neighbors =
                [
                    (x-2,y)
                    (x+2,y)
                    (x-1,y-1)
                    (x-1,y+1)
                    (x+1,y-1)
                    (x+1,y+1)
                ]
            List.foldBack addTile neighbors tiles

let getNeighbors (((x,y),_):tile) (tiles: floor) : floor =
    let neighbors =
        [
            (x-2,y)
            (x+2,y)
            (x-1,y-1)
            (x-1,y+1)
            (x+1,y-1)
            (x+1,y+1)
        ]
    List.filter (fun ((x',y'),_) -> List.contains (x',y') neighbors) tiles

let shouldFlip tile tiles =
    let bn = getNeighbors tile tiles |> List.filter (fun (_,c) -> c = Black) 
    match tile with
        | (pos,White) -> List.length bn = 2
        | (pos,Black) -> let len = List.length bn in (len = 0 || len > 2)
            

let rec nextDay n (tiles:Lazy<tile list>) =
    match n with
        | 0 -> tiles.Force()
        | n ->
            // For every tile, add it's neighbors
            let tiles'' = lazy(
                 let tiles' = lazy(List.foldBack addNeighbors (tiles.Force()) (tiles.Force()))
                 // Then collect a list of tiles to be flipped and flip them
                 let toFlip = lazy(List.filter (fun x -> shouldFlip x (tiles'.Force())) (tiles'.Force()))
                 List.map (flipTile' (toFlip.Force())) (tiles'.Force()) )
            nextDay (n-1) tiles''


printfn "Computing part 2. Wait for it... (for a long time)"
nextDay 100 (lazy(flipTiles))
|> List.filter (fun (_,c) -> c = Black)
|> List.length
|> printfn "Solution part 2: %d"

