#time
open System.IO
open System.Text.RegularExpressions

let explode (s:string) = [for c in s do yield c]

let readFile file =
    File.ReadAllText file
    |> fun s -> s.Split "\n\n"
    |> List.ofArray


type Entity = Robot | Wall | Box | LeftBox | RightBox

let toEntity = function
    | '#' -> Some Wall
    | 'O' -> Some Box
    | '@' -> Some Robot
    | '[' -> Some LeftBox
    | ']' -> Some RightBox
    | x -> None

let parseMap (data:string) =
    data.Split "\n"
    |> Array.filter ((<>) "")
    |> List.ofArray
    |> List.map explode
    |> List.mapi (fun j x -> List.mapi (fun i y -> (i,j),toEntity y) x)
    |> List.concat
    |> List.filter (fun (pos,e) -> Option.isSome e)
    |> List.map (fun (pos,e) -> pos, Option.get e)
    |> Map.ofList


type Move = Up | Down | Left | Right
let toMove = function
    | '^' -> Up
    | 'v' -> Down
    | '<' -> Left
    | '>' -> Right
    | x -> failwith <| sprintf "Bad move: %A" x

let parseMoves data =
    explode data
    |> List.filter ((<>) '\n')
    |> List.map toMove
    

let parseData data =
    match data with
        | map::moves::_ ->
            parseMap map, parseMoves moves
        | x -> failwith <| sprintf "Bad data: %A" x
        
//let data = readFile "test.txt"
//let data = readFile "test2.txt"
let data = readFile "input.txt"
let map,moves = parseData data

let nextPos (i,j) dir =
    match dir with
        | Up -> i,j-1
        | Down -> i,j+1
        | Left -> i-1,j
        | Right -> i+1,j


let rec findNextEmptySpace map pos dir =
    let next' = nextPos pos dir
    match Map.tryFind next' map with
        | None -> Some next'
        | Some Wall -> None
        | Some Box -> findNextEmptySpace map next' dir
        | Some Robot -> failwith <| sprintf "Robot tried to push into itself at %A" next'
        | Some LeftBox | Some RightBox -> failwith <| sprintf "[ and ] only exist in part 2!"


let rec attemptMoveDown map visited toVisit acc =
    match toVisit with
        | [] -> if acc = [] then None else Some acc
        | x::xs ->
            if Set.contains x visited then attemptMoveDown map visited xs acc
            else
                let down = nextPos x Down
                match Map.tryFind down map with
                    | None -> attemptMoveDown map (Set.add x visited) xs (x::acc)
                    | Some Wall -> None
                    | Some Robot -> failwith <| sprintf "Found unexpected robot at %A" down
                    | Some Box -> failwith <| sprintf "Found unexpected Box at %A" down
                    | Some LeftBox ->
                        let downRight = nextPos down Right
                        attemptMoveDown map (Set.add x visited) (down::downRight::xs) (x::acc)
                    | Some RightBox ->
                        let downLeft = nextPos down Left
                        attemptMoveDown map (Set.add x visited) (down::downLeft::xs) (x::acc)

let rec attemptMoveUp map visited toVisit acc =
    match toVisit with
        | [] -> if acc = [] then None else Some acc
        | x::xs ->
            if Set.contains x visited then attemptMoveUp map visited xs acc
            else
                let up = nextPos x Up
                match Map.tryFind up map with
                    | None -> attemptMoveUp map (Set.add x visited) xs (x::acc)
                    | Some Wall -> None
                    | Some Robot -> failwith <| sprintf "Found unexpected robot at %A" up
                    | Some Box -> failwith <| sprintf "Found unexpected Box at %A" up
                    | Some LeftBox ->
                        let upRight = nextPos up Right
                        attemptMoveUp map (Set.add x visited) (up::upRight::xs) (x::acc)
                    | Some RightBox ->
                        let upLeft = nextPos up Left
                        attemptMoveUp map (Set.add x visited) (up::upLeft::xs) (x::acc)

let rec moveLeftBox map pos dir =
    let nextPosition = nextPos pos dir
    match dir with
        | Right ->
            // Next pos should be a right box
            match Map.tryFind nextPosition map with
                | Some RightBox ->
                    let next = nextPos nextPosition dir
                    match Map.tryFind next map with
                        | None -> Some (pos::nextPosition::[])
                        | Some RightBox -> failwith <| sprintf "Unexpected RightBox at %A" next
                        | Some Box -> failwith <| sprintf "Unexpected Box at %A" next
                        | Some Robot -> failwith <| sprintf "Unexpected Box at %A" next                       
                        | Some Wall -> None
                        | Some LeftBox ->
                            match moveLeftBox map next dir with
                                | None -> None
                                | Some stuffToMove -> Some (pos::nextPosition::stuffToMove)
                | x -> failwith <| sprintf "Expecting RightBox at %A, found %A" nextPosition x
        | Left -> failwith <| sprintf "%A not implemented" dir
        | Up ->
            let rightBoxPos = nextPos pos Right
            attemptMoveUp map Set.empty (pos::rightBoxPos::[]) []
        | Down ->
            // Check if this box can move down and if the rightbox next to it can move down            
            let rightBoxPos = nextPos pos Right
            attemptMoveDown map Set.empty (pos::rightBoxPos::[]) []

and moveRightBox map pos dir =
    let nextPosition = nextPos pos dir
    match dir with
        | Left ->
            // Next pos should be a LeftBox
            match Map.tryFind nextPosition map with
                | Some LeftBox ->
                    let next = nextPos nextPosition dir
                    match Map.tryFind next map with
                        | None -> Some (pos::nextPosition::[])
                        | Some LeftBox -> failwith <| sprintf "Unexpected LeftBox at %A" next
                        | Some Box -> failwith <| sprintf "Unexpected Box at %A" next
                        | Some Robot -> failwith <| sprintf "Unexpected Robot at %A" next
                        | Some Wall -> None
                        | Some RightBox ->
                            match moveRightBox map next dir with
                                | None -> None
                                | Some stuffToMove -> Some (pos::nextPosition::stuffToMove)
                | x -> failwith <| sprintf "Expecting LeftBox at %A, found %A" nextPosition x

        | Right -> failwith <| sprintf "%A not implemented" dir
        | Up ->
            let leftBoxPos = nextPos pos Left
            attemptMoveUp map Set.empty (pos::leftBoxPos::[]) []
        | Down ->
            let leftBoxPos = nextPos pos Left
            attemptMoveDown map Set.empty (pos::leftBoxPos::[]) []


let attemptMove map robotPos nextMove =
    match Map.tryFind robotPos map with
        | None -> failwith <| sprintf "Robot not found at pos: %A" robotPos
        | Some entity when entity <> Robot ->
            failwith <| sprintf "Found %A instead of robot at %A" entity robotPos
        | Some Robot ->
            let nextPosition = nextPos robotPos nextMove
            match Map.tryFind nextPosition map with
                | None ->
                    // Move the robot into the empty space
                    let map' = Map.remove robotPos map
                    let map'' = Map.add nextPosition Robot map'
                    map'',nextPosition
                | Some Robot -> failwith <| sprintf "Pushing into robot?"
                | Some Wall -> map,robotPos
                | Some Box ->
                    // Figure out if we can move the box, and if we can, move it
                    match findNextEmptySpace map nextPosition nextMove with
                        | None -> map,robotPos
                        | Some pos' ->
                            let map' = Map.remove nextPosition map |> Map.remove robotPos
                            let map'' = Map.add pos' Box map' |> Map.add nextPosition Robot
                            map'',nextPosition
                | Some LeftBox ->
                    match moveLeftBox map nextPosition nextMove with
                        | None -> map,robotPos
                        | Some stuffToMove ->
                            let stuffToMove' = robotPos::stuffToMove
                            let movedStuff = List.map (fun k -> nextPos k nextMove, Map.find k map) stuffToMove'
                            let map' = List.fold (fun acc k -> Map.remove k acc) map stuffToMove'
                            let map'' = List.fold (fun acc (k,v) -> Map.add k v acc) map' movedStuff
                            map'',nextPosition
                            
                | Some RightBox ->
                    match moveRightBox map nextPosition nextMove with
                        | None -> map,robotPos
                        | Some stuffToMove ->
                            let stuffToMove' = robotPos::stuffToMove
                            let movedStuff = List.map (fun k -> nextPos k nextMove, Map.find k map) stuffToMove'
                            let map' = List.fold (fun acc k -> Map.remove k acc) map stuffToMove'
                            let map'' = List.fold (fun acc (k,v) -> Map.add k v acc) map' movedStuff
                            map'',nextPosition


let robotStart = Map.findKey (fun k v -> v = Robot) map
let final = List.fold (fun (map,robotPos) move -> attemptMove map robotPos move) (map,robotStart) moves

let score map =
    Map.filter (fun k v -> v = Box) map
    |> Map.keys |> List.ofSeq
    |> List.fold (fun acc (x,y) -> acc + 100UL * (uint64 y) + (uint64 x)) 0UL

score (fst final)
|> printfn "Part 1: %d"



// Part 2
let transform = function
    | '#' -> "##"
    | 'O' -> "[]"
    | '.' -> ".."
    | '@' -> "@."
    | '\n' -> "\n"
    | x -> failwith <| sprintf "Bad data: %A" x

let transformedInputMap =
    List.head data
    |> explode
    |> List.map transform
    |> List.reduce (+)
    |> parseMap

let printer = function
    | None -> printf "."
    | Some Wall -> printf "#"
    | Some LeftBox -> printf "["
    | Some RightBox -> printf "]"
    | Some Robot -> printf "@"

let visualize map =
    let maxx,maxy = Map.fold (fun (maxx,maxy) (kx,ky) _ -> max maxx kx, max maxy ky) (0,0) map
    let coords = [for x in [0..maxx] do for y in [0..maxy] do yield (x,y)]
    printfn ""
    for y in [0..maxy] do
        for x in [0..maxx] do
            Map.tryFind (x,y) map |> printer
        printfn ""

let score2 map =
    Map.filter (fun _ v -> v = LeftBox) map
    |> Map.keys |> List.ofSeq
    |> List.fold (fun acc (x,y) -> acc + 100UL * (uint64 y) + (uint64 x)) 0UL


let robotStart2 = Map.findKey (fun _ v -> v = Robot) transformedInputMap
let final2 = List.fold (fun (map,robotPos) move -> attemptMove map robotPos move) (transformedInputMap,robotStart2) moves
// let final2 = List.fold (fun (map,robotPos) move -> visualize map; printfn "%A" move; attemptMove map robotPos move) (transformedInputMap,robotStart2) moves

score2 (fst final2)
|> printfn "Part 2: %d"


