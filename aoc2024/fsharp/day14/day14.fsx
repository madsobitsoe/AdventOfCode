open System.IO
open System.Text.RegularExpressions

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0L
   if System.Int64.TryParse(str, &intvalue) then Some(intvalue)
   else None

let getInt = function
    | Integer i -> i
    | x -> failwith <| sprintf "Bad data: %A" x

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|Line|_|) = function
    | ParseRegex "p=(\d+),(\d+) v=(-?\d+),(-?\d+)" [Integer px; Integer py; Integer vx; Integer vy] -> Some (px,py,vx,vy)
    | _ -> None

let getLine = function
    | Line (px,py,vx,vy) -> (px,py),(vx,vy)
    | x -> failwith <| sprintf "Bad data: %A" x


let readFile file =
    File.ReadAllText file
    |> (fun s -> s.Split "\n")
    |> List.ofArray
    |> List.filter ((<>) "")
    |> List.map getLine


// Test input
//let data = readFile "test.txt"
// let maxx,maxy = 11L,7L

// Real input
let data = readFile "input.txt"
let maxx,maxy = 101L,103L

let step' (maxx:int64) (maxy:int64) ((px:int64,py:int64),(vx:int64,vy:int64)) =
    let px' =
        // Moving left
        if vx < 0L then
            if px + vx < 0L then
                maxx + (px + vx)
            else
                px + vx
        // Moving right
        else
            if px + vx >= maxx then
                (px+vx) - (maxx )
            else
                px+vx

    let py' =
        // Moving up
        if vy < 0 then
            if py + vy < 0L then
                maxy + (py + vy)
            else
                py + vy
        // Moving right
        else
            if py + vy >= maxy then
                (py+vy) - (maxy )
            else
                py+vy
    (px',py'),(vx,vy)

let step = step' maxx maxy

let stepMany data count =
    [1..count]
    |> List.fold (fun acc _ -> List.map step acc) data

let findQuadrants maxx maxy =
    let mx = maxx / 2L
    let my = maxy / 2L
    let tl = (0L,0L),(mx-1L,my-1L)
    let tr = (mx+1L,0L),(maxx,my-1L)
    let bl = (0L,my+1L),(mx-1L,maxy)
    let br = (mx+1L,my+1L),(maxx,maxy)
    tl,tr,bl,br

let part1 maxx maxy data =
    let sim = stepMany data 100
    let tl,tr,bl,br = findQuadrants maxx maxy
    let (tlMinX,tlMinY),(tlMaxX,tlMaxY) = tl
    let topLeftRobots = List.filter (fun ((px,py),_) -> px >= tlMinX && px <= tlMaxX && py >= tlMinY && py <= tlMaxY) sim

    let (trMinX,trMinY),(trMaxX,trMaxY) = tr
    let topRightRobots = List.filter (fun ((px,py),_) -> px >= trMinX && px <= trMaxX && py >= trMinY && py <= trMaxY) sim

    let (blMinX,blMinY),(blMaxX,blMaxY) = bl
    let bottomLeftRobots = List.filter (fun ((px,py),_) -> px >= blMinX && px <= blMaxX && py >= blMinY && py <= blMaxY) sim
    let (brMinX,brMinY),(brMaxX,brMaxY) = br
    let bottomRightRobots = List.filter (fun ((px,py),_) -> px >= brMinX && px <= brMaxX && py >= brMinY && py <= brMaxY) sim

    [topLeftRobots; topRightRobots; bottomLeftRobots; bottomRightRobots]
    |> List.map List.length
    |> List.fold (fun acc x -> acc * x) 1
    

part1 maxx maxy data
|> printfn "Part 1: %d"


let visualize (maxx:int64) (maxy:int64) (data: ((int64*int64)*(int64*int64)) list) =
    let robots = data |> Map.ofList
    for y = 1 to int maxy do
        for x = 1 to int maxx do
            match Map.tryFind (int64 x, int64 y) robots with
                | None -> printf " "
                | Some _ -> printf "#"
        printfn ""

// When no robots overlap, a tree is drawn
let rec part2 maxx maxy (data: ((int64*int64)*(int64*int64)) list) acc =
    let data' = List.map step data
    let dataMapSize = Map.ofList data' |> Map.count
    if List.length data' = dataMapSize then
        visualize maxx maxy data' |> ignore
        acc
    else
        part2 maxx maxy data' (acc+1)
    
     
part2 maxx maxy data 1
|> printfn "Part 2: %d"

