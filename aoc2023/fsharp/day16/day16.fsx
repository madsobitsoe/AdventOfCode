open System.IO

let explode (s:string) : char list = [for c in s do yield c]

let memoize f =
    let d = new System.Collections.Generic.Dictionary<_,_>()
    let rec g x y =
        match d.TryGetValue ((x,y)) with
            | (true, res) -> res
            | _ ->
                let res = f g x y
                d.Add((x,y), res)
                res
    g


let readFile file =
    File.ReadAllLines file
    |> Array.map explode
    |> List.ofArray

let dataToMap xs =
    xs
    |> List.mapi (fun i x -> List.mapi (fun j y -> (i,j),y) x)
    |> List.concat
    |> Map.ofList




//let data' = readFile "test.txt"
let data' = readFile "input.txt"

let data = dataToMap data'

type Direction =
    | Up
    | Down
    | Left
    | Right

let nextCoord dir (i,j) =
        match dir with
        | Up -> (i-1,j)
        | Down -> (i+1,j)
        | Left -> (i,j-1)
        | Right -> (i,j+1)


let rec visit' f dir coord map visited =
    if Set.contains (dir,coord) visited then visited
    else
        let visited' = Set.add (dir,coord) visited        
        let next = nextCoord dir coord
        match Map.tryFind coord map with
        | None -> visited
        | Some c ->
            match c with
                | '.' ->                   
                    f dir (nextCoord dir coord) map visited'
                | '/' ->
                    match dir with
                        | Right ->
                            f Up (nextCoord Up coord) map visited'
                        | Left ->
                            f Down (nextCoord Down coord) map visited'
                        | Up ->
                            f Right (nextCoord Right coord) map visited'
                        | Down ->
                            f Left (nextCoord Left coord) map visited'
                | '\\'  ->
                    match dir with
                        | Right ->
                            f Down (nextCoord Down coord) map visited'
                        | Left ->
                            f Up (nextCoord Up coord) map visited'
                        | Down ->
                            f Right (nextCoord Right coord) map visited'
                        | Up ->
                            f Left (nextCoord Left coord) map visited'
                | '-' ->
                    match dir with
                        | Down | Up ->
                            let l = f Left (nextCoord Left coord) map visited'
                            f Right (nextCoord Right coord) map l
                        | _ -> f dir (nextCoord dir coord) map visited'
                | '|' ->
                    match dir with
                        | Left | Right ->
                            let u = f Up (nextCoord Up coord) map visited'
                            f Down (nextCoord Down coord) map u
                        | _ -> f dir (nextCoord dir coord) map visited'
                | _ -> failwith "yup"


let visit = memoize visit'


visit Right (0,0) data (set [])
|> Set.map snd
|> Set.count
|> printfn "Part 1: %d"


// Part 2
let len1 = List.length data' - 1
let len2 = (List.head data' |> List.length) - 1
let starts =
    let r = [0..len1] |> List.map (fun x -> Right, (0,x))
    let l = [0..len1] |> List.map (fun x -> Left, (x,len2))
    let d = [0..len2] |> List.map (fun x -> Down, (0,x))
    let u = [0..len2] |> List.map (fun x -> Up, (len2,x))
    r @ l @ d @ u

List.map (fun (dir,coord) -> visit dir coord data (set [])) starts
|> List.map (Set.map snd >> Set.count)
|> List.max
|> printfn "Part 2: %d"
