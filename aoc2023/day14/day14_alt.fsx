open System.IO


let explode (s:string) : char list = [for c in s do yield c]

let to2dArray (xs : char list list) =
    let l1 = List.length xs
    let l2 = List.head xs |> List.length
    let map = List.mapi (fun i x -> List.mapi (fun j y -> (i,j),y) x) xs |> List.concat |> Map.ofList
    Array2D.init l1 l2 (fun i j -> Map.find (i,j) map)
    
let readFile file =
    File.ReadAllLines file
    |> Array.map explode
    |> List.ofArray
    |> to2dArray

//let origData = readFile "test.txt"
let origData = readFile "input.txt"

let part1Data = Array2D.copy origData
let part2Data = Array2D.copy origData


let slideNorth (data:char array2d) =
    let slideColumnNorth data col =
        let mutable lastFree = 0
        for i = 0 to Array2D.length2 data - 1 do
            match data.[i,col] with
                | 'O' ->
                    if lastFree < i then
                        data.[lastFree,col] <- 'O'
                        data.[i,col] <- '.'
                    lastFree <- lastFree + 1                    
                | '#' -> lastFree <- i+1
                | '.' -> ()
            
    // For each column
    for i = 0 to Array2D.length1 data - 1 do
        slideColumnNorth data i
    data

let slideWest (data:char array2d) =
    let slideColumnWest data row =
        let mutable lastFree = 0
        for i = 0 to Array2D.length2 data - 1 do
            match data.[row,i] with
                | 'O' ->
                    if lastFree < i then
                        data.[row,lastFree] <- 'O'
                        data.[row,i] <- '.'
                    lastFree <- lastFree + 1                    
                | '#' -> lastFree <- i+1
                | '.' -> ()
            
    // For each column
    for i = 0 to Array2D.length1 data - 1 do
        slideColumnWest data i
    data

let slideSouth (data:char array2d) =
    let slideColumnSouth data col =
        let mutable lastFree = Array2D.length2 data - 1
        for i = Array2D.length2 data - 1 downto 0 do
            match data.[i,col] with
                | 'O' ->
                    if lastFree > i then
                        data.[lastFree,col] <- 'O'
                        data.[i,col] <- '.'
                    lastFree <- lastFree - 1
                | '#' -> lastFree <- i-1
                | '.' -> ()
            
    // For each column
    for i = 0 to Array2D.length1 data - 1 do
        slideColumnSouth data i
    data

let slideEast (data:char array2d) =
    let slideColumnEast data row =
        let mutable lastFree = Array2D.length1 data - 1
        for i = Array2D.length1 data - 1 downto 0 do
            match data.[row,i] with
                | 'O' ->
                    if lastFree > i then
                        data.[row,lastFree] <- 'O'
                        data.[row,i] <- '.'
                    lastFree <- lastFree - 1
                | '#' -> lastFree <- i-1
                | '.' -> ()
            
    // For each column
    for i = 0 to Array2D.length1 data - 1 do
        slideColumnEast data i
    data


let calculateLoad (data: char array2d) =
    let rows = Array2D.length1 data
    let cols = Array2D.length2 data
    let mutable sum = 0
    for i = 0 to rows - 1 do
        let mutable count = 0
        for j = 0 to cols - 1 do
            if data[i,j] = 'O' then count <- count + 1

        sum <- sum + (rows - i) * count
    sum


part1Data
|> slideNorth
|> calculateLoad
|> printfn "Part 1: %d"


// Part 2
let cycle data =
    data
    |> slideNorth
    |> slideWest
    |> slideSouth
    |> slideEast
    |> (fun x -> calculateLoad x,x)

let samples data =
    (calculateLoad data, data)
    |> Seq.unfold (fun (c,d) ->
                   let load = calculateLoad d
                   let next = cycle d
                   Some ((load),next))
    |> Seq.mapi (fun i x -> i,x)
    |> Seq.cache


let cs s1 s2 =
    Seq.compareWith (fun elem1 elem2 ->
                          if elem1 > elem2 then 1
                          else if elem1 < elem2 then -1
                          else 0) s1 s2

let seqEq s1 s2 = cs s1 s2 = 0
        
let rec detectCycle skip n (data: seq<int * int>) =
    if skip = 0 then printfn "Looking for cycle of size %d" n
    let xs = Seq.skip skip data
    if skip = n-1 then None
    else
    let h = Seq.take n xs
    let h' = Seq.map snd h
    let t =
        Seq.fold (fun (acc,cs) _ -> (Seq.take n cs)::acc, Seq.skip n cs) ([],xs) [1..n]
        |> fst
        |> Seq.map (Seq.map snd)
    if Seq.forall (seqEq h') t then
        printfn "Found cycle:\n%A" h
        for x in h do printf "%A " x
        Some h
    else
        match detectCycle (skip + 1) n data with
        | None -> detectCycle 0 (n+1) data
        | x -> x



// samples part2Data |> Seq.take 1000

let sample = samples part2Data
sample
|> detectCycle 0 154
|> Option.get
|> (fun x -> Seq.head x |> fst, Seq.last x |> fst)
|> (fun (start,stop) -> (1000000000) % (stop) + stop + 1)
|> (fun x -> printfn "i: %d" (x - 236); x)
|> Seq.item <| sample

|> printfn "Part 2: %A"
