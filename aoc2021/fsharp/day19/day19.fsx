open System.IO

let explode (s:string) = [for c in s do yield c]
let readFile filename =
    File.ReadAllText filename
    |> (fun (s:string) -> s.Split("\n\n"))
    |> Array.map (fun (s:string) -> s.Split("---\n"))
    |> Array.map ((fun (s:string[]) -> s.[0],s.[1].Split("\n") |> Array.toList))
    |> Array.toList
    |> List.map (fun (name,data) ->
                        name, List.map (fun (s:string) -> s.Split(",")) data
                        |> List.map Array.toList
                        |> List.filter (fun x -> x <> [""])
                        |> List.map (fun (x::y::z::_) -> (int64 x, int64 y, int64 z))
                 )


let test = readFile "test.txt"
let data = readFile "input.txt"

let subv (v1: int64*int64*int64) v2 =
    let x1,y1,z1 = v1
    let x2,y2,z2 = v2
    x1 - x2, y1 - y2, z1 - z2

let manhattanDist (v1: int64*int64*int64) v2 : int64 =
    let x1,y1,z1 = v1
    let x2,y2,z2 = v2
    abs (x2 - x1) + abs (y2-y1) + abs (z2 - z1)

let translate' (vx,vy,vz) (px,py,pz) : (int64 * int64 * int64) = vx+px,vy+py,vz+pz
let translate v xs = List.map (translate' v) xs


let transformations  =
    [
        List.map (fun ((x:int64),(y:int64),(z:int64)) -> (x,y,z));
        List.map (fun (x,y,z) -> (x,-y,-z));
        List.map (fun (x,y,z) -> (x,-z,y));
        List.map (fun (x,y,z) -> (x,z,-y));
        List.map (fun (x,y,z) -> (-x,-z,-y));
        List.map (fun (x,y,z) -> (-x,-y,z));
        List.map (fun (x,y,z) -> (-x,y,-z));
        List.map (fun (x,y,z) -> (-x,z,y));
        List.map (fun (x,y,z) -> (y,z,x));
        List.map (fun (x,y,z) -> (y,-z,-x));
        List.map (fun (x,y,z) -> (y,-x,z));
        List.map (fun (x,y,z) -> (y,x,-z));
        List.map (fun (x,y,z) -> (-y,-x,-z));
        List.map (fun (x,y,z) -> (-y,-z,x));
        List.map (fun (x,y,z) -> (-y,z,-x));
        List.map (fun (x,y,z) -> (-y,x,z));
        List.map (fun (x,y,z) -> (z,x,y));
        List.map (fun (x,y,z) -> (z,-x,-y));
        List.map (fun (x,y,z) -> (z,-y,x));
        List.map (fun (x,y,z) -> (z,y,-x));
        List.map (fun (x,y,z) -> (-z,-y,-x));
        List.map (fun (x,y,z) -> (-z,-x,y));
        List.map (fun (x,y,z) -> (-z,x,-y));
        List.map (fun (x,y,z) -> (-z,y,x));
    ]


let matchingPoints xs ys =
    // For all points in xs, check if they exist in ys
    List.filter (fun x -> List.contains x ys) xs
    |> List.length

// let tryTranslations (xs: (int64 * int64 * int64) list) (ys : (int64 * int64 * int64) list) =
//     let rec helper xs' ys' =
//         match xs',ys' with
//             | [],_ -> None
//             | _::xs',[] -> helper xs' ys
//             | (x'::xs'),y'::ys' -> 
//                 let v = subv x' y'  
//                 let tys = translate v ys
//                 if matchingPoints tys xs >= 12 then
//                     Some (v,tys)
//                 else
//                     helper (x'::xs') ys'
//                 // let valid = List.filter (fun (_,x) -> matchingPoints x xs >= 12) tys
//                 // match valid with
//                 //     | [] -> helper xs'
//                 //     | x::_ -> Some x

//     helper xs ys 


let tryTranslations' (xs: (int64 * int64 * int64) list) (ys : (int64 * int64 * int64) list) =
    let rec helper xs' =
        match xs' with
            | [] -> None
            | (x'::xs') -> 
                let vs = List.map (subv x') ys  
                let tys = List.map (fun x -> x, translate x ys) vs
                let valid = List.filter (fun (_,x) -> matchingPoints x xs >= 12) tys
                match valid with
                    | [] -> helper xs'
                    | x::_ -> Some x

    helper xs
                    

let rec findRot' toMatch ps transforms =
    match transforms with
        | [] -> None
        | tf::tfs ->
            let withT = tf ps
            match tryTranslations toMatch withT with
                | Some (v,vs) ->
                    Some (v,vs)
                | None ->
                    findRot' toMatch ps tfs 

let mapBeacons start unknown =
    let mutable known = start
    let mutable whereTheFuckAreYou = unknown
    let mutable displacements = []
    let mutable lc = 0
    while lc < 42 && not (List.isEmpty whereTheFuckAreYou) do
        let mutable wtfay = []
        printfn "Only %d scanners remaining" (List.length whereTheFuckAreYou)        
        for i = 0 to List.length whereTheFuckAreYou - 1 do
            match findRot' known (snd whereTheFuckAreYou.[i]) transformations with
                | None ->
                    wtfay <- whereTheFuckAreYou.[i] :: wtfay 
                    ()
                | Some (v,vs) ->
                    displacements <- v :: displacements
                    printfn "mapped %s" (fst whereTheFuckAreYou.[i])
                    printfn "Found translation by %A" v
                    known <- known @ vs |> List.distinct

        whereTheFuckAreYou <- wtfay
        lc <- lc + 1
    printfn "Found beacons: %d" (List.length known)
    printfn "Leftovers: %A" (List.length whereTheFuckAreYou)
    let maxdist =
        List.allPairs (List.distinct displacements) (List.distinct displacements)
        |> List.map (fun (a,b) -> manhattanDist a b)|> List.max
    printfn "Largest manhattan distance: %A" maxdist
    known

let t0 = List.head test |> snd
let tt = List.tail test

// Run both parts on test-data
mapBeacons t0 tt
// Solve both parts on actual input data (wait for it.. forever because my code is slow AF)
mapBeacons (List.head data |> snd) (List.tail data |> List.rev)

