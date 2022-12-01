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
// A single scanner but with different rotations
// All observed beacons are the same
let test2 = readFile "test2.txt"

let data = readFile "input.txt"

let subv (v1: int64*int64*int64) v2 =
    let x1,y1,z1 = v1
    let x2,y2,z2 = v2
    x1 - x2, y1 - y2, z1 - z2

let getRegion coords =
    let xs = List.map (fun (x,_,_) -> x) coords
    let ys = List.map (fun (_,y,_) -> y) coords
    let zs = List.map (fun (_,_,z) -> z) coords
    let minx = List.min xs
    let maxx = List.max xs
    let miny = List.min ys
    let maxy = List.max ys
    let minz = List.min zs
    let maxz = List.max zs
    (minx,miny,minz),(maxx,maxy,maxz)
    
let metaData' (name,coords) =
    printfn "%s\nNumber of coords: %d" name (List.length coords)
    let min,max = getRegion coords
    printfn "Min: %A\nMax: %A" min max
    
let metaData xs = List.iter metaData' xs

let translate' (vx,vy,vz) (px,py,pz) : (int64 * int64 * int64) = vx+px,vy+py,vz+pz
let translate v xs = List.map (translate' v) xs

let rotatex' (px,py,pz) = (px, -1L * pz, py)
let rotateX ps = List.map rotatex' ps
let rotatey' (px,py,pz) = (pz, py, -1L * px)
let rotateY ps = List.map rotatey' ps
let rotatez'  (px,py,pz) = (-1L * py, px, pz)
let rotateZ ps = List.map rotatez' ps

let flipx' (px,py,pz) = -1L * px, py, pz
let flipX ps = List.map flipx' ps
let flipy' (px,py,pz) = px, -1L * py, pz
let flipY ps = List.map flipy' ps
let flipz' (px,py,pz) = px,py,-1L * pz
let flipZ ps = List.map flipz' ps


let rotate' : ((int64 * int64 * int64) -> (int64 * int64 * int64)) = rotatex' >> rotatey' >> rotatez'
let rotate = List.map rotate'
let rotate2 = rotate >> rotate
let rotate3 = rotate >> rotate >> rotate

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

// let transformations =
//     [
//         id
//         flipX
//         flipY
//         flipZ
//         flipX >> flipY
//         flipX >> flipZ
//         flipX >> flipY >> flipZ
//         flipY >> flipZ
//         rotate
//         rotate2
//         rotate3
//         flipX >> rotate
//         flipY >> rotate
//         flipZ >> rotate
//         flipX >> flipY >> rotate
//         flipX >> flipZ >> rotate
//         flipX >> flipY >> flipZ >> rotate
//         flipY >> flipZ >> rotate
//         flipX >> rotate2
//         flipY >> rotate2
//         flipZ >> rotate2
//         flipX >> flipY >> rotate2
//         flipX >> flipZ >> rotate2
//         flipX >> flipY >> flipZ >> rotate2
//         flipY >> flipZ >> rotate2
//         flipX  >> rotate3
//         flipY >> rotate3
//         flipZ >> rotate3
//         flipX >> flipY >> rotate3
//         flipX >> flipZ >> rotate3
//         flipX >> flipY >> flipZ >> rotate3
//         flipY >> flipZ >> rotate3
//      // ] 
// // // Possible transformations
// // let transformations =
// //     [id // no transformation
// //      flipX
// //      flipY
// //      flipZ
// //      flipX >> flipY
// //      flipX >> flipZ
// //      flipX >> flipY >> flipZ
// //      flipY >> flipZ
//         rotateX
//         rotateX >> rotateX
//         rotateX >> rotateX >> rotateX
//         rotateY
//         rotateY >> rotateY
//         rotateY >> rotateY >> rotateY     
//         rotateZ
//         rotateZ >> rotateZ
//         rotateZ >> rotateZ >> rotateZ     
//         rotateX >> rotateY
//         rotateX >> rotateZ
//         rotateX >> rotateY >> rotateZ
//         rotateY >> rotateX
//         rotateY >> rotateZ
//         rotateY >> rotateX >> rotateZ
//         rotateY >> rotateZ >> rotateY
//         rotateZ >> rotateY
//         rotateZ >> rotateX
//         rotateZ >> rotateX >> rotateY
//         rotateZ >> rotateZ >> rotateX
//         rotateX
//         rotateX >> rotateX
//         rotateX >> rotateX >> rotateX
//         rotateY
//         rotateY >> rotateY
//         rotateY >> rotateY >> rotateY     
//         rotateZ
//         rotateZ >> rotateZ
//         rotateZ >> rotateZ >> rotateZ     
//         rotateX >> rotateY
//         rotateX >> rotateZ
//         rotateX >> rotateY >> rotateZ
//         rotateY >> rotateX
//         rotateY >> rotateZ
//         rotateY >> rotateX >> rotateZ
//         rotateY >> rotateZ >> rotateY
//         rotateZ >> rotateY
//         rotateZ >> rotateX
//         rotateZ >> rotateX >> rotateY
//         rotateZ >> rotateZ >> rotateX
//         flipX >> rotateX
//         flipX >> rotateX >> rotateX
//         flipX >> rotateX >> rotateX >> rotateX
//         flipX >> rotateY
//         flipX >> rotateY >> rotateY
//         flipX >> rotateY >> rotateY >> rotateY     
//         flipX >> rotateZ
//         flipX >> rotateZ >> rotateZ
//         flipX >> rotateZ >> rotateZ >> rotateZ     
//         flipX >> rotateX >> rotateY
//         flipX >> rotateX >> rotateZ
//         flipX >> rotateX >> rotateY >> rotateZ
//         flipX >> rotateY >> rotateX
//         flipX >> rotateY >> rotateZ
//         flipX >> rotateY >> rotateX >> rotateZ
//         flipX >> rotateY >> rotateZ >> rotateY
//         flipX >> rotateZ >> rotateY
//         flipX >> rotateZ >> rotateX
//         flipX >> rotateZ >> rotateX >> rotateY
//         flipX >> rotateZ >> rotateZ >> rotateX
//         flipY >> rotateX
//         flipY >> rotateX >> rotateX
//         flipY >> rotateX >> rotateX >> rotateX
//         flipY >> rotateY
//         flipY >> rotateY >> rotateY
//         flipY >> rotateY >> rotateY >> rotateY     
//         flipY >> rotateZ
//         flipY >> rotateZ >> rotateZ
//         flipY >> rotateZ >> rotateZ >> rotateZ     
//         flipY >> rotateX >> rotateY
//         flipY >> rotateX >> rotateZ
//         flipY >> rotateX >> rotateY >> rotateZ
//         flipY >> rotateY >> rotateX
//         flipY >> rotateY >> rotateZ
//         flipY >> rotateY >> rotateX >> rotateZ
//         flipY >> rotateY >> rotateZ >> rotateY
//         flipY >> rotateZ >> rotateY
//         flipY >> rotateZ >> rotateX
//         flipY >> rotateZ >> rotateX >> rotateY
//         flipY >> rotateZ >> rotateZ >> rotateX
//         flipZ >> rotateX
//         flipZ >> rotateX >> rotateX
//         flipZ >> rotateX >> rotateX >> rotateX
//         flipZ >> rotateY
//         flipZ >> rotateY >> rotateY
//         flipZ >> rotateY >> rotateY >> rotateY     
//         flipZ >> rotateZ
//         flipZ >> rotateZ >> rotateZ
//         flipZ >> rotateZ >> rotateZ >> rotateZ     
//         flipZ >> rotateX >> rotateY
//         flipZ >> rotateX >> rotateZ
//         flipZ >> rotateX >> rotateY >> rotateZ
//         flipZ >> rotateY >> rotateX
//         flipZ >> rotateY >> rotateZ
//         flipZ >> rotateY >> rotateX >> rotateZ
//         flipZ >> rotateY >> rotateZ >> rotateY
//         flipZ >> rotateZ >> rotateY
//         flipZ >> rotateZ >> rotateX
//         flipZ >> rotateZ >> rotateX >> rotateY
//         flipZ >> rotateZ >> rotateZ >> rotateX
//         flipX >> flipY >> rotateX
//         flipX >> flipY >> rotateX >> rotateX
//         flipX >> flipY >> rotateX >> rotateX >> rotateX
//         flipX >> flipY >> rotateY
//         flipX >> flipY >> rotateY >> rotateY
//         flipX >> flipY >> rotateY >> rotateY >> rotateY     
//         flipX >> flipY >> rotateZ
//         flipX >> flipY >> rotateZ >> rotateZ
//         flipX >> flipY >> rotateZ >> rotateZ >> rotateZ     
//         flipX >> flipY >> rotateX >> rotateY
//         flipX >> flipY >> rotateX >> rotateZ
//         flipX >> flipY >> rotateX >> rotateY >> rotateZ
//         flipX >> flipY >> rotateY >> rotateX
//         flipX >> flipY >> rotateY >> rotateZ
//         flipX >> flipY >> rotateY >> rotateX >> rotateZ
//         flipX >> flipY >> rotateY >> rotateZ >> rotateY
//         flipX >> flipY >> rotateZ >> rotateY
//         flipX >> flipY >> rotateZ >> rotateX
//         flipX >> flipY >> rotateZ >> rotateX >> rotateY
//         flipX >> flipY >> rotateZ >> rotateZ >> rotateX
//         flipZ >> flipZ >> rotateX
//         flipZ >> flipZ >> rotateX >> rotateX
//         flipZ >> flipZ >> rotateX >> rotateX >> rotateX
//         flipZ >> flipZ >> rotateY
//         flipZ >> flipZ >> rotateY >> rotateY
//         flipZ >> flipZ >> rotateY >> rotateY >> rotateY     
//         flipZ >> flipZ >> rotateZ
//         flipZ >> flipZ >> rotateZ >> rotateZ
//         flipZ >> flipZ >> rotateZ >> rotateZ >> rotateZ     
//         flipZ >> flipZ >> rotateX >> rotateY
//         flipZ >> flipZ >> rotateX >> rotateZ
//         flipZ >> flipZ >> rotateX >> rotateY >> rotateZ
//         flipZ >> flipZ >> rotateY >> rotateX
//         flipZ >> flipZ >> rotateY >> rotateZ
//         flipZ >> flipZ >> rotateY >> rotateX >> rotateZ
//         flipZ >> flipZ >> rotateY >> rotateZ >> rotateY
//         flipZ >> flipZ >> rotateZ >> rotateY
//         flipZ >> flipZ >> rotateZ >> rotateX
//         flipZ >> flipZ >> rotateZ >> rotateX >> rotateY
//         flipZ >> flipZ >> rotateZ >> rotateZ >> rotateX
//         flipY >> flipZ >> rotateX
//         flipY >> flipZ >> rotateX >> rotateX
//         flipY >> flipZ >> rotateX >> rotateX >> rotateX
//         flipY >> flipZ >> rotateY
//         flipY >> flipZ >> rotateY >> rotateY
//         flipY >> flipZ >> rotateY >> rotateY >> rotateY     
//         flipY >> flipZ >> rotateZ
//         flipY >> flipZ >> rotateZ >> rotateZ
//         flipY >> flipZ >> rotateZ >> rotateZ >> rotateZ     
//         flipY >> flipZ >> rotateX >> rotateY
//         flipY >> flipZ >> rotateX >> rotateZ
//         flipY >> flipZ >> rotateX >> rotateY >> rotateZ
//         flipY >> flipZ >> rotateY >> rotateX
//         flipY >> flipZ >> rotateY >> rotateZ
//         flipY >> flipZ >> rotateY >> rotateX >> rotateZ
//         flipY >> flipZ >> rotateY >> rotateZ >> rotateY
//         flipY >> flipZ >> rotateZ >> rotateY
//         flipY >> flipZ >> rotateZ >> rotateX
//         flipY >> flipZ >> rotateZ >> rotateX >> rotateY
//         flipY >> flipZ >> rotateZ >> rotateZ >> rotateX
//         flipX >> flipY >> flipZ >> rotateX
//         flipX >> flipY >> flipZ >> rotateX >> rotateX
//         flipX >> flipY >> flipZ >> rotateX >> rotateX >> rotateX
//         flipX >> flipY >> flipZ >> rotateY
//         flipX >> flipY >> flipZ >> rotateY >> rotateY
//         flipX >> flipY >> flipZ >> rotateY >> rotateY >> rotateY     
//         flipX >> flipY >> flipZ >> rotateZ
//         flipX >> flipY >> flipZ >> rotateZ >> rotateZ
//         flipX >> flipY >> flipZ >> rotateZ >> rotateZ >> rotateZ     
//         flipX >> flipY >> flipZ >> rotateX >> rotateY
//         flipX >> flipY >> flipZ >> rotateX >> rotateZ
//         flipX >> flipY >> flipZ >> rotateX >> rotateY >> rotateZ
//         flipX >> flipY >> flipZ >> rotateY >> rotateX
//         flipX >> flipY >> flipZ >> rotateY >> rotateZ
//         flipX >> flipY >> flipZ >> rotateY >> rotateX >> rotateZ
//         flipX >> flipY >> flipZ >> rotateY >> rotateZ >> rotateY
//         flipX >> flipY >> flipZ >> rotateZ >> rotateY
//         flipX >> flipY >> flipZ >> rotateZ >> rotateX
//         flipX >> flipY >> flipZ >> rotateZ >> rotateX >> rotateY
//         flipX >> flipY >> flipZ >> rotateZ >> rotateZ >> rotateX
     
//      ]


// rotateX{1,3} >> rotateY{1,3}
// rotateX{1,3} >> rotateZ{1,3}







let matchingPoints xs ys =
    // For all points in xs, check if they exist in ys
    List.filter (fun x -> List.contains x ys) xs
    |> List.length
let tryTranslations (xs: (int64 * int64 * int64) list) (ys : (int64 * int64 * int64) list) =
    // For each point in xs
    // subv xs.[i] ys.[0]
    // then translate by that vector
    // And check if we have >= 12 matching points
    // let yh = List.head ys
    // let yh' = List.tail ys |> List.head
    let vs = List.collect (fun x -> List.map (subv x) ys) xs |> List.distinct
    // let vs' = List.map (fun x -> subv x yh') xs    
    let tys = List.map (fun x -> x,translate x ys) vs
    // let tys' = List.map (fun x -> x,translate x ys) vs'

    let validTranslations = List.filter (fun (_,x) -> matchingPoints x xs >= 12) (tys)
    match validTranslations with
        | [] -> None
        | xs -> Some xs

let rec findRot' toMatch ps transforms =
    match transforms with
        | [] ->
            // printfn "No more transformations!"
            None
        | tf::tfs ->
            let withT = tf ps
            match tryTranslations toMatch withT with
                | Some ((v,vs)::_) ->
                    printfn "Oh boy, found a translation!\ntv: %A\nnew points:\n%A" v vs
                    Some (v,vs)
                | None ->
                    // printfn "Dang it! Trying next transformation"
                    findRot' toMatch ps tfs 

let mapBeacons start unknown =
    let mutable known = start
    let mutable whereTheFuckAreYou = unknown
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
                    printfn "mapped %s" (fst whereTheFuckAreYou.[i])
                    printfn "Found translation by %A" v
                    known <- known @ vs |> List.distinct
                    printfn "Found one!\n%A" vs
                    printfn "New known:\n%A" known
        whereTheFuckAreYou <- wtfay
        lc <- lc + 1
    printfn "Found beacons: %d" (List.length known)
    printfn "Leftovers: %A" (List.length whereTheFuckAreYou)
    known

let t0 = List.head test |> snd
let tt = List.tail test
mapBeacons t0 tt

mapBeacons (List.head data |> snd) (List.tail data)

