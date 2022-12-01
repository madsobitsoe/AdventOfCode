open System.IO

let explode (s:string) = [for c in s do yield c]

let toBin = function
    | '0' -> "0000"
    | '1' -> "0001"
    | '2' -> "0010"
    | '3' -> "0011"
    | '4' -> "0100"
    | '5' -> "0101"
    | '6' -> "0110"
    | '7' -> "0111"
    | '8' -> "1000"
    | '9' -> "1001"
    | 'A' -> "1010"
    | 'B' -> "1011"
    | 'C' -> "1100"
    | 'D' -> "1101"
    | 'E' -> "1110"
    | 'F' -> "1111"
    | _ -> ""


let binToInt (xs:char list) =
    match xs with
    | [] -> failwith "Empty bin string"
    | xs ->
        let s = List.foldBack (fun  c acc -> string c + acc) xs ""
        System.Convert.ToInt64(s,2)

let readFile filename =
    File.ReadAllText filename
    |> explode
    |> List.map toBin
    |> List.reduce (+)
    |> explode

type Packet = { version : int64;
                pid : int64;
                content : int64;
                subpackets : Packet list;
                }

let defaultPacket = {version = 0L; pid = -1L; content = -1L; subpackets = []; }

let pVersion xs =
    match xs with
        | [] -> failwith "Empty input!"
        | xs ->
            let ver,rest = List.take 3 xs, List.skip 3 xs
            binToInt ver,rest

let pID xs =
    match xs with
        | [] -> failwith "Empty input!"
        | xs ->
            let id,rest = List.take 3 xs, List.skip 3 xs
            binToInt id,rest

let pHeader xs =
    match xs with
        | [] -> failwith "Empty input!"
        | xs ->
            let v,r = pVersion xs
            let pid,r' = pID r
            { defaultPacket with version = v; pid = pid },r'

let rec pLiteral p xs : Packet * int * char list =
    let rec inner xs acc n =
        match xs with
            | '1'::xs ->
                let digits = List.take 4 xs
                let rest = List.skip 4 xs
                inner rest (acc @ digits) (n+1)
            | '0'::xs ->
                let digits = List.take 4 xs
                let rest = List.skip 4 xs
                binToInt (acc @ digits),(((n) * 5)),rest
            | _ -> failwith "failed to parse literal"
    let c,n,rest = inner xs [] 1
    { p with content = c; }, n, rest
            
and pOpTypeZero p xs : Packet * int * char list =
    // Next 15 bits are total length in bits of sub-packets in this packet
    let len = List.take 15 xs |> binToInt |> int
    let mutable r = List.skip 15 xs
    let mutable subps = []
    let mutable parsedN = 0
    while parsedN < (len) do
        let sp,n,rest' = parse r
        r <- rest'
        subps <- sp :: subps
        parsedN <- parsedN + (n)

    { p with content = -1L; subpackets = List.rev subps; },parsedN + 15,r

and pOpTypeOne p xs : Packet * int * char list =
    // Next 11 bits are number of sub-packets in this packet
    let mutable r = List.skip 11 xs
    let numPackets = List.take 11 xs |> binToInt |> int
    let mutable subps = []
    let mutable left = numPackets
    let mutable parsedN = 11
    while left > 0 do
            let sp,n,rest' = parse r
            r <- rest' //List.skip n r
            subps <- sp :: subps
            left <- left - 1
            parsedN <- parsedN + n
    { p with content = -1L; subpackets = List.rev subps; },parsedN,r

and parse xs : Packet * int * char list =
    let p,r' = pHeader xs
    match p.pid with
        | 4L ->
            let p',n,r'' = pLiteral p r'
            p',n+6,r''
        | n ->
            // Operator packet
            let packet = List.skip 1 r'
            let lenTypeID = List.take 1 r' |> List.head
            match lenTypeID with
                | '0' ->
                    let p',n,r'' = pOpTypeZero p packet
                    p',n+7,r''
                | '1' ->
                    let p',n,r'' = pOpTypeOne p packet
                    p',n+7,r''
                | _ -> failwith "Invalid length type of packet"

            
let rec getVersion (packet:Packet) =
    match packet.content with
        // We have sub-packets
        | -1L ->
            let subVs = List.map getVersion packet.subpackets |> List.concat
            int64 packet.version :: subVs
        // Literal, easy
        | n -> [(int64 packet.version)]

let rec getPacketVal (p : Packet) : int64 =
    match p.pid with
        | 0L ->
            // Sum packet
            List.fold (fun acc x -> getPacketVal x + acc) 0L (p.subpackets)            
        | 1L ->
            // Product packet
            List.fold (fun acc x -> getPacketVal x * acc) 1L (p.subpackets)
        | 2L ->
            // minimum packet
            List.map getPacketVal (p.subpackets) |> List.min
        | 3L ->
            // Maximum packet
            List.map getPacketVal (p.subpackets) |> List.max
        | 4L ->
            // Literal value
            p.content
        | 5L ->
            // Greater than packet
            let vals = List.map getPacketVal (p.subpackets)
            if vals.[0] > vals.[1] then 1L else 0L
        | 6L ->
            // Less than packet
            let vals = List.map getPacketVal (p.subpackets)
            if vals.[0] < vals.[1] then 1L else 0L
        | 7L ->
            // Equal packet
            let vals = List.map getPacketVal (p.subpackets)
            if vals.[0] = vals.[1] then 1L else 0L
        | _ -> failwith <| sprintf "Invalid packet type: %A" p.pid


let test4 = readFile "test4.txt"
let test5 = readFile "test5.txt"
let test6 = readFile "test6.txt"
let test7 = readFile "test7.txt"
let test8 = readFile "test8.txt"
let test9 = readFile "test9.txt"
let test10 = readFile "test10.txt"
let test11 = readFile "test11.txt"
let test12 = readFile "test12.txt"
let test13 = readFile "test13.txt"
let test14 = readFile "test14.txt"
let test15 = readFile "test15.txt"

let part1test =
    [
        test4, 16L;
        test5, 12L;
        test6, 23L;
        test7, 31L;
    ]
List.map (fun (a,e) -> (parse a |> (fun (p,_,_) -> getVersion p) |> List.sum) = e) part1test
|> List.reduce (&&) |> printfn "All tests for part 1 passed: %A"


let part2test =
    [
        test8, 3L;
        test9, 54L;
        test10, 7L;
        test11, 9L;
        test12, 1L;
        test13, 0L;
        test14, 0L;
        test15, 1L;
    ]

List.map (fun (a,e) -> (parse a |> (fun (p,_,_) -> getPacketVal p)) = e) part2test
|> List.reduce (&&) |> printfn "All tests for part 2 passed: %A"
    

let data = readFile "input.txt"

let (p,_,_) = parse data

p |> getVersion |> List.sum |> printfn "Solution part 1: %A"
p |> getPacketVal |> printfn "Solution part 2: %A"
