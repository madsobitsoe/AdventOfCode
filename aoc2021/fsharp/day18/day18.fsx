open System.IO


type snailfishNumber = Term of int64 | Pair of snailfishNumber * snailfishNumber

let toSFN a b = Pair (a,b)
let cToInt (c:char) = (int c) - 48 |> int64 |> Term

let rec eatRParen = function
    | [] -> []
    | ']'::xs -> eatRParen xs
    | xs -> xs

let rec parseLeft = function
    | [] -> failwith "Empty input while parsing, parseLeft"
    // Single number followed by comma
    | '['::n::','::xs -> cToInt n,(','::xs)
    // nested left
    | '['::'['::xs ->
        parse ('['::xs)
    | xs ->
        // printfn "PL: Unexpected %A" xs
        parse xs

and parseRight = function
    | [] -> failwith "Empty input while parsing, parseRight"
    // comma, followed by single number, followed by closing bracket
    | ','::n::']'::xs ->
        cToInt n,xs
    // comma followed by anything else
    | ','::xs ->
        // printfn "PR: Recursively calling parse with %A" xs
        parse xs

and parse = function
    | [] -> failwith "empty input while parsing, parse"
    | xs ->
        let left,rest = parseLeft xs
        let right,rest' = parseRight rest
        toSFN left right,eatRParen rest'

let parseSFN s =
    match parse s with
        | sfn,[] -> sfn
        | sfn,xs -> sprintf "Parsed %A with leftover %A" sfn xs |> failwith 

    
let explode (s:string) = [for c in s do yield c]
let readFile filename =
    File.ReadAllLines filename
    |> Array.toList
    |> List.map explode
    |> List.map parseSFN



let shouldExplode sfn =
    let rec shouldExplode' n sfn =
            match n,sfn with
                | 4,Pair (a,b)  -> true
                | _,Pair (a,b) -> shouldExplode' (n+1) a || shouldExplode' (n+1) b
                | _,_ -> false
    shouldExplode' 0 sfn

let rec shouldSplit = function
    | Term n when n >= 10L -> true
    | Pair (a,b) -> shouldSplit a || shouldSplit b
    | _ -> false

let addToFirstLeftTerm t sfn =
    let rec helper t sfn =
        match sfn with
        | Term x -> Some (Term (x+t))
        | Pair (a,b) ->
            match helper t a with
                | Some na -> Some (Pair (na,b))            
                | None ->
                    match helper t b with
                        | Some nb -> Some (Pair (a,nb))
                        | None -> None

    match helper t sfn with
        | None -> sfn
        | Some sfn -> sfn

let addToFirstRightTerm t sfn =
    let rec helper t sfn =
        match sfn with
        | Term x -> Some (Term (x+t))
        | Pair (a,b) ->
            match helper t b with
                | Some nb -> Some (Pair (a,nb))
                | None ->
                    match helper t a with
                        | Some na -> Some (Pair (na,b))
                        | None -> None
    match helper t sfn with
        | None -> sfn
        | Some sfn -> sfn


// This *has* to be possible to do easier
// I am probably just really really stupid and missing the obvious
let explodeSFN sfn = 
    let rec helper n sfn =
        match n,sfn with
            | _,Term a -> None,None,None
            | 4, Pair (Term a, Term b) ->
                Some (Term 0L),Some a,Some b
            | 4,_ -> None,None,None
            // Closer left!
            | n,Pair (Term a,b) ->
                match helper (n+1) b with
                    | None,_,_ -> Some (Pair (Term a, b)),None,None
                    | Some nt,Some l,r -> Some (Pair (Term (a+l), nt)),None,r
                    | Some nt,None,r -> Some (Pair (Term a, nt)),None,r

            // Closer right!
            | n,Pair (a,Term b) ->
                match helper (n+1) a with
                    | None,_,_ -> Some (Pair (a, Term b)),None,None                    
                    | Some nt, l, Some r -> Some (Pair (nt, Term (b+r))),l,None
                    | Some nt,l,None -> Some (Pair (nt, Term b)),l,None
                    
            // No new closer terms, just recurse left, then right
            | n,Pair (a,b) ->
                match helper (n+1) a with
                    | Some a',_,_ when a' = a ->
                        // If we came from b, we can only add to a
                        match helper (n+1) b with
                            | Some nb,Some l,r ->
                                let na,nl =
                                    let na' = addToFirstRightTerm l a
                                    if na' = a then a,Some l else na',None
                                Some (Pair (na,nb)),nl,r
                            | Some nb,None,r ->
                                Some (Pair (a ,nb)),None,r

                    // If we came from a, we can not add anything to a
                    | Some na, l, Some r ->
                        let nb,nr =
                            let nb' = addToFirstLeftTerm r b
                            if nb' = b then b,Some r else nb',None
                        Some (Pair(na,nb)),l,nr
                    | Some na, l, None ->
                        Some (Pair(na, b)),l,None
                    | Some na, l, Some r ->
                        let nb,nr =
                            let nb' = addToFirstLeftTerm r b
                            if nb' = b then b,Some r else nb',None
                        Some (Pair(na,nb)),l,nr
                        
    match helper 0 sfn with
        | Some sfn,_,_ -> sfn
        | _,_,_ -> failwith "Error when exploding sfn"



let splitSFN sfn =
    let splitHelper n =
        let asf = float n
        Pair (Term (asf / 2.0 |> int64), Term (asf / 2.0 |> ceil |> int64))
    let rec helper sfn =
        match sfn with
            | Term n when n >= 10L -> Some (splitHelper n)
            | Term _ -> None
            | Pair (a,b) ->
                match helper a with
                    | Some p -> Some (Pair (p, b))
                    | None ->
                        match helper b with
                            | None -> None
                            | Some p -> Some (Pair (a,p))

    match helper sfn with
        | None -> failwith "Failure when splitting sfn"
        | Some sfn -> sfn

let rec psfn sfn =
    match sfn with
        | Term n -> sprintf "%d" n
        | Pair (a,b) -> "[" + psfn a + "," + psfn b + "]"

let rec reduceSFN sfn =
    if shouldExplode sfn then
        let x = explodeSFN sfn
        reduceSFN x
    else if shouldSplit sfn then
        let x = splitSFN sfn
        reduceSFN x
    else
        sfn


let rec magnitude = function
    | Term n -> n
    | Pair (a,b) -> magnitude a * 3L + magnitude b * 2L



let data = readFile "input.txt"

// let test = readFile "test.txt"
// let test2 = readFile "test2.txt"
printfn "Oh boy here we go:\n"

// Part 1
data
|> List.reduce (fun acc x -> toSFN  acc x |> reduceSFN)
|> magnitude
|> printfn "Solution part 1: %d"


// Part 2
let p2data = List.allPairs data data |> List.filter (fun (a,b) -> a <> b)

p2data
|> List.map (fun (a,b) -> toSFN a b |> reduceSFN |> magnitude)
|> List.max
|> printfn "Solution part 2: %d"
