open System.IO

let explode (s:string) = [for c in s do yield c]

let readFile file =
    File.ReadAllText file
    |> (fun s -> s.Split " ")
    |> List.ofArray
    |> List.map explode
    |> List.map (List.filter ((<>) '\n'))


let (|Integer|_|) (str: string) =
   let mutable intvalue = 0L
   if System.Int64.TryParse(str, &intvalue) then Some(intvalue)
   else None

let getInt = function
    | Integer i -> i
    | x -> failwith <| sprintf "Bad data: %A" x

let rec transform cache stone times =
    if times = 0 then cache,1L
    else
        match Map.tryFind (times,stone) cache with
            | Some x -> cache,x
            | None ->
                let cache,res =
                    match stone with
                    | ['0'] ->
                        transform cache ['1'] (times-1)

                    | x when List.length x % 2 = 0 ->
                        let front,back = List.splitInto 2 x |> (fun x -> List.head x, List.skip 1 x |> List.head |> List.skipWhile ((=) '0') |> fun x -> if x = [] then ['0'] else x)
                        let cache',frontCount = transform cache front (times-1)
                        let cache'',backCount = transform cache' back (times-1)

                        cache'',frontCount+backCount
                    | x ->
                        let asInt64 = List.map string stone |> List.reduce (+) |> getInt
                        let asCharList = asInt64 * 2024L |> sprintf "%d" |> explode
                        transform cache asCharList (times-1)
                Map.add (times,stone) res cache,res


let data = readFile "input.txt"

let part1cache,part1res =
    data
    |> List.fold (fun (cache,acc) x -> let (cache',res) = transform cache x 25 in (cache',acc+res)) (Map.empty,0L)

let part2cache,part2res =
    data
    |> List.fold (fun (cache,acc) x -> let (cache',res) = transform cache x 75 in (cache',acc+res)) (part1cache,0L)

part1res
|> printfn "Part 1: %d"
part2res
|> printfn "Part 2: %d"

