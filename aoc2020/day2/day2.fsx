open System.IO
open System.Text.RegularExpressions

// Handy for exploding a string into a char list
let explode s = [for c in s do yield c]

// (try to) Parse an integer
let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

// (try to) parse a char
let (|Char|_|) (str:string) =
    if str.Length > 0 then Some str.[0] else None

// (try to) parse a regex - fancy stuff!
let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

type policy = int*int*char
type password = string

// Parse a policy and it's associated password, very fancy
let (|Policy|_|) : string -> (policy * password) option = function
    | ParseRegex "(\d+)-(\d+)\s+([a-zA-Z]):\s+([a-zA-Z]+)" [Integer l; Integer m; Char c;s] -> Some ((l,m,c) ,s)
    | _ -> None

// Use the active patterns to actually parse policies and passwords
let getPolicyAndPassword = function
    | Policy p -> p
    | _ -> failwith "No policy and password found when parsing"

// validate a password according to it's associated policy
let validatePassword = function
    | (l,m,c),p -> explode p |> List.filter ((=) c) |> (fun x -> List.length x >= l && List.length x <= m)



// Just reads the file of policies and passwords into a list of strings
let readFile file  =
    File.ReadAllLines file
    |> List.ofArray


let data = readFile "input.txt"
let test = readFile "test.txt"
let policiesAndPasswords = List.map getPolicyAndPassword data

policiesAndPasswords
|> List.filter validatePassword
|> List.length
|> printfn "Solution part 1: %d"


// Part 2
// validate a password according to it's associated policy
let validatePassword' = function
    | (l,m,c),(p:password) ->
        let c1 = p.[l-1] = c
        let c2 = p.[m-1] = c
        c1 <> c2


policiesAndPasswords
|> List.filter validatePassword'
|> List.length
|> printfn "Solution part 2: %d"


