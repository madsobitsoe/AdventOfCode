open System.IO
open System.Text.RegularExpressions


// The automatic passport scanners are slow because they're having trouble detecting which passports have all required fields. The expected fields are as follows:

// byr (Birth Year)
// iyr (Issue Year)
// eyr (Expiration Year)
// hgt (Height)
// hcl (Hair Color)
// ecl (Eye Color)
// pid (Passport ID)
// cid (Country ID)
// Passport data is validated in batch files (your puzzle input). Each passport is represented as a sequence of key:value pairs separated by spaces or newlines. Passports are separated by blank lines.


type height = Cm of int | Inches of int | Unspecified of int
type pid = ValidPid of string | InvalidPid of string
type hcl = ValidHcl of string | InvalidHcl of string
type ecl = ValidEcl of string | InvalidEcl of string
type passport =
    {
        byr:int option;
        iyr:int option;
        eyr:int option;
        hgt:height option;
        hcl:hcl option;
        ecl:ecl option;
        pid:pid option;
        cid:int option;
    }

let defaultPassport = 
    {
        byr = None;
        iyr = None;
        eyr = None;
        hgt = None;
        hcl = None;
        ecl = None;
        pid = None;
        cid = None;
    }

// Partial active pattern for parsing an int
let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let addField passport = function
    | ParseRegex "byr:(\d+)" [Integer year] -> { passport with passport.byr = Some year }
    | ParseRegex "iyr:(\d+)" [Integer year] -> { passport with passport.iyr = Some year }
    | ParseRegex "eyr:(\d+)" [Integer year] -> { passport with passport.eyr = Some year }    
    | ParseRegex "hgt:(\d+)in" [Integer height] -> { passport with passport.hgt = height |> Inches |> Some }
    | ParseRegex "hgt:(\d+)cm" [Integer height] -> { passport with passport.hgt = height |> Cm |> Some }
    | ParseRegex "hgt:(\d+)" [Integer height] -> { passport with passport.hgt = Unspecified height |> Some }
    | ParseRegex "hcl:(#[a-f0-9]{6})" [hcl] -> { passport with passport.hcl = ValidHcl hcl |> Some }
    | ParseRegex "hcl:(.+)" [hcl] -> { passport with passport.hcl = InvalidHcl hcl |> Some }
    | ParseRegex "ecl:(amb|blu|brn|gry|grn|hzl|oth)" [ecl] -> { passport with passport.ecl = ValidEcl ecl |> Some }    
    | ParseRegex "ecl:(.*)" [ecl] -> { passport with passport.ecl = InvalidEcl ecl |> Some }
    | ParseRegex "pid:([0-9]{9})$"  [pid] -> { passport with passport.pid = ValidPid pid |> Some }
    | ParseRegex "pid:(.*)" [pid] -> { passport with passport.pid = InvalidPid pid |> Some }
    | ParseRegex "cid:(\d+)" [Integer cid] -> { passport with passport.cid = Some cid }        
    | _ -> passport

let buildPassport = List.fold addField defaultPassport

// Just reads the file of passports into a list of passports
let readFile file  =
    File.ReadAllText file
    |> (fun (s:string) -> s.Split("\n\n"))
    |> Array.map (fun (s:string) -> s.Replace(" ", "\n").Split("\n") |> Array.toList)
    |> Array.toList
    |> List.map buildPassport

let getValidPassports validator = List.filter validator 

let validatorPart1 passport =
        passport.byr.IsSome &&
        passport.iyr.IsSome &&
        passport.eyr.IsSome &&
        passport.hgt.IsSome &&
        passport.hcl.IsSome &&
        passport.ecl.IsSome &&
        passport.pid.IsSome

let inRange low high value = low <= value && value <= high

// Part 2
// You can continue to ignore the cid field, but each other field has strict rules about what values are valid for automatic validation:
// byr (Birth Year) - four digits; at least 1920 and at most 2002.
// iyr (Issue Year) - four digits; at least 2010 and at most 2020.
// eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
// hgt (Height) - a number followed by either cm or in:
// If cm, the number must be at least 150 and at most 193.
// If in, the number must be at least 59 and at most 76.
// hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
// ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
// pid (Passport ID) - a nine-digit number, including leading zeroes.
// cid (Country ID) - ignored, missing or not.
let validatorPart2 passport =
    // All the required fields need to be present
    if not (validatorPart1 passport) then false
    else
        let byr = passport.byr.Value |> inRange 1920 2002 
        let iyr = passport.iyr.Value |> inRange 2010 2020 
        let eyr = passport.eyr.Value |> inRange 2020 2030
        let height =
            match passport.hgt with
                | Some (Inches h) -> inRange 59 76 h
                | Some (Cm c) -> inRange 150 193 c
                | _ -> false
        let hcl =
            match passport.hcl with
                | Some (ValidHcl _) -> true
                | _ -> false
        let ecl =
            match passport.ecl with
                | Some (ValidEcl _) -> true
                | _ -> false
        let pid =
            match passport.pid with
                | Some (ValidPid _) -> true
                | _ -> false
        
        byr && iyr && eyr && height && hcl && ecl && pid

let test = readFile "test.txt"
let data = readFile "input.txt"

getValidPassports validatorPart1 data
|> List.length
|> printfn "Solution part 1: %d" 

getValidPassports validatorPart2 data
|> List.distinct
|> List.length
|> printfn "Solution part 2: %d" 

