app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br" }

import pf.Stdout
import pf.File

stringToColor = \s ->
    when s is
        "red" -> Ok Red
        "blue" -> Ok Blue
        "green" -> Ok Green
        _ -> Err NotAColor

parseHand = \h ->
    when Str.split h " " is
        [count, color] ->
            realColor = stringToColor? color
            realCount = Str.toU32? count
            Ok (realColor, realCount)

        _ -> Err NotAValidHand

parseHandsOuter = \game ->
    parsed = List.map game.games parseHands
    { idx: game.idx, games: parsed }

parseHands = \hss ->
    List.map hss parseHand

getIndex = \r ->
    x = Str.splitFirst? r.before " "

    when Str.toU32 x.after is
        Ok i ->
            Ok { idx: i, gameString: r.after }

        Err e -> Err e

splitSemicolonsAndCommas = \r ->
    s = Str.split r.gameString "; "
    ss = List.map s \x -> Str.split x ", "
    Ok { idx: r.idx, games: ss }

flattenGames = \gs ->
    flattened = List.join gs.games
    { idx: gs.idx, games: flattened }

readFile = \s ->
    Str.split s "\n"
        |> List.keepOks \x -> Str.splitFirst? x ": "
        |> List.keepOks getIndex
        |> List.keepOks splitSemicolonsAndCommas
        |> List.map parseHandsOuter
        |> List.map flattenGames

part1 = \data ->
    # Leaving out the type annotations causes LLVM Consistency check (i.e. compiler error)
    red : U32
    red = 12
    green : U32
    green = 13
    blue : U32
    blue = 14
    filter = \game ->
        when game is
            Ok (Red, n) -> n <= red
            Ok (Green, n) -> n <= green
            Ok (Blue, n) -> n <= blue
            _ -> crash "whoopsie"

    outerFilter = \r ->
        List.all r.games filter
    filtered = List.keepIf data outerFilter
    indices = List.map filtered (\r -> r.idx)
    result = List.sum indices
    result

# This is so very not pretty
part2Inner = \games ->
    redFilter = \d ->
        when d is
            Ok (Red, _) -> Bool.true
            _ -> Bool.false

    blueFilter = \d ->
        when d is
            Ok (Blue, _) -> Bool.true
            _ -> Bool.false

    greenFilter = \d ->
        when d is
            Ok (Green, _) -> Bool.true
            _ -> Bool.false

    extract = \g ->
        when g is
            Ok (_, n) -> n
            _ -> 0

    maxRed = List.keepIf games redFilter |> List.map extract |> List.max
    maxBlue = List.keepIf games blueFilter |> List.map extract |> List.max
    maxGreen = List.keepIf games greenFilter |> List.map extract |> List.max
    # No idea how to unpack this proper with e.g. the ? syntax but shot works
    when maxRed is
        Err _ -> crash "red"
        Ok r ->
            when maxBlue is
                Err _ -> crash "blue"
                Ok b ->
                    when maxGreen is
                        Err _ -> crash "green"
                        Ok g -> r * b * g

part2 = \r ->
    List.map r (\rr -> part2Inner rr.games)
    |> List.sum

main =
    input = File.readUtf8! "input.txt"
    data = readFile input

    part1Result = part1 data

    part1String = Num.toStr part1Result
    part2Result = part2 data

    part2String = Num.toStr part2Result

    Stdout.line! "Part 1: $(part1String)\nPart 2: $(part2String)"
