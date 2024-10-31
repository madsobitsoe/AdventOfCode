app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br" }

import pf.Stdout
import pf.File

isNumber = \u -> u >= '0' && u <= '9'

part1 = \s ->
    ss = Str.toUtf8 s
    first = List.findFirst? ss isNumber
    last = List.findLast? ss isNumber
    [first, last]
        |> Str.fromUtf8?
        |> Str.toU32

part2 = \s ->
    Str.replaceEach s "one" "o1e"
    |> Str.replaceEach "two" "t2o"
    |> Str.replaceEach "three" "t3e"
    |> Str.replaceEach "four" "f4r"
    |> Str.replaceEach "five" "f5e"
    |> Str.replaceEach "six" "s6x"
    |> Str.replaceEach "seven" "s7n"
    |> Str.replaceEach "eight" "e8t"
    |> Str.replaceEach "nine" "n9n"
    |> part1

main =
    input = File.readUtf8! "input.txt"
    s = Str.split input "\n"

    part1String =
        List.keepOks s part1
        |> List.sum
        |> Num.toStr
    # sums = List.map s \e -> Str.split e "\n"
    #   |> List.keepOks Str.toI32
    #  |> List.sum
    # max = List.max sums

    part2String =
        List.keepOks s part2
        |> List.sum
        |> Num.toStr
    # top3 =
    #    List.sortDesc sums
    #    |> List.takeFirst 3
    #    |> List.sum
    #    |> Num.toStr

    Stdout.line! "Part 1: $(part1String)\nPart 2: $(part2String)"
