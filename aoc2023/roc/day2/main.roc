app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br" }

import pf.Stdout
import pf.File

main =
    input = File.readUtf8! "input.txt"
    s = Str.split input "\n"

    part1String = "Not yet"

    part2String = "Not yet"

    Stdout.line! "Part 1: $(part1String)\nPart 2: $(part2String)"
