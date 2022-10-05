# See https://adventofcode.com/2021/day/2

app "solve_day_2"
    packages { pf: "./roc/examples/interactive/cli-platform/main.roc" }
    imports [
        pf.File,
        pf.Path,
        pf.Program.{ Program },
        pf.Stderr,
        pf.Stdout,
        pf.Task,
    ]
    provides [main] to pf

# Core of my solution (before seeing other people's solutions)

parseInputDataByJancvanb = \inputText ->
    lines =
        inputText
        |> Str.trim
        |> Str.split "\n"
    List.mapTry lines \line ->
        List.walk (Str.split line " ") (Err MissingCommand) \command, word ->
            when command is
                Err MissingCommand -> Err (PartialCommand word)
                Err (PartialCommand direction) ->
                    amount = Str.toI32 word
                    when amount is
                        Ok a -> Ok (Command direction a)
                        Err _ -> Err InvalidCommand
                _ -> Err InvalidCommand

calculatePart1AnswerByJancvanb = \commands ->
    finalPosition =
        List.walk
            commands
            { horizontal: 0, depth: 0 }
            \position, Command direction distance ->
                when direction is
                    "forward" -> { position &
                        horizontal: position.horizontal + distance,
                    }
                    "down" -> { position &
                        depth: position.depth + distance,
                    }
                    "up" -> { position &
                        depth: position.depth - distance,
                    }
                    _ -> position
    finalPosition.horizontal * finalPosition.depth

calculatePart2AnswerByJancvanb = \commands ->
    finalState =
        List.walk
            commands
            { horizontal: 0, depth: 0, aim: 0 }
            \state, Command direction amount ->
                when direction is
                    "forward" -> { state &
                        horizontal: state.horizontal + amount,
                        depth: state.depth + amount * state.aim,
                    }
                    "down" -> { state &
                        aim: state.aim + amount,
                    }
                    "up" -> { state &
                        aim: state.aim - amount,
                    }
                    _ -> state
    finalState.horizontal * finalState.depth

# Core of @ghigt's solution, lightly refactored
# See https://github.com/ghigt/advent-of-code/blob/main/roc/day-2/main.roc

parseInputDataByGhigt = \content ->
    content
    |> Str.split "\n"

calculatePart1AnswerByGhigt = \lines ->
    lines
    |> List.map (\line -> Str.split line " ")
    |> List.walk { h: 0, d: 0, aim: 0 } \{ h, d, aim }, list ->
        l0 = List.first list
        l1 = List.last list |> Result.try Str.toU32
        when P l0 l1 is
            P (Ok "forward") (Ok move) -> { h: h + move, d: d + aim * move, aim }
            P (Ok "down") (Ok move) -> { h, d, aim: aim + move }
            P (Ok "up") (Ok move) -> { h, d, aim: aim - move }
            _ -> { h, d, aim }
    |> \{ h, d } -> h * d
    |> Num.toStr

# Core of @bhansconnect's solution
# See https://github.com/bhansconnect/roc-aoc-2021/blob/trunk/day2/part1.roc
# and https://github.com/bhansconnect/roc-aoc-2021/blob/trunk/day2/part2.roc

parseInputDataByBhansconnect = \contents ->
    contents
        |> Str.trim
        |> Str.split "\n"
        |> List.map (\x -> Str.split x " ")
        |> List.mapTry (\sublist ->
            when T (List.first sublist) (List.last sublist) is
                T (Ok cmd) (Ok numStr) ->
                    when Str.toI64 numStr is
                        Ok num ->
                            Ok (T cmd num)
                        _ ->
                            Err ParseError
                _ ->
                    Err ParseError
        )

calculatePart1AnswerByBhansconnect = \data ->
    # I feel like doing this a silly map way instead of with a walk at first.
    List.map data (\T cmd num ->
        when cmd is
            "up" -> T -num 0
            "down" -> T num 0
            "forward" -> T 0 num
            _ -> T 0 0
    ) |> List.walk (T 0 0) (\T depth fwd, T deltaDepth deltaFwd ->
        T (depth + deltaDepth) (fwd + deltaFwd)
    ) |> (\T depth fwd -> depth * fwd)

calculatePart2AnswerByBhansconnect = \data ->
    # I feel like doing this a silly map way instead of with a walk at first.
    List.map data (\T cmd num ->
        when cmd is
            "up" -> T -num 0
            "down" -> T num 0
            "forward" -> T 0 num
            _ -> T 0 0
    ) |> List.walk (T 0 0 0) (\T depth fwd aim, T deltaAim deltaFwd ->
        T (depth + deltaFwd * aim) (fwd + deltaFwd) (aim + deltaAim)
    ) |> (\T depth fwd _ -> depth * fwd)

# Sample of @ayazhafiz's parser, which depends on unpublished functions
# See https://gist.github.com/ayazhafiz/35449afcaf3c2b06f577ab9ff3806c16

# parseInputDataByAyazHafiz : _ -> Result (List Command) _
# parseInputDataByAyazHafiz = \input ->
#     parseCmd : Parser _ (U64 -> Command)
#     parseCmd =
#         oneOf [
#             map (string "forward") (\_ -> (\n -> Forward n)),
#             map (string "down") (\_ -> (\n -> Down n)),
#             map (string "up") (\_ -> (\n -> Up n)),
#         ]
#     parseLine =
#         const (\cmd -> \_spaces -> \num -> \_newline -> cmd num)
#         |> apply parseCmd
#         |> apply spaces
#         |> apply digits
#         |> apply newline
#     when parse (many parseLine) input is
#         Ok a -> Ok a
#         Err (Msg s) -> Err (Msg s)

# Command : [Forward U64, Down U64, Up U64]

# Core of @shritesh's solution
# See https://github.com/shritesh/advent/blob/main/2021/2.roc

parseInputDataByShritesh = \input ->
    parseLine = \line ->
        when Str.splitFirst line " " is
            Ok { before: "forward", after } -> Str.toI32 after |> Result.map Forward
            Ok { before: "down", after } -> Str.toI32 after |> Result.map Down
            Ok { before: "up", after } -> Str.toI32 after |> Result.map Up
            _ -> Err InvalidInput
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.mapTry parseLine

calculatePart1AnswerByShritesh = \commands ->
    submarine = List.walk commands { position: 0, depth: 0 } \{ position, depth }, command ->
        when command is
            Forward n -> { position: position + n, depth }
            Down n -> { position, depth: depth + n }
            Up n -> { position, depth: depth - n }
    submarine.position * submarine.depth

calculatePart2AnswerByShritesh = \commands ->
    submarine = List.walk commands { position: 0, depth: 0, aim: 0 } \{ position, depth, aim }, command ->
        when command is
            Forward n -> { position: position + n, depth: depth + aim * n, aim }
            Down n -> { position, depth, aim: aim + n }
            Up n -> { position, depth, aim: aim - n }
    submarine.position * submarine.depth

# Core of @mkalvas's solution
# See https://github.com/mkalvas/aoc/blob/roc/src/2021/02/code.roc

parseInputDataByMkalvas = \data ->
    lineToDir = \line ->
        result =
            parts = Str.split line " "
            cmd <- List.get parts 0 |> Result.try
            val <- List.get parts 1 |> Result.try Str.toI32 |> Result.try
            Ok { cmd, val }
        Result.mapErr result \_ -> InvalidLine line
    Str.split data "\n"
    |> List.dropLast
    |> List.mapTry lineToDir

calculatePart1AnswerByMkalvas = \nums ->
    state = List.walk nums { h: 0, d: 0, a: 0 } moveByMkalvas
    state.h * state.a |> Num.toStr

calculatePart2AnswerByMkalvas = \nums ->
    state = List.walk nums { h: 0, d: 0, a: 0 } moveByMkalvas
    state.h * state.d |> Num.toStr

moveByMkalvas = \{ h, d, a }, dir ->
    when dir is
        { cmd: "up", val } -> { h, d, a: a - val }
        { cmd: "down", val } -> { h, d, a: a + val }
        { cmd: "forward", val } -> { h: h + val, d: d + val * a, a }
        _ -> { h, d, a }

################################################################################
################################################################################
################################################################################

# Problem-agnostic boilerplate

main =
    "day_2_input.txt"
    |> solve
    |> Task.attempt exit
    |> Program.noArgs

solve = \inputDataFileName ->
    inputText <-
        inputDataFileName
        |> Path.fromStr
        |> File.readUtf8
        |> Task.await
    when parseInputData inputText is
        Ok inputData -> Task.succeed (Answers
            (calculatePart1Answer inputData)
            (calculatePart2Answer inputData))
        Err _ -> Task.fail DataParseError

exit = \result ->
    when result is
        Ok answers -> succeed answers
        Err error -> fail error

succeed = \Answers part1Answer part2Answer ->
    a1 = Num.toStr part1Answer
    a2 = Num.toStr part2Answer
    "Answer to part 1: \(a1)\nAnswer to part 2: \(a2)"
    |> Stdout.line 
    |> Program.exit 0

fail = \error ->
    errorMessage =
        when error is
            FileReadError _ _ ->
                "Failed to read input data file!"
            DataParseError ->
                "Failed to parse input data!"
            _ ->
                "An unknown error occurred!"
    Stderr.line errorMessage
    |> Program.exit 1

parseInputData = parseInputDataByJancvanb

calculatePart1Answer = calculatePart1AnswerByJancvanb

calculatePart2Answer = calculatePart2AnswerByJancvanb
