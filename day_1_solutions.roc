# See https://adventofcode.com/2021/day/1

app "solve_day_1"
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
    inputText
    |> Str.trim
    |> Str.split "\n"
    |> List.keepOks Str.toI16
    |> Ok # I only added this `Ok` after I refactored `solve` to handle errors,
          # based on @bhansconnect's design of error handling for the parser.

calculatePart1AnswerByJancvanb = \depths ->
    len = List.len depths - 1
    befores = List.sublist depths { start: 0, len }
    afters = List.sublist depths { start: 1, len }
    List.map2 afters befores Num.sub
    |> List.keepIf (\x -> x > 0)
    |> List.len

calculatePart2AnswerByJancvanb = \depths ->
    depthsCount = List.len depths - 1
    a = List.sublist depths { start: 0, len: depthsCount }
    b = List.sublist depths { start: 1, len: depthsCount }
    c = List.sublist depths { start: 2, len: depthsCount }
    sums = List.map3 a b c (\x, y, z -> x + y + z)
    sumsCount = List.len sums - 1
    befores = List.sublist sums { start: 0, len: sumsCount }
    afters = List.sublist sums { start: 1, len: sumsCount }
    List.map2 afters befores Num.sub
    |> List.keepIf (\x -> x > 0)
    |> List.len

# Core of @georgesboris's solution
# See https://gist.github.com/georgesboris/3c43ed7618f5816df85d620c41eea73c
# and https://gist.github.com/georgesboris/85dbd20b3c8c1be752fe252a59309cdb

calculatePart1AnswerByGeorgesboris = \numbers ->
    numbers
    |> List.walk { result: 0, last: Nothing } \state, num ->
        next = { state & last: Just num }
        when state.last is
            Nothing -> next
            Just l if l < num -> { next & result: state.result + 1 }
            Just _ -> next
    |> .result

calculatePart2AnswerByGeorgesboris = \numbers ->
    List.map3
        numbers
        (List.drop numbers 1)
        (List.drop numbers 2)
        (\a, b, c -> a + b + c)
        |> List.walk { result: 0, last: Nothing } \state, num ->
            next = { state & last: Just num }
            when state.last is
                Nothing -> next
                Just l if l < num -> { next & result: state.result + 1 }
                Just _ -> next
        |> .result

# Core of @ghigt's solution, lightly refactored
# See https://github.com/ghigt/advent-of-code/blob/main/roc/day-1/main.roc

calculatePart2AnswerByGhigt = \numbers ->
    numbers
    |> List.walk { range: Pair None None, sum: None, total: 0 } \acc, curr ->
        { range, sum, total } = acc
        when range is
            Pair (Some prev) None ->
                { acc & range: Pair (Some curr) (Some prev) }
            Pair (Some prev) (Some ante) ->
                newSum = curr + prev + ante
                when sum is
                    Some s if newSum > s ->
                        {
                            range: Pair (Some curr) (Some prev),
                            sum: Some newSum,
                            total: total + 1,
                        }
                    _ ->
                        { acc &
                            range: Pair (Some curr) (Some prev),
                            sum: Some newSum,
                        }
            _ ->
                { acc & range: Pair (Some curr) None }
    |> .total

# Core of @bhansconnect's solution
# See https://github.com/bhansconnect/roc-aoc-2021/blob/trunk/day1/part1.roc
# and https://github.com/bhansconnect/roc-aoc-2021/blob/trunk/day1/part2.roc

parseInputDataByBhansconnect = \contents ->
    contents
    |> Str.trim
    |> Str.split "\n"
    |> List.mapTry Str.toI16

calculatePart1AnswerByBhansconnect = \data ->
    List.map2 data (List.dropFirst data) Num.isLt
    |> List.keepIf (\x -> x)
    |> List.len

calculatePart2AnswerByBhansconnect = \data ->
    movingSums =
        drop1 = (List.dropFirst data)
        drop2 = (List.dropFirst drop1)
        List.map3 data drop1 drop2 (\a,b,c -> a + b + c)
    List.map2 movingSums (List.dropFirst movingSums) Num.isLt
    |> List.keepIf (\x -> x)
    |> List.len

# Core of @ayazhafiz's solution
# See https://gist.github.com/ayazhafiz/0d5d40b35bb58f79fb742b1d2ec7515c

calculatePart2AnswerByAyazhafiz = \nums ->
    walker = \{last2, sum, prev}, last0 ->
        when Triple last0 last2 prev is
            Triple l0 (Both l1 l2) (Some prevSum) ->
                curSum = l0 + l1 + l2
                sum1 = if curSum > prevSum then sum + 1 else sum
                { last2: Both l0 l1, sum: sum1, prev: Some curSum }
            Triple l0 (Both l1 l2) _ ->
                # First case, no increase
                {last2: Both l0 l1, sum, prev: Some (l0 + l1 + l2)}
            Triple l0 (One l1) _ ->
                {last2: Both l0 l1, sum, prev}
            Triple l0 None _ ->
                {last2: One l0, sum, prev}
    List.walk nums { last2: None, sum: 0, prev: None } walker
    |> .sum

# Core of @shritesh's solution
# See https://github.com/shritesh/advent/blob/main/2021/1.roc

parseInputDataByShritesh = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.mapTry Str.toU16

calculatePart1AnswerByShritesh = \numbers ->
    List.walk numbers { count: 0, prev: None } \{ count, prev }, n ->
        when prev is
            Some p if n > p -> { count: count + 1, prev: Some n }
            _ -> { count, prev: Some n }
    |> .count

calculatePart2AnswerByShritesh = \numbers ->
    numbers2 = List.dropFirst numbers
    numbers3 = List.dropFirst numbers2
    sums = List.map3 numbers numbers2 numbers3 \a, b, c -> a + b + c
    calculatePart1AnswerByShritesh sums

# Core of @mkalvas's solution
# See https://github.com/mkalvas/aoc/blob/roc/src/2021/01/code.roc

parseInputDataByMkalvas = \data ->
    Str.split data "\n"
    |> List.dropLast # final newline
    |> List.mapTry Str.toI32

calculatePart1AnswerByMkalvas = \nums ->
    zip nums (List.drop nums 1) |> filter |> List.len

calculatePart2AnswerByMkalvas = \nums ->
    zip nums (List.drop nums 3) |> filter |> List.len

zip = \x, y -> List.map2 x y (\a, b -> { a, b })
filter = \list -> List.keepIf list (\pair -> pair.a < pair.b)

# Core of @tritowntim's solution
# See https://github.com/tritowntim/roctoberfest/blob/main/2021/01/main.roc

parseInputDataByTritowntim = \input ->
    lines = Str.split input "\n"
    lines
    |> List.keepOks Str.toU128

calculatePart1AnswerByTritowntim = \values ->
    countIncrease = (\state, reading ->
        when state.previous is
            Just prev ->
                if reading > prev then
                    { state & count: state.count + 1, previous: Just reading }
                else
                    { state & previous: Just reading }
            Nothing ->
                { state & previous: Just reading })
    values
    |> List.walk { count: 0, previous: Nothing } countIncrease
    |> .count
    |> Num.toStr

calculatePart2AnswerByTritowntim = \values ->
    countSlidingIncrease = (\state, reading ->
        current =
            when List.len state.previous is
                3 ->
                    state.previous
                        |> List.dropFirst
                        |> List.append reading
                _ ->
                    List.append state.previous reading
        increased =
            when List.len current is
                    3 ->
                        when List.len state.previous is
                            3 ->
                                List.sum current > List.sum state.previous
                            _ ->
                                Bool.false
                    _ ->
                        Bool.false
        i = if increased then 1 else 0
        { state & count: state.count + i, previous: current })
    values
    |> List.walk { count: 0, previous: [] } countSlidingIncrease
    |> .count
    |> Num.toStr

################################################################################
################################################################################
################################################################################

# Problem-agnostic boilerplate

main =
    "day_1_input.txt"
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
