open System

let isAllZeros sequence =
    sequence |> Seq.forall (fun s -> s = 0)

let formatArray (a) =
    "(" + String.Join("; ", a |> Seq.map (fun (x:int) -> $"{x:``#,#``}")) + ")"

let areAllEqual sequence =
    assert not (sequence |> Seq.isEmpty)
    let f = sequence |> Seq.head
    sequence |> Seq.skip 1 |> Seq.forall (fun a -> a = f)

let parseLine (input: string) =
    input.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun s -> Int32.Parse(s))
        |> Seq.toArray

let difiSequence (sequence: int[]) =
    sequence
    |> Seq.scan (fun (result, last) next -> next-last, next) (-1, 0)
    // First result will be the initial state, 2nd will be diff with zero from initial state
    |> Seq.skip 2
    |> Seq.map (fun (result, _) -> result)
    |> Seq.toArray

let makeDiffSet startSequence =
    let rec inner ss =
        seq {
            if not (isAllZeros ss) then
                yield ss
                yield! inner (difiSequence ss)
        }
    inner startSequence |> Seq.toArray

let calcNextInBaseSequence (diffSet: int array array) =
    assert (diffSet.Length > 1)

    let rec recurse diffSetIndex prevNewValue =
        let sequence = diffSet[diffSetIndex]
        let newNextValue = sequence[0] - prevNewValue
        if diffSetIndex = 0 then
            newNextValue
        else
            recurse (diffSetIndex-1) newNextValue

    let lastDiffs = diffSet[diffSet.Length - 1]
    assert (areAllEqual lastDiffs)
    let firstNewValue = lastDiffs[0]

    recurse (diffSet.Length - 2) firstNewValue

let processOneSequence index (sequence: int[]) =
    let diffSet = makeDiffSet sequence

    //for outer in diffSet do
    //    printfn $"###: {formatArray outer}"

    let answer = calcNextInBaseSequence diffSet

    printfn $"Seq {index}: answer = {answer}"

    answer

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn ""
    printfn $"Day 9 Part 2"
    printfn ""
    let filename = args[0]
    printfn $"Input file {filename}"
    printfn ""
    let inputText = System.IO.File.ReadLines(filename)

    let sw = System.Diagnostics.Stopwatch.StartNew ()

    let sum
        = inputText
          //|> Seq.take 1  (* **** LIMIT TO ONE INPUT **** *)
          |> Seq.mapi (fun idx line->
                                processOneSequence (idx+1) (parseLine line)
                               )
          |> Seq.sumBy (fun a -> int64 a)

    printfn ""
    printfn $"Final sum = {sum:``#,#``} ({sum})"
    printfn ""
    let ts = sw.Elapsed.ToString("h':'mm':'ss'.'FFF")
    printfn $"Completed in +{ts}"
    printfn $"GC counts 0: {GC.CollectionCount(0)}; 1: {GC.CollectionCount(1)}; 2: {GC.CollectionCount(2)}; "
    9
