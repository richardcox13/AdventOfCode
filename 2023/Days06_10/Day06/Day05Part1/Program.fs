open System


type Race = {
        Time: int
        DistanceRecord: int
    }

type RaceResult = {
        ChargeTime: int
        DistanceMoved: int
    }

let readInput filename =
    let parseOneLine (line: string) =
        let nosStr = ((line.Split(':'))[1]).Trim()
        nosStr.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                        |> Seq.map(fun s -> Int32.Parse(s))
    let content = System.IO.File.ReadAllLines(filename)
    let times = parseOneLine content[0]
    let records = parseOneLine content[1]

    (times, records) ||>Seq.map2 (fun t r -> { Time = t; DistanceRecord = r })

let calcApproachesToRace race =
    assert (race.Time > 1)
    seq { 1..(race.Time-1) }
        |> Seq.map (fun chargeTime ->
                let moveTime = race.Time - chargeTime
                let distance = moveTime * chargeTime
                { ChargeTime = chargeTime; DistanceMoved = distance }
            )

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    let filename = args[0]
    printfn $"Reading from {filename}"

    let sw = System.Diagnostics.Stopwatch.StartNew ()

    let races = readInput filename
    printf $"There are {races |> Seq.length} races: "
    printfn "%s" (races |> Seq.map (fun r -> $"(Time:{r.Time},Record:{r.DistanceRecord})") |> String.concat ", ")

    let result
        = races
          //|> Seq.take 1  (* ***** FOR TESTING ***** *)
          |> Seq.map (fun r ->
                printfn $"Calculating ways for race of time {r.Time} (record is {r.DistanceRecord})"
                let options = calcApproachesToRace r |> Seq.toArray
                printfn "   Results: %s" (options |> Seq.map(fun res -> $"(charge={res.ChargeTime}; dist={res.DistanceMoved})") |> String.concat ", ")
                let count =
                    options
                        |> Seq.where (fun rr -> rr.DistanceMoved > r.DistanceRecord)
                        |> Seq.length
                printfn $"    {count} exceed the record"
                count
             )
          |> Seq.reduce (fun a b -> a*b)

    sw.Stop()
    printfn ""
    printfn $"Result = {result}"
    printfn ""
    let ts = sw.Elapsed.ToString("h':'mm':'ss'.'FFF")
    printfn $"Completed in +{ts}"
    printfn $"GC counts 0: {GC.CollectionCount(0)}; 1: {GC.CollectionCount(1)}; 2: {GC.CollectionCount(2)}; "
    0
