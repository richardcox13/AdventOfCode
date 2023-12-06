open System


type Race = {
        Time: int
        DistanceRecord: int
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

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    let filename = args[0]
    printfn $"Reading from {filename}"

    let sw = System.Diagnostics.Stopwatch.StartNew ()

    let races = readInput filename
    printf $"There are {races |> Seq.length} races: "
    printfn "%s" (races |> Seq.map (fun r -> $"(Time:{r.Time},Record:{r.DistanceRecord})") |> String.concat ", ")

    sw.Stop()
    let ts = sw.Elapsed.ToString("h':'mm':'ss'.'FFF")
    printfn $"Completed in +{ts}"
    0
