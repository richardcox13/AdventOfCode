open System



let readFile filename =
    let reader = System.IO.File.ReadLines(filename)
    reader |> Seq.map (fun l ->
                  let x = l.Split(' ') |> Array.map _.Trim()
                  x[0], Int32.Parse(x[1])
              )

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    let filename = args[0]
    printfn $"Input file {filename}"
    let sw = System.Diagnostics.Stopwatch.StartNew ()

    let data = readFile filename
    printfn "%s" (data |> Seq.map (fun (h,b) -> $"({h},{b})") |> String.concat ", ")

    let result = -1

    sw.Stop()
    printfn ""
    printfn $"Result = {result}"
    printfn ""
    let ts = sw.Elapsed.ToString("h':'mm':'ss'.'FFF")
    printfn $"Completed in +{ts}"
    printfn $"GC counts 0: {GC.CollectionCount(0)}; 1: {GC.CollectionCount(1)}; 2: {GC.CollectionCount(2)}; "
    0
