open System
open System.IO

open AoC.Common


let elfHash (str:string) =
    str
      |> Seq.fold (fun h c -> ((h + (int c)) * 17) % 256) 
                  0

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 15 Part 2"
    printfn ""
    use diag = Utility.GetTracker ()

    let input = if args.Length = 1 then
                    let filename = args[0]
                    printfn $"Input file {filename}"
                    File.ReadAllText(filename).Trim()
                else
                    printfn "Using test input"
                    "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
    printfn ""

    let result
        = input.Trim().Split(',')
          |> Seq.map (fun s -> elfHash s)
          |> Seq.sum

    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
