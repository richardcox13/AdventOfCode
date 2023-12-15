open System
open System.Collections.Generic
open System.IO

open AoC.Common

type LensLabel = {
        Id: string
        Focus: int
    }

type Boxes = List<LensLabel> array

let elfHash (str:string) =
    str
      |> Seq.fold (fun h c -> ((h + (int c)) * 17) % 256) 
                  0

let processOneInstruction (boxes: Boxes) (instruction: string) =
    // returns (op: - or =, LensLabel, label hash)
    let splitInstruction (inst: string) =
        if inst.Contains('-') then
            let id = inst.TrimEnd('-')
            let ll =  { Id = id; Focus = -1 }
            '-', ll, (elfHash ll.Id)
        else
            let parts = inst.Split('=')
            assert (parts.Length = 2)
            let ll =  { Id = parts[0]; Focus = Int32.Parse(parts[1])}
            '=', ll, (elfHash ll.Id)

    let findLensInBox (label: LensLabel) (hash: int) =
        let box = boxes[hash]
        box, (box |> Seq.tryFindIndex (fun l -> l.Id = label.Id))

    let processUpsertInstruction (label: LensLabel) (hash: int) =
        printfn $"Processing upsert of {label.Id} from {hash}"
        let box,idxOpt = findLensInBox label hash
        match idxOpt with
        | None ->
            printfn "  appending to box"
            box.Add(label)
        | Some  idx ->
            printfn $"  replacing lense at index {idx}"
            box[idx] <- label

    let processRemoveInstruction (label: LensLabel) (hash: int) =
        printfn $"Processing remove of {label.Id} from {hash}"
        let box,idxOpt = findLensInBox label hash
        match idxOpt with
        | None ->
            printfn "  Not found"
            ()
        | Some idx ->
            printfn $"  Removing at index {idx}"
            box.RemoveAt(idx);



    let (op,label,hash) = splitInstruction instruction

    if op = '-' then
        processRemoveInstruction label hash
    else
        assert (op = '=')
        processUpsertInstruction label hash

let printBoxes (title: string) (boxes: Boxes) =
    Console.WriteLine(title)
    for bi in 0 .. 255 do
        let b = boxes[bi]
        if b.Count > 0 then
            Console.Write($"  {bi:``##0``}: ")
            let s = b |> Seq.map (fun l -> $"[{l.Id},{l.Focus}]") |> String.concat " "
            Console.WriteLine(s)

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

    let boxes: Boxes = Array.init 256 (fun _ -> new List<LensLabel>())

    // Populate boxes...
    input.Trim().Split(',')
          |> Seq.iter (fun inst -> processOneInstruction boxes inst)

    printBoxes "After running instructions" boxes

    let result = -1
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
