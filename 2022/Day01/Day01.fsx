#time "on"

open System.IO

let res =  (File.ReadAllText("./Day1.txt").Trim().Split("\r\n\r\n")
         |> Seq.map (fun group -> group.Split("\r\n") |> Seq.map (fun x -> int x) |> Seq.sum)
         |> Seq.sortDescending
         |> Seq.take 3)

printfn "Highest: %d" (res |> Seq.last)
printfn "Sum of top 3: %d" (res |> Seq.sum)

