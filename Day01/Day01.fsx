#time "on"

open System.IO

printfn "Max: %d" (File.ReadAllText("./Day1.txt").Trim().Split("\r\n\r\n")
         |> Seq.map (fun group -> group.Split("\r\n") |> Seq.map (fun x -> int x) |> Seq.sum)
         |> Seq.max)

