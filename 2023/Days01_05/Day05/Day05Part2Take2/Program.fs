﻿open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type GardenMapRow = {
        SourceStart: int64
        DestinationStart: int64
        Range: int64
    }

type GardenMapper =
    {
        From: string
        To: string
        Maps: GardenMapRow[]
    }
    member x.Map value =
        let m = x.Maps |> Seq.tryFind (fun x -> x.SourceStart <= value && value < (x.SourceStart + x.Range))
        match m with
        | Some mm ->
            let offset = value - mm.SourceStart
            mm.DestinationStart + offset
        | None -> value

type MappersToApply =
    {
        SeedToSoil: GardenMapper
        SoilToFertiliser: GardenMapper
        FertiliserToWater: GardenMapper
        WaterToLight: GardenMapper
        LightToTemperature: GardenMapper
        TemperatureToHumidity: GardenMapper
        HumidityToLocation: GardenMapper
    }
    member maps.getLocationForSeed seed =
        let soil = maps.SeedToSoil.Map seed
        let fertiliser = maps.SoilToFertiliser.Map soil
        let water = maps.FertiliserToWater.Map fertiliser
        let light = maps.WaterToLight.Map water
        let temp = maps.LightToTemperature.Map light
        let humidity = maps.TemperatureToHumidity.Map temp
        let location = maps.HumidityToLocation.Map humidity
        location

type SeedAndRange = {
        Seed: int64
        Range: int64
    }

let loadGardenMap (title: string) (rows: string[]) =
    //printf $"Title: {title}, {rows.Length} map lines"

    let ms = Regex.Match(title, @"(\w+)-to-(\w+)")
    assert ms.Success
    assert (ms.Groups.Count = 3) // zeroth is whole match
    let fromName = ms.Groups[1].Value
    let destName = ms.Groups[2].Value
    //printfn $"... source {fromName} to {destName}"

    let mapRows = rows |> Array.map (fun r ->
                            let nos = r.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                                    |> Array.map (fun x -> Int64.Parse(x.Trim()))
                            assert (nos.Length = 3)
                            { DestinationStart = nos[0]; SourceStart = nos[1]; Range = nos[2] }
                        )

    { From = fromName; To = destName; Maps = mapRows }

let readSeeds (input: string) =
    // eg.
    // seeds: 79 14 55 13
    // so can just skip the first 7 chars
    let line = input.Substring(7)
    line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun x -> Int64.Parse(x.Trim()))
        |> Seq.chunkBySize 2
        |> Seq.map (fun ss ->
                        assert (ss.Length = 2)
                        { Seed = ss[0]; Range = ss[1] }
                   )
        |> Seq.toArray

let processInputFile filename : SeedAndRange[] * GardenMapper seq =
    let inputText = File.ReadAllLines(filename)
    printfn $"InputText has {inputText.Length} elements"
    let seeds = readSeeds inputText[0]
    // This is followed by a blank line, line 3 (index 2) starts the first
    // map
    let mutable lineIndex = 2;
    let maps = List<GardenMapper>()
    while lineIndex < inputText.Length do
        assert not (String.IsNullOrWhiteSpace(inputText[lineIndex]))
        printf $"Reading title at index {lineIndex}"
        let title = inputText[lineIndex]
        printfn $"... {title.Trim(':')}"
        let mutable offset = 1
        while lineIndex+offset < inputText.Length && not (String.IsNullOrWhiteSpace(inputText[lineIndex+offset])) do
            //printfn $"checked offset = {offset} for index {lineIndex+offset}: {inputText[lineIndex+offset]}"
            offset <- offset+1

        let mapLines = inputText[(lineIndex+1)..(lineIndex+offset-1)]

        let map = loadGardenMap title mapLines
        maps.Add(map)
        lineIndex <- lineIndex + offset + 1 // +1 to skip ending blank line

    seeds, maps

let nameMaps mapsArray =
    let findMap name = mapsArray |> Seq.find (fun m -> m.From = name )
    {
        SeedToSoil = (findMap "seed")
        SoilToFertiliser = (findMap "soil")
        FertiliserToWater = (findMap "fertilizer")
        WaterToLight = (findMap "water")
        LightToTemperature = (findMap "light")
        TemperatureToHumidity = (findMap "temperature")
        HumidityToLocation = (findMap "humidity")
    }


[<EntryPoint>]
let main(args) =
    //printfn "args: %A" args
    //printfn "env.cmdline: %A" <| Environment.GetCommandLineArgs()
    //printfn $"Working folder: {Environment.CurrentDirectory}"

    let filename = args[0]

    let (seeds, mapsArray) = processInputFile filename
    printfn "Loaded..."
    printfn "  Seeds: %s" (seeds |> Array.map (fun s -> $"{{ Seed={s.Seed}; Range={s.Range}}}") |> String.concat ",")
    for (idx, map) in (mapsArray |> Seq.mapi (fun idx m -> idx, m)) do
        printfn $"  Map #{idx} \"{map.From}\" to \"{map.To}\" has {map.Maps.Length} maps"
        let m0 = map.Maps[0]
        printfn $"    Map #0: source from {m0.SourceStart}, destination {m0.DestinationStart}, for {m0.Range}"

    let maps = nameMaps mapsArray

    // Define here, so can close over maps

    printfn ""
    let result = seeds
                //|> Seq.take 1 (* ***** REMOVE ME! ***** *)
                |> Seq.mapi (fun idx s ->
                        printfn $"{DateTime.Now.TimeOfDay} Starting seed #{idx} (start={s.Seed}, Range={s.Range})"
                        seq { s.Seed..(s.Seed + s.Range - 1L) }
                            |> Seq.map (fun seed -> seed |> maps.getLocationForSeed )
                    )
                |> Seq.concat
                |> Seq.min

    printfn $"Result = {result}"

    0
