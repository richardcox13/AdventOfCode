﻿open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open AoC.Common

type Part = {
        A: int
        M: int
        S: int
        X: int
    }

type Rule =
    | Goto of name:string
    | GreaterThan of Attribute:string * Value:int * Next:string
    | LessThan of Attribute:string * Value:int * Next:string
    override r.ToString() =
        match r with
        | Goto(name) -> $"Goto {name}"
        | GreaterThan(attr, value, name) -> $"if {attr}>{value} goto {name}"
        | LessThan(attr, value, name) -> $"if {attr}<{value} goto {name}"

type Workflow = {
        Name: string
        Rules: Rule[]
    }

// eg, "ex{x>10:one,m<20:two,a>30:R,A}"
let parseWorkflow (line: string) =
    let m = Regex.Match(line, @"(?<name>[ARa-z]+){(?<rules>.*)}")
    assert (m.Success)
    let name = m.Groups["name"].Value
    let ruleStr = m.Groups["rules"].Value
    let rules
        = ruleStr.Trim('{', '}').Split(',')
          |> Seq.map (fun r ->
                                let m = Regex.Match(r, @"(?:(?<attr>[amsx])(?<op>[<>])(?<val>\d+):)?((?<name>[ARa-z]+))")
                                assert (m.Success)
                                let n = m.Groups["name"].Value
                                if m.Groups["attr"].Success then
                                    let v = Int32.Parse(m.Groups["val"].Value)
                                    let a = m.Groups["attr"].Value
                                    if m.Groups["op"].Value = ">" then
                                        GreaterThan(a, v, n)
                                    else
                                        LessThan(a, v, n)
                                else
                                    Goto(n)
                            )
          |> Seq.toArray

    { Name = name; Rules = rules }

let parsePart (line: string) =
    { A = -1; M = -1; S = -1; X = -1}

// Returns (Workdlow[] * Part[])
let parseInput (input: string[]) =
    let mutable loadParts = false

    let workflows = new List<Workflow>()
    let parts = new List<Part>()
    for line in input do
        match (loadParts, line) with
        | (false, "") ->
            loadParts <- true
        | (false, l) ->
            workflows.Add(parseWorkflow l)
        | (true, l) ->
            parts.Add(parsePart l)

    (workflows |> Seq.toArray), (parts |> Seq.toArray)

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 19 Part 1"
    printfn ""
    use diag = Utility.GetTracker ()
    let filename = args[0]
    printfn $"Input file {filename}"
    let input = File.ReadAllLines(filename)
    printfn ""

    let (workflowArray, parts) = parseInput input
    printfn $"There are {workflowArray.Length} workflows, and {parts.Length} parts"

    let workflows = Map (workflowArray |> Seq.map (fun w -> w.Name, w))
    let inW = workflows["in"]
    printfn $"\"in\" rules = %A{inW.Rules}"

    let result = -1
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
