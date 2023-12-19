open System
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

// Eg, "{x=2127,m=1623,a=2188,s=1013}"
let parsePart (line: string) =
    let m = Regex.Match(line, @"{x=(?<x>\d+),m=(?<m>\d+),a=(?<a>\d+),s=(?<s>\d+)")
    assert (m.Success)

    let g (n:string) = Int32.Parse(m.Groups[n].Value)

    { A = g "a"; M = g "m"; S = g "s"; X = g "x" }

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

let processPart (workflows: Map<string, Workflow>) (part: Part) =
    let getAttr attr =
        match attr with
        | "a" -> part.A
        | "s" -> part.S
        | "m" -> part.M
        | "x" -> part.X
        | x -> failwith $"Invalid attribute \"{x}\""

    // return true for accepted and false for rejected
    let rec runWorkflows wfName =
        let rec runRules ruleIdx (rules: Rule[]) =
            assert (ruleIdx < rules.Length)
            let r = rules[ruleIdx]
            match r with
            | Goto(n) -> n
            | GreaterThan(attr,value,name) ->
                let a = getAttr attr
                if a > value then
                    name
                else
                    runRules (ruleIdx+1) rules
            | LessThan(attr,value,name) ->
                let a = getAttr attr
                if a < value then
                    name
                else
                    runRules (ruleIdx+1) rules

        printf $"{wfName} -> "
        let wf = workflows[wfName]
        let next = runRules 0 wf.Rules
        match next with
         | "A" ->
            let res = part.A + part.M + part.S + part.X
            printfn $"accept ({res})"
            res
         | "R" ->
            printfn "REJECT"
            0
         | x -> runWorkflows x

    runWorkflows "in"

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 19 Part 2"
    printfn ""
    use diag = Utility.GetTracker ()
    let filename = args[0]
    printfn $"Input file {filename}"
    let input = File.ReadAllLines(filename)
    printfn ""

    let (workflowArray, parts) = parseInput input
    printfn $"There are {workflowArray.Length} workflows, and {parts.Length} parts"

    let workflows = Map (workflowArray |> Seq.map (fun w -> w.Name, w))


    let result
        = parts
          |> Seq.map(fun p -> processPart workflows p)
          |> Seq.sum
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
