#time "on"

open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

type Operation = | Add of int
                 | Multiply of int
                 | Square

type Monkey = {
    // Use queue to pop from front as money examines and throws the items
    // and push on the back when caught
    mutable Items: int[];
    mutable Inspections: int;
    Operation: Operation;
    TestDivisor: int;
    TestPass: int;
    TestFail: int;
}

let testInput = [|
    {
        Items = [| 79; 98 |];
        Inspections = 0;
        Operation = Multiply(19);
        TestDivisor = 23;
        TestPass = 2;
        TestFail = 3
    };
    {
        Items = [| 54; 65; 75; 74 |];
        Inspections = 0;
        Operation = Add(6);
        TestDivisor = 19;
        TestPass = 2;
        TestFail = 0
    };
    {
        Items = [| 79; 60; 97 |];
        Inspections = 0;
        Operation = Square;
        TestDivisor = 13;
        TestPass = 1;
        TestFail = 3
    };
    {
        Items = [| 74 |];
        Inspections = 0;
        Operation = Add(3);
        TestDivisor = 17;
        TestPass = 0;
        TestFail = 1
    };
|]


let monkeys = testInput

let oneMoneyOnrRound (m : Monkey) item =
    let updateItem i =
        match m.Operation with
        | Add n -> i+n
        | Multiply n -> i*n
        | Square -> i*i

    let appendArray a i =
        Array.append a [| i |]

    let newItem = (updateItem item) / 3
    let target = monkeys[if newItem % m.TestDivisor = 0 then m.TestPass else m.TestFail]
    target.Items <- appendArray target.Items newItem
    m.Inspections <- m.Inspections+1

let printMoney n m =
    printfn "Monkey #%d: Inspections: %d" n m.Inspections
    printfn "  Items: %s" (m.Items |> Seq.map (fun x -> x.ToString()) |> String.concat ", ")

let printAll msg =
    printfn ""
    printfn "%s" msg
    for m in (monkeys |> Seq.mapi (fun idx m -> (idx, m))) do
        printMoney (fst m) (snd m)

printAll "Before Any Inspections:"

let m0 = monkeys[0].Items |> Array.head
oneMoneyOnrRound monkeys[0] m0
monkeys[0].Items <- Array.skip 1  monkeys[0].Items

printAll "After one Inspection:"

let m1 = monkeys[0].Items |> Array.head
oneMoneyOnrRound monkeys[0] m1
monkeys[0].Items <- Array.skip 1  monkeys[0].Items

printAll "After two Inspections:"
