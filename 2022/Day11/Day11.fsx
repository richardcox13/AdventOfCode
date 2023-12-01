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

let realInput = [|
    {   // 0
        Items = [|65; 58; 93; 57; 66|];
        Inspections = 0;
        Operation = Multiply(7);
        TestDivisor = 19;
        TestPass = 6;
        TestFail = 4;
    };
    {   // 1
        Items = [| 76; 97; 58; 72; 57; 92; 82 |];
        Inspections = 0;
        Operation = Add(4);
        TestDivisor = 3;
        TestPass = 7;
        TestFail = 5;
    };
    {   // 2
        Items = [| 90; 89; 96 |];
        Inspections = 0;
        Operation = Multiply(5);
        TestDivisor = 13;
        TestPass = 5;
        TestFail = 1;
    };
    {   // 3
        Items = [| 72; 63; 72; 99 |];
        Inspections = 0;
        Operation = Square;
        TestDivisor = 17;
        TestPass = 0;
        TestFail = 4;
    };
    {   // 4
        Items = [| 65 |];
        Inspections = 0;
        Operation = Add(1);
        TestDivisor = 2;
        TestPass = 6;
        TestFail = 2;
    };
    {   // 5
        Items = [| 97; 71 |];
        Inspections = 0;
        Operation = Add(8);
        TestDivisor = 11;
        TestPass = 7;
        TestFail = 3;
    };
    {   // 6
        Items = [| 83; 68; 88; 55; 87; 67 |];
        Inspections = 0;
        Operation = Add(2);
        TestDivisor = 5;
        TestPass = 2;
        TestFail = 1;
    };
    {   // 7
        Items = [| 64; 81; 50; 96; 82; 53; 62; 92 |];
        Inspections = 0;
        Operation = Add(5);
        TestDivisor = 7;
        TestPass = 3;
        TestFail = 0;
    };
|]

let monkeys = realInput // testInput

let oneMonkeyOneItem (m : Monkey) item =
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

let oneMonkeyOneRound (m: Monkey) =
    let items = m.Items
    m.Items <- [||]
    items |> Seq.iter (fun item -> oneMonkeyOneItem m item)

let oneRound () =
    monkeys |> Seq.iter oneMonkeyOneRound

let printMoney n m =
    printfn "Monkey #%d: Inspections: %d" n m.Inspections
    printfn "  Items: %s" (m.Items |> Seq.map (fun x -> x.ToString()) |> String.concat ", ")

let printAll msg =
    printfn ""
    printfn "%s" msg
    for m in (monkeys |> Seq.mapi (fun idx m -> (idx, m))) do
        printMoney (fst m) (snd m)

printAll "Start:"

for x in 1..20 do
    oneRound ()

printAll "After 20 rounds:"

let topTwoInspections = monkeys
                            |> Seq.sortByDescending (fun m -> m.Inspections)
                            |> Seq.take 2
                            |> Seq.map (fun m -> m.Inspections)
                            |> Seq.toArray


printfn "Monkey business = %d (= %d x %d)"
    (topTwoInspections[0] * topTwoInspections[1])
    topTwoInspections[0] topTwoInspections[1] 
