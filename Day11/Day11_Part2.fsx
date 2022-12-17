#time "on"

open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

type Operation = | Add of int
                 | Multiply of int
                 | Square

type MonkeyInit = {
    // Use queue to pop from front as money examines and throws the items
    // and push on the back when caught
    mutable Items: int[];
    Operation: Operation;
    TestDivisor: int;
    TestPass: int;
    TestFail: int;
}

type Monkey = {
    // Use queue to pop from front as money examines and throws the items
    // and push on the back when caught
    mutable Items: Dictionary<int, int64>[];
    mutable Inspections: int;
    Operation: Operation;
    TestDivisor: int64;
    TestPass: int;
    TestFail: int;
}


let testInput = [|
    {
        Items = [| 79; 98 |];
        Operation = Multiply(19);
        TestDivisor = 23;
        TestPass = 2;
        TestFail = 3
    };
    {
        Items = [| 54; 65; 75; 74 |];
        Operation = Add(6);
        TestDivisor = 19;
        TestPass = 2;
        TestFail = 0
    };
    {
        Items = [| 79; 60; 97 |];
        Operation = Square;
        TestDivisor = 13;
        TestPass = 1;
        TestFail = 3
    };
    {
        Items = [| 74 |];
        Operation = Add(3);
        TestDivisor = 17;
        TestPass = 0;
        TestFail = 1
    };
|]

let realInput = [|
    {   // 0
        Items = [|65; 58; 93; 57; 66|];
        Operation = Multiply(7);
        TestDivisor = 19;
        TestPass = 6;
        TestFail = 4;
    };
    {   // 1
        Items = [| 76; 97; 58; 72; 57; 92; 82 |];
        Operation = Add(4);
        TestDivisor = 3;
        TestPass = 7;
        TestFail = 5;
    };
    {   // 2
        Items = [| 90; 89; 96 |];
        Operation = Multiply(5);
        TestDivisor = 13;
        TestPass = 5;
        TestFail = 1;
    };
    {   // 3
        Items = [| 72; 63; 72; 99 |];
        Operation = Square;
        TestDivisor = 17;
        TestPass = 0;
        TestFail = 4;
    };
    {   // 4
        Items = [| 65 |];
        Operation = Add(1);
        TestDivisor = 2;
        TestPass = 6;
        TestFail = 2;
    };
    {   // 5
        Items = [| 97; 71 |];
        Operation = Add(8);
        TestDivisor = 11;
        TestPass = 7;
        TestFail = 3;
    };
    {   // 6
        Items = [| 83; 68; 88; 55; 87; 67 |];
        Operation = Add(2);
        TestDivisor = 5;
        TestPass = 2;
        TestFail = 1;
    };
    {   // 7
        Items = [| 64; 81; 50; 96; 82; 53; 62; 92 |];
        Operation = Add(5);
        TestDivisor = 7;
        TestPass = 3;
        TestFail = 0;
    };
|]


let makeMoneys (mis: MonkeyInit[]) =
    let diviors = mis |> Array.map (fun mi -> mi.TestDivisor)
    
    let makeOneMoney (mi: MonkeyInit) =
        let makeOneItem item =
            let moduli  = diviors |> Seq.map (fun d -> KeyValuePair(d, int64 (item % d)))
            Dictionary<int, int64>(moduli)
        let items = mi.Items |> Array.map makeOneItem
        {
            Items = items;
            Inspections = 0;
            Operation = mi.Operation;
            TestDivisor = int64 mi.TestDivisor;
            TestPass = mi.TestPass;
            TestFail = mi.TestFail
        }

    mis |> Array.map makeOneMoney


let monkeys =  makeMoneys (* realInput // *) testInput

let printMonkey n m =
    printfn "Monkey #%d: Inspections: %d" n m.Inspections
    printfn "  Items: %s" (m.Items |> Seq.map (fun x -> sprintf "%A" x) |> String.concat ", ")

printMonkey 0 monkeys[0]
