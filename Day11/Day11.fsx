#time "on"

open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

type Operation = | Add of int
                 | Multiply of int
                 | Sequare

type Money = {
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
        Operation = Multiply(10);
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
        Operation = Sequare;
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

