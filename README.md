# AdventOfCode2022

See https://adventofcode.com/2022

One folder per day.

# Advent of Code 2023

See https://adventofcode.com/2023

Five days per folder, with one solution (but not all days/parts use F# projects, but rather FSI).


# Helpful Information

## F# Interactive

Release run:

    dotnet fsi <file.fsx> <args>

Debug run, with `DEBUG` defined (so `assert` is executed):

    dotnet fsi --debug+ --define:DEBUG <file.fsx> <args>


## Building Projects

### Create solution

    dotnet new sln -o <folder>

will create

    ./name/name.sln

(folder can, of course, be renamed). Also use `-n <sln-name>` to explicitly name the solution file, otherwise output (`-o`) folder name is used.

### Create project

    dotnet new console -lang "f#" -o <name>

will create

    ./name/name.fsproj
    ./name/program.fs

and add to the solution (current folder must be that of the solution file, or need to specify the solution file):

    dotnet sln add <path-to-fsproj>

If there are any folders, beyond the one containing the project file, they are added to the solution as solution folders, alternately:

    dotnet sln <sln-file> add <path-to-fsproj>
