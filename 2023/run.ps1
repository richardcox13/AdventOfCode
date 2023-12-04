param(
    [int]
    [parameter(mandatory=$true, Position=1)]
    $day,
    [int]
    [parameter(mandatory=$true, Position=2)]
    $part = 15,
    [string]
    [parameter(mandatory=$true, Position=3)]
    $inputFile
)

$cli = "dotnet fsi --debug+ --define:DEBUG Day{0:00}_Part{1}.fsx {2}" -f $day,$part,$inputFile

Write-Output $cli
Invoke-Expression $cli
