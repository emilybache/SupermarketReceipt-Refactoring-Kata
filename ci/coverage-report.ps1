$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
$solution = Join-Path $repoRoot "src\SupermarketReceipt.sln"
$resultsRoot = Join-Path $repoRoot "TestResults"
$htmlReportDir = Join-Path $resultsRoot "coverage-report"
$summaryHtml = Join-Path $resultsRoot "coverage-summary.html"

function Format-Percent {
    param([double] $Value)

    return "{0:N2}%" -f ($Value * 100)
}

function HtmlEncode {
    param([string] $Value)

    return [System.Net.WebUtility]::HtmlEncode($Value)
}

if (-not (Test-Path $solution)) {
    throw "Solution not found: $solution"
}

if (Test-Path $resultsRoot) {
    Remove-Item -Recurse -Force $resultsRoot
}

New-Item -ItemType Directory -Force -Path $resultsRoot | Out-Null

Write-Host "Repository root: $repoRoot"
Write-Host "Solution:        $solution"
Write-Host "Results folder:  $resultsRoot"
Write-Host ""

Write-Host "Running tests with coverage..."
& dotnet test $solution `
    --collect:"XPlat Code Coverage" `
    --results-directory $resultsRoot `
    --logger "trx;LogFileName=test-results.trx"

if ($LASTEXITCODE -ne 0) {
    exit $LASTEXITCODE
}

$coverageFiles = Get-ChildItem -Path $resultsRoot -Recurse -Filter "coverage.cobertura.xml" |
    Sort-Object LastWriteTime -Descending

if (-not $coverageFiles) {
    throw @"
Coverage file was not created.

This script does not install or add packages. The 'XPlat Code Coverage' collector must already be available to the test project/environment.
Expected file pattern: $resultsRoot\**\coverage.cobertura.xml
"@
}

$coverageFile = $coverageFiles[0].FullName
Write-Host "Coverage XML:    $coverageFile"

$reportGenerator = Get-Command "reportgenerator" -ErrorAction SilentlyContinue
if ($reportGenerator) {
    Write-Host "Generating full HTML report with reportgenerator..."
    & reportgenerator `
        "-reports:$coverageFile" `
        "-targetdir:$htmlReportDir" `
        "-reporttypes:Html;TextSummary"

    if ($LASTEXITCODE -ne 0) {
        exit $LASTEXITCODE
    }
}
else {
    Write-Host "reportgenerator not found. Skipping full HTML report."
    Write-Host "A simple HTML summary will still be created."
}

[xml] $coverageXml = Get-Content -Path $coverageFile
$coverage = $coverageXml.coverage
$lineRate = [double] $coverage."line-rate"
$branchRate = [double] $coverage."branch-rate"

$classRows = foreach ($package in $coverage.packages.package) {
    foreach ($class in $package.classes.class) {
        $classLineRate = [double] $class."line-rate"
        $classBranchRate = [double] $class."branch-rate"
        "<tr><td>$(HtmlEncode $class.filename)</td><td>$(Format-Percent $classLineRate)</td><td>$(Format-Percent $classBranchRate)</td></tr>"
    }
}

$html = @"
<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>SupermarketReceipt Coverage Summary</title>
  <style>
    body { font-family: Segoe UI, Arial, sans-serif; margin: 32px; color: #222; }
    h1 { margin-bottom: 4px; }
    .meta { color: #666; margin-bottom: 24px; }
    .summary { display: flex; gap: 16px; margin-bottom: 24px; }
    .card { border: 1px solid #ddd; border-radius: 8px; padding: 16px 24px; min-width: 160px; }
    .value { font-size: 28px; font-weight: 600; }
    table { border-collapse: collapse; width: 100%; }
    th, td { border: 1px solid #ddd; padding: 8px 10px; text-align: left; }
    th { background: #f4f4f4; }
  </style>
</head>
<body>
  <h1>SupermarketReceipt Coverage Summary</h1>
  <div class="meta">Generated from $(HtmlEncode $coverageFile)</div>
  <div class="summary">
    <div class="card"><div>Line coverage</div><div class="value">$(Format-Percent $lineRate)</div></div>
    <div class="card"><div>Branch coverage</div><div class="value">$(Format-Percent $branchRate)</div></div>
  </div>
  <h2>Classes</h2>
  <table>
    <thead><tr><th>File</th><th>Line coverage</th><th>Branch coverage</th></tr></thead>
    <tbody>
      $($classRows -join "`n      ")
    </tbody>
  </table>
</body>
</html>
"@

Set-Content -Path $summaryHtml -Value $html -Encoding UTF8

Write-Host ""
Write-Host "Simple summary:  $summaryHtml"

$fullReportIndex = Join-Path $htmlReportDir "index.html"
if (Test-Path $fullReportIndex) {
    Write-Host "Full HTML:       $fullReportIndex"
    Start-Process $fullReportIndex
}
else {
    Start-Process $summaryHtml
}
