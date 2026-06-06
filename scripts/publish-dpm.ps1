#Requires -Version 5.1
<#
.SYNOPSIS
  Pack and push sivv.chimera to the DPM public gallery.

.DESCRIPTION
  1. Resolves the package version from the current release/* git tag (see below).
  2. Runs `dpm pack` on the package spec into dist\dpm (by default).
  3. Pushes matching .dpkg files using DPM_API_KEY (skips duplicates by default).

  Version resolution:
  - If HEAD has a release/* tag (e.g. release/3.0.2), use that.
  - Otherwise use the highest semver among all release/* tags.

.PARAMETER PackOnly
  Pack only; do not push (no API key required).

.PARAMETER PushOnly
  Push existing .dpkg files only; skip pack.

.PARAMETER PackageDir
  Output folder for pack and source folder for push (default: dist\dpm).

.PARAMETER SpecFile
  Path to the .dspec.yaml file (default: sivv.chimera.dspec.yaml at repo root).

.PARAMETER Version
  Override the version from the release/* tag (passed to `dpm pack -version=...`).

.PARAMETER Source
  Registered DPM source name for the public gallery (default: delphi.dev). The script
  adds this source to your dpm.config if it is missing.

.PARAMETER SkipDuplicate
  Pass -skipDuplicate when pushing (default: true).

.EXAMPLE
  $env:DPM_API_KEY = 'your-api-key'
  .\scripts\publish-dpm.ps1

.EXAMPLE
  .\scripts\publish-dpm.ps1 -PackOnly

.EXAMPLE
  .\scripts\publish-dpm.ps1 -PushOnly -SkipDuplicate $false
#>
[CmdletBinding(DefaultParameterSetName = 'PackAndPush')]
param(
  [Parameter(ParameterSetName = 'PackOnly')]
  [switch] $PackOnly,

  [Parameter(ParameterSetName = 'PushOnly')]
  [switch] $PushOnly,

  [string] $PackageDir = '',
  [string] $SpecFile = '',
  [string] $Version = '',
  [string] $Source = 'delphi.dev',
  [bool] $SkipDuplicate = $true
)

$ErrorActionPreference = 'Stop'

$PackageId = 'sivv.chimera'

$DefaultGallerySourceName = 'delphi.dev'
$DefaultGallerySourceUrl = 'https://delphi.dev/api/v2/index.json'

$repoRoot = Split-Path -Parent $PSScriptRoot

if ([string]::IsNullOrWhiteSpace($PackageDir)) {
  $PackageDir = Join-Path $repoRoot 'dist\dpm'
}

if ([string]::IsNullOrWhiteSpace($SpecFile)) {
  $SpecFile = Join-Path $repoRoot "$PackageId.dspec.yaml"
}

if (-not (Get-Command dpm -ErrorAction SilentlyContinue)) {
  Write-Error 'dpm.exe not found on PATH. Install the DPM client first.'
}

if (-not (Get-Command git -ErrorAction SilentlyContinue)) {
  Write-Error 'git not found on PATH. Required to resolve release/* version tags.'
}

# Git on Windows may refuse cross-mounted repos (e.g. //Mac/Home/... via Parallels).
# safe.directory is scoped to these git invocations only — not your global git config.
function Invoke-RepoGit {
  param(
    [Parameter(Mandatory = $true, ValueFromRemainingArguments = $true)]
    [string[]] $GitArgs
  )

  $priorEap = $ErrorActionPreference
  $ErrorActionPreference = 'Continue'
  try {
    $output = & git -C $repoRoot -c 'safe.directory=*' @GitArgs 2>&1
    if ($LASTEXITCODE -ne 0) {
      Write-Error ($output | Out-String).Trim()
    }
    return $output
  }
  finally {
    $ErrorActionPreference = $priorEap
  }
}

function Get-VersionFromReleaseTag {
  param([string] $Tag)

  if ($Tag -notmatch '^release/(.+)$') {
    Write-Error "Invalid release tag format: $Tag (expected release/x.y.z)"
  }

  return ($Matches[1] -replace '^v', '')
}

function Get-ReleasePackageVersion {
  $headTag = Invoke-RepoGit tag --points-at HEAD --list 'release/*' |
    Select-Object -First 1

  if (-not [string]::IsNullOrWhiteSpace($headTag)) {
    Write-Host "Using release tag at HEAD: $headTag"
    return Get-VersionFromReleaseTag $headTag.Trim()
  }

  $tags = @(Invoke-RepoGit tag -l 'release/*')
  if ($tags.Count -eq 0) {
    Write-Error 'No release/* tags found. Create one (e.g. release/3.0.2) or pass -Version.'
  }

  $latest = $tags | ForEach-Object {
    $ver = Get-VersionFromReleaseTag $_.Trim()
    [PSCustomObject]@{ Tag = $_; Version = $ver; SortKey = [version]$ver }
  } | Sort-Object SortKey -Descending | Select-Object -First 1

  Write-Host "Using latest release/* tag: $($latest.Tag)"
  return $latest.Version
}

if ([string]::IsNullOrWhiteSpace($Version)) {
  $Version = Get-ReleasePackageVersion
}

Write-Host "Package version: $Version"
Write-Host ''

function Invoke-Dpm {
  param(
    [Parameter(Mandatory = $true)]
    [string[]] $DpmArgs
  )

  $priorEap = $ErrorActionPreference
  $ErrorActionPreference = 'Continue'
  try {
    $output = & dpm @DpmArgs 2>&1
    if ($LASTEXITCODE -ne 0) {
      throw ($output | Out-String).Trim()
    }
    return $output
  }
  finally {
    $ErrorActionPreference = $priorEap
  }
}

function Get-DpmSourcesListText {
  $priorEap = $ErrorActionPreference
  $ErrorActionPreference = 'Continue'
  try {
    $output = & dpm sources List '-format=Detailed' 2>&1
    if ($LASTEXITCODE -ne 0) {
      $output = & dpm sources List 2>&1
    }
    return ($output | ForEach-Object { "$_" }) -join "`n"
  }
  finally {
    $ErrorActionPreference = $priorEap
  }
}

function Test-DpmSourceRegistered {
  param([string] $Name)

  $text = Get-DpmSourcesListText
  $escaped = [regex]::Escape($Name)
  return $text -match "(?im)(?:^|\r?\n)\s*(?:name\s*:\s*)?$escaped\s*(?:\r?\n|$|\[)"
}

function Find-DpmGallerySourceName {
  param([string] $GalleryHost = 'delphi.dev')

  $lines = @(Get-DpmSourcesListText -split "`n")
  $currentName = ''
  foreach ($line in $lines) {
    if ($line -match '^\s*(?:name)\s*:\s*(.+)\s*$') {
      $currentName = $Matches[1].Trim()
      continue
    }
    if (($line -match '^\s*(?:source|url)\s*:\s*(.+)\s*$') -and ($Matches[1] -match [regex]::Escape($GalleryHost))) {
      if (-not [string]::IsNullOrWhiteSpace($currentName)) {
        return $currentName
      }
    }
  }
  return ''
}

function Get-DpmRegisteredSourceUrl {
  param([string] $Name)

  $lines = @(Get-DpmSourcesListText -split "`n")
  $currentName = ''
  foreach ($line in $lines) {
    if ($line -match '^\s*(?:name)\s*:\s*(.+)\s*$') {
      $currentName = $Matches[1].Trim()
      continue
    }
    if ($currentName -ieq $Name) {
      if ($line -match '^\s*(?:source|url)\s*:\s*(.+)\s*$') {
        return $Matches[1].Trim()
      }
    }
  }
  return ''
}

function Update-DpmGallerySource {
  param(
    [string] $Name,
    [string] $Url
  )

  try {
    Invoke-Dpm -DpmArgs @('sources', 'Update', "-name=$Name", "-source=$Url", '-type=DPMServer') | Out-Null
  }
  catch {
    if ($_.Exception.Message -notmatch 'not found|does not exist|unknown') {
      throw
    }
  }
}

function Test-DpmGalleryConnectivity {
  param([string] $Url)

  $uri = [Uri]$Url
  Write-Host "Checking gallery connectivity: $($uri.Host) ..."

  $priorEap = $ErrorActionPreference
  $ErrorActionPreference = 'Continue'
  try {
    $dns = Resolve-DnsName $uri.Host -ErrorAction Stop | Select-Object -First 1
    Write-Host "  DNS: $($uri.Host) -> $($dns.IPAddress)"
  }
  catch {
    Write-Error @"
Cannot resolve gallery host '$($uri.Host)' from this machine.

This is a network/DNS problem in the environment running dpm (not the package).
Try from the same PowerShell session:
  Resolve-DnsName $($uri.Host)
  Test-NetConnection $($uri.Host) -Port 443

Common fixes (Parallels / corporate networks):
  - Use host DNS or set Windows DNS to 1.1.1.1 / 8.8.8.8
  - Disable VPN/proxy blocking delphi.dev
  - Push from macOS or another network if the Windows VM cannot reach the internet
"@
  }
  finally {
    $ErrorActionPreference = $priorEap
  }

  $priorEap = $ErrorActionPreference
  $ErrorActionPreference = 'Continue'
  try {
    $response = Invoke-WebRequest -Uri $Url -UseBasicParsing -TimeoutSec 30
    if ($response.StatusCode -lt 200 -or $response.StatusCode -ge 300) {
      Write-Error "Gallery index returned HTTP $($response.StatusCode) for $Url"
    }
    Write-Host "  HTTP: $Url OK"
  }
  catch {
    Write-Error "Cannot download gallery index from $Url : $($_.Exception.Message)"
  }
  finally {
    $ErrorActionPreference = $priorEap
  }
}

function Enable-DpmSource {
  param([string] $Name)

  try {
    Invoke-Dpm -DpmArgs @('sources', 'Enable', "-name=$Name") | Out-Null
  }
  catch {
    if ($_.Exception.Message -notmatch 'already enabled|is already enabled') {
      throw
    }
  }
}

function Ensure-DpmGallerySource {
  param(
    [string] $Name,
    [string] $Url
  )

  if (Test-DpmSourceRegistered -Name $Name) {
    $registeredUrl = Get-DpmRegisteredSourceUrl -Name $Name
    if ($registeredUrl -and ($registeredUrl -ne $Url)) {
      Write-Host "Updating DPM source '$Name' URL: $registeredUrl -> $Url"
      Update-DpmGallerySource -Name $Name -Url $Url
    }
    Enable-DpmSource -Name $Name
    return $Name
  }

  $existing = Find-DpmGallerySourceName
  if (-not [string]::IsNullOrWhiteSpace($existing)) {
    Write-Host "Using existing DPM gallery source: $existing"
    Update-DpmGallerySource -Name $existing -Url $Url
    Enable-DpmSource -Name $existing
    return $existing
  }

  Write-Host "Registering DPM source '$Name' -> $Url"
  try {
    Invoke-Dpm -DpmArgs @('sources', 'Add', "-name=$Name", "-source=$Url", '-type=DPMServer') | Out-Null
  }
  catch {
    if ($_.Exception.Message -notmatch 'already exists') {
      throw
    }
    Write-Host "DPM source '$Name' is already registered"
    Update-DpmGallerySource -Name $Name -Url $Url
  }

  Enable-DpmSource -Name $Name
  return $Name
}

function Get-VersionedDpmPackages {
  if (-not (Test-Path -LiteralPath $PackageDir)) {
    return @()
  }

  $versionPattern = '-{0}\.dpkg$' -f [regex]::Escape($Version)
  return @(Get-ChildItem -LiteralPath $PackageDir -Filter "$PackageId-*.dpkg" -File |
    Where-Object { $_.Name -match $versionPattern } |
    Sort-Object Name)
}

function Repair-DpmGalleryPackageSpec {
  param([string] $PackagePath)

  Add-Type -AssemblyName System.IO.Compression
  Add-Type -AssemblyName System.IO.Compression.FileSystem

  $zip = [System.IO.Compression.ZipFile]::Open($PackagePath, 'Update')
  try {
    $entry = $zip.GetEntry('package.dspec.yaml')
    if ($null -eq $entry) {
      Write-Warning "No package.dspec.yaml in $PackagePath"
      return
    }

    $reader = New-Object System.IO.StreamReader($entry.Open())
    $yaml = $reader.ReadToEnd()
    $reader.Close()
    $entry.Delete()

    # DPM 0.9 writes lowercase framework enums; delphi.dev expects VCL|FMX|None.
    $yaml = $yaml -replace '(?m)^(\s*-\s*)vcl\s*$', '$1VCL'
    $yaml = $yaml -replace '(?m)^(\s*-\s*)fmx\s*$', '$1FMX'
    $yaml = $yaml -replace '(?m)^(\s*-\s*)none\s*$', '$1None'
    $yaml = $yaml -replace '(?ms)^  frameworks:\s*\r?\n(?:\s*-\s*\S+\s*\r?\n)+', ''

    $newEntry = $zip.CreateEntry('package.dspec.yaml')
    $writer = New-Object System.IO.StreamWriter($newEntry.Open())
    try {
      $writer.Write($yaml)
    }
    finally {
      $writer.Close()
    }
  }
  finally {
    $zip.Dispose()
  }
}

function Repair-DpmGalleryPackages {
  $packages = Get-VersionedDpmPackages
  if ($packages.Count -eq 0) {
    return
  }

  Write-Host "Repairing embedded package.dspec.yaml for gallery schema ..."
  foreach ($pkg in $packages) {
    Repair-DpmGalleryPackageSpec -PackagePath $pkg.FullName
  }
  Write-Host ''
}

function Invoke-DpmPack {
  if (-not (Test-Path -LiteralPath $SpecFile)) {
    Write-Error "Spec file not found: $SpecFile"
  }

  if (-not (Test-Path -LiteralPath $PackageDir)) {
    New-Item -ItemType Directory -Path $PackageDir -Force | Out-Null
  }

  $packArgs = @(
    'pack',
    $SpecFile,
    "-o=$PackageDir",
    "-version=$Version"
  )

  Write-Host "Packing $SpecFile -> $PackageDir"
  Invoke-Dpm -DpmArgs $packArgs | Out-Null
  Repair-DpmGalleryPackages
  Write-Host ''
}

function Invoke-DpmPush {
  $apiKey = $env:DPM_API_KEY
  if ([string]::IsNullOrWhiteSpace($apiKey)) {
    Write-Error 'Set the DPM_API_KEY environment variable before pushing.'
  }

  $sourceName = $Source
  $sourceUrl = $DefaultGallerySourceUrl
  if ($sourceName -match '^https?://') {
    $sourceUrl = $sourceName
    if ($sourceUrl -notmatch 'index\.json$') {
      $sourceUrl = $sourceUrl.TrimEnd('/') + '/index.json'
    }
    $sourceName = $DefaultGallerySourceName
  }

  $sourceName = Ensure-DpmGallerySource -Name $sourceName -Url $sourceUrl
  $registeredUrl = Get-DpmRegisteredSourceUrl -Name $sourceName
  if ([string]::IsNullOrWhiteSpace($registeredUrl)) {
    $registeredUrl = $sourceUrl
  }

  Test-DpmGalleryConnectivity -Url $registeredUrl

  if (-not (Test-Path -LiteralPath $PackageDir)) {
    Write-Error "Package directory not found: $PackageDir"
  }

  Repair-DpmGalleryPackages

  $packages = Get-VersionedDpmPackages
  if ($packages.Count -eq 0) {
    Write-Error "No $PackageId-*-$Version.dpkg files in $PackageDir."
  }

  # DPM 0.9 -source is the registered source *name*, not the URL.
  Write-Host "Pushing $($packages.Count) package(s) to source '$sourceName' ($registeredUrl) ..."
  if ($SkipDuplicate) {
    Write-Host '(skip duplicate enabled)'
  }
  Write-Host ''

  $failures = @()

  foreach ($pkg in $packages) {
    $pushArgs = @(
      'push',
      $pkg.FullName,
      "-source=$sourceName",
      "-apiKey=$apiKey"
    )
    if ($SkipDuplicate) {
      $pushArgs += '-skipDuplicate'
    }

    Write-Host "-> $($pkg.Name)"
    try {
      Invoke-Dpm -DpmArgs $pushArgs | Out-Null
    }
    catch {
      Write-Host "   FAILED: $($_.Exception.Message)" -ForegroundColor Red
      if ($_.Exception.Message -match 'Name not resolved|serviceindex|No source named') {
        Write-Host "   Hint: run: dpm sources List -format=Detailed" -ForegroundColor Yellow
        Write-Host "   Source name: $sourceName  URL: $registeredUrl" -ForegroundColor Yellow
      }
      $failures += $pkg.Name
    }
  }

  Write-Host ''
  if ($failures.Count -gt 0) {
    Write-Error "Failed to push: $($failures -join ', ')"
  }

  Write-Host 'All packages pushed successfully.'
}

if (-not $PushOnly) {
  Invoke-DpmPack
}

if (-not $PackOnly) {
  Invoke-DpmPush
}

if ($PackOnly) {
  Write-Host 'Pack complete.'
}
