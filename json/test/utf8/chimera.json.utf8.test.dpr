program chimera.json.utf8.test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  chimera.json.utf8.tests,
  chimera.json.pool.tests;

begin
  ExitCode := RunUtf8SaveTests;
  if ExitCode <> 0 then
    Exit;
  ExitCode := RunPoolTests;
end.
