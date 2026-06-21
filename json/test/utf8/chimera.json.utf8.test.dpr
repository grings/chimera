program chimera.json.utf8.test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  chimera.json.utf8.tests;

begin
  ExitCode := RunUtf8SaveTests;
end.
