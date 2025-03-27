program FMXJSONViewer;

uses
  System.StartUpCopy,
  FMX.Forms,
  chimera.viewer.forms.fmx.main in 'chimera.viewer.forms.fmx.main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
