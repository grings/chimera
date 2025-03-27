program JSONViewer;

uses
  Vcl.Forms,
  chimera.viewer.forms.vcl.main in 'chimera.viewer.forms.vcl.main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
