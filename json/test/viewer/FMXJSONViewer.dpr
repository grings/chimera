program FMXJSONViewer;

uses
  System.StartUpCopy,
  FMX.Forms,
  chimera.viewer.forms.fmx.main in 'chimera.viewer.forms.fmx.main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
