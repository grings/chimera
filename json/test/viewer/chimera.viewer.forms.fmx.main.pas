unit chimera.viewer.forms.fmx.main;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.TabControl,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  chimera.json,
  chimera.json.fmx.viewer;

type
  TForm1 = class(TForm)
    loTop: TLayout;
    btnOpen: TButton;
    tsFiles: TTabControl;
    dlgOpen: TOpenDialog;
    chkSorted: TCheckBox;
    Viewer: TJSONViewer;
    procedure btnOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tsFilesChange(Sender: TObject);
    procedure chkSortedChange(Sender: TObject);
  private
    FFiles : TDictionary<string, IJSONObject>;
    procedure OpenFile(const Filename : string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.IOUtils;

{$R *.fmx}

procedure TForm1.btnOpenClick(Sender: TObject);
begin
  if dlgOpen.execute then
  begin
    for var fn in dlgOpen.Files do
      OpenFile(fn);
  end;
end;

procedure TForm1.chkSortedChange(Sender: TObject);
begin
  Viewer.Sorted := chkSorted.IsChecked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FFiles := TDictionary<string, IJSONObject>.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FFiles.Free;
end;

procedure TForm1.OpenFile(const Filename: string);
begin
  if FFiles.ContainsKey(Filename) then
  begin
    for var i := 0 to tsFiles.TabCount-1 do
    begin
      if tsFiles.Tabs[i].Text = Filename then
      begin
        tsFiles.ActiveTab := tsFiles.Tabs[i];
      end;
    end;
  end else
  begin
    FFiles.Add(Filename, TJSON.FromFile(Filename));
    var tab := tsFiles.Add;

    var sFilename := Filename;
    var sEXEPath := ExtractFilePath(ParamStr(0));
    if sFilename.StartsWith(sEXEPath) then
      sFilename := sFilename.SubString(sEXEPath.Length);

    tab.Text := sFilename;
    Viewer.JSON := FFiles[Filename];
  end;
end;

procedure TForm1.tsFilesChange(Sender: TObject);
begin
  if FFiles.ContainsKey(tsFiles.ActiveTab.text) then
  begin
    var sFilename := tsFiles.ActiveTab.text;
    if TPath.IsRelativePath(sFilename) then
      sFilename := ExtractFilePath(ParamStr(0))+sFilename;
    Viewer.JSON := FFiles[sFilename];
    Caption :=  'JSON Viewer - '+ExtractFilename(tsFiles.ActiveTab.text);
  end;
end;

end.
