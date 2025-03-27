unit chimera.viewer.forms.vcl.main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellAPI,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Tabs,
  chimera.json.vcl.viewer,
  chimera.json;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    btnOpen: TButton;
    dlgOpen: TOpenDialog;
    tsFiles: TTabSet;
    Viewer: TJSONViewer;
    chkSorted: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tsFilesChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure chkSortedClick(Sender: TObject);
  private
    FFiles : TDictionary<string, IJSONObject>;

    procedure OpenFile(const Filename : string);
    procedure OnURLClick(Sender: TObject; const URL: string);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  System.IOUtils;

{$R *.dfm}

procedure TfrmMain.btnOpenClick(Sender: TObject);
begin
  if dlgOpen.execute then
  begin
    for var fn in dlgOpen.Files do
      OpenFile(fn);
  end;
end;

procedure TfrmMain.chkSortedClick(Sender: TObject);
begin
  Viewer.Sorted := chkSorted.Checked;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FFiles := TDictionary<string, IJSONObject>.Create;
  Viewer.OnOpenURL := OnURLClick;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FFiles.Free;
end;

procedure TfrmMain.OnURLClick(Sender: TObject; const URL: string);
begin
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmMain.OpenFile(const Filename: string);
begin
  if FFiles.ContainsKey(Filename) then
  begin
    tsFiles.TabIndex := tsFiles.Tabs.IndexOf(Filename);
  end else
  begin
    FFiles.Add(Filename, TJSON.FromFile(Filename));
    var sFilename := Filename;
    var sEXEPath := ExtractFilePath(ParamStr(0));
    if sFilename.StartsWith(sEXEPath) then
      sFilename := sFilename.SubString(sEXEPath.Length);

    tsFiles.TabIndex := tsFiles.Tabs.Add(sFilename);
  end;
end;

procedure TfrmMain.tsFilesChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  var sFilename := tsFiles.Tabs[NewTab];
  if TPath.IsRelativePath(sFilename) then
    sFilename := ExtractFilePath(ParamStr(0))+sFilename;
  Viewer.JSON := FFiles[sFilename];
  Caption :=  'JSON Viewer - '+ExtractFilename(tsFiles.Tabs[NewTab]);
end;

end.
