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
  TfrmMain = class(TForm)
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
    procedure ViewerOpenURL(Sender: TObject; const URL: string);
  private
    FFiles : TDictionary<string, IJSONObject>;
    procedure OpenFile(const Filename : string);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  System.IOUtils,
  {$IF Defined(IOS)}
  macapi.helpers, iOSapi.Foundation, FMX.helpers.iOS
  {$ELSEIF Defined(ANDROID)}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net,
  Androidapi.JNI.App,
  Androidapi.helpers
  {$ELSEIF Defined(MACOS)}
  Posix.Stdlib
  {$ELSEIF Defined(MSWINDOWS)}
  Winapi.ShellAPI, Winapi.Windows
  {$ENDIF}
  ;

type
  TUrlOpen = class
    class procedure Open(URL: string);
  end;


{$R *.fmx}



{ TUrlOpen }

class procedure TUrlOpen.Open(URL: string);
{$IF Defined(ANDROID)}
var
  Intent: JIntent;
{$ENDIF}
begin
{$IF Defined(ANDROID)}
  Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
  Intent.setData(StrToJURI(URL));
  tandroidhelper.Activity.startActivity(Intent);
  // SharedActivity.startActivity(Intent);
{$ELSEIF Defined(MSWINDOWS)}
  ShellExecute(0, 'OPEN', PWideChar(URL), nil, nil, SW_SHOWNORMAL);
{$ELSEIF Defined(IOS)}
  SharedApplication.OpenURL(StrToNSUrl(URL));
{$ELSEIF Defined(MACOS)}
  _system(PAnsiChar('open ' + AnsiString(URL)));
{$ENDIF}
end;


{ TfrmMain }

procedure TfrmMain.btnOpenClick(Sender: TObject);
begin
  if dlgOpen.execute then
  begin
    for var fn in dlgOpen.Files do
      OpenFile(fn);
  end;
end;

procedure TfrmMain.chkSortedChange(Sender: TObject);
begin
  Viewer.Sorted := chkSorted.IsChecked;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FFiles := TDictionary<string, IJSONObject>.Create;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FFiles.Free;
end;

procedure TfrmMain.OpenFile(const Filename: string);
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

procedure TfrmMain.tsFilesChange(Sender: TObject);
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


procedure TfrmMain.ViewerOpenURL(Sender: TObject; const URL: string);
begin
  TUrlOpen.Open(URL);
end;

end.
