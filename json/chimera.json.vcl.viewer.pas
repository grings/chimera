unit chimera.json.vcl.viewer;

interface

uses
  System.SysUtils, System.Classes, System.Math, System.Types, System.Generics.Collections,
  Vcl.Controls, Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, Vcl.Imaging.pngimage,
  Winapi.Windows, Winapi.Messages, chimera.json;

type
  TJSONTokenType = (jttBrace, jttKey, jttString, jttNumber, jttKeyword, jttWhitespace);
  TJSONBraceType = (brNone, brOpenObject, brCloseObject, brOpenArray, brCloseArray);

  PJSONToken = ^TJSONToken;
  TJSONToken = record
    TokenType: TJSONTokenType;
    Value: string;
    LineIndex: Integer;
    ColumnIndex: Integer;
  end;

  TJSONLine = class
  private
    FTokens: TList;
    FText: string;
    FLevel: Integer;
    FBraceType: TJSONBraceType;
    FIsCollapsed: Boolean;
    FMatchingLine: Integer;
    FIsVisible: Boolean;
  public
    constructor Create(const AText: string);
    destructor Destroy; override;
    procedure AddToken(TokenType: TJSONTokenType; const Value: string; ColIndex: Integer);
    property Text: string read FText;
    property Tokens: TList read FTokens;
    property Level: Integer read FLevel write FLevel;
    property BraceType: TJSONBraceType read FBraceType write FBraceType;
    property IsCollapsed: Boolean read FIsCollapsed write FIsCollapsed;
    property MatchingLine: Integer read FMatchingLine write FMatchingLine;
    property IsVisible: Boolean read FIsVisible write FIsVisible;
  end;

  /// <summary>
  /// TJSONViewer control for displaying and editing JSON data
  /// </summary>
  TJSONViewer = class(TCustomControl)
  private
    FJSON: IJSONObject;
    FSource: TStrings;
    FColorizeJSON: Boolean;
    FBraceColor: TColor;
    FKeyColor: TColor;
    FStringColor: TColor;
    FNumberColor: TColor;
    FKeywordColor: TColor;
    FLines: TList;
    FLineHeight: Integer;
    FFirstLine: Integer;
    FVisibleLines: Integer;
    FFontName: string;
    FFontSize: Integer;
    FGutterWidth: Integer;
    FShowGutter: Boolean;
    FGutterColor: TColor;
    FGutterBorderColor: TColor;
    FVisibleLineIndices: TList<Integer>;
    FMaxLineWidth: Integer;
    FHScrollPos: Integer;
    FSorted: Boolean;
    
    procedure SetJSON(const Value: IJSONObject);
    procedure SetSource(const Value: TStrings);
    procedure SetColorizeJSON(const Value: Boolean);
    procedure SetShowGutter(const Value: Boolean);
    procedure UpdateDisplay;
    procedure ParseJSON;
    procedure TokenizeJSON;
    procedure CalculateLineHeight;
    procedure CalculateMaxLineWidth;
    procedure SetFontName(const Value: string);
    procedure SetFontSize(const Value: Integer);
    procedure AnalyzeStructure;
    procedure CalculateVisibleLines;
    procedure ToggleCollapse(LineIndex: Integer);
    function GetBraceTypeFromChar(const C: Char): TJSONBraceType;
    function IsExpandableJSON(LineIndex: Integer): Boolean;
    function FindMatchingBrace(StartLineIndex: Integer): Integer;
    procedure DrawPlusMinusIcon(Canvas: TCanvas; X, Y: Integer; IsCollapsed: Boolean);
    procedure UpdateScrollBars;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;

    procedure SetSorted(const Value: Boolean);  protected
    procedure Paint; override;
    procedure Resize; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    /// <summary>
    /// Reloads the JSON from the current string or object
    /// </summary>
    procedure Refresh;
    /// <summary>
    /// The JSON object to display and edit
    /// </summary>
    property JSON: IJSONObject read FJSON write SetJSON;
  published
    /// <summary>
    /// The JSON string to display and edit
    /// </summary>
    property Source: TStrings read FSource write SetSource;
    
    /// <summary>
    /// When true, applies syntax coloring to the JSON text
    /// </summary>
    property ColorizeJSON: Boolean read FColorizeJSON write SetColorizeJSON default True;
    
    /// <summary>
    /// When true, shows a gutter with expand/collapse buttons
    /// </summary>
    property ShowGutter: Boolean read FShowGutter write SetShowGutter default True;
    
    /// <summary>
    /// Font name used for displaying the JSON
    /// </summary>
    property FontName: string read FFontName write SetFontName;
    
    /// <summary>
    /// Font size used for displaying the JSON
    /// </summary>
    property FontSize: Integer read FFontSize write SetFontSize;
    
    /// <summary>
    /// Color for braces, brackets, and commas
    /// </summary>
    property BraceColor: TColor read FBraceColor write FBraceColor default clNavy;
    
    /// <summary>
    /// Color for JSON keys (property names)
    /// </summary>
    property KeyColor: TColor read FKeyColor write FKeyColor default clBlack;
    
    /// <summary>
    /// Color for string values
    /// </summary>
    property StringColor: TColor read FStringColor write FStringColor default clGreen;
    
    /// <summary>
    /// Color for numeric values
    /// </summary>
    property NumberColor: TColor read FNumberColor write FNumberColor default clMaroon;
    
    /// <summary>
    /// Color for keywords (true, false, null)
    /// </summary>
    property KeywordColor: TColor read FKeywordColor write FKeywordColor default clBlue;

    /// <summary>
    /// Properties should be sorted or in natural unspecified order
    /// </summary>
    property Sorted: Boolean read FSorted write SetSorted default False;

        // Standard control properties
    property Align;
    property Anchors;
    property Color default clWindow;
    property Constraints;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Chimera', [TJSONViewer]);
end;

{ TJSONLine }

constructor TJSONLine.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
  FTokens := TList.Create;
  FLevel := 0;
  FBraceType := brNone;
  FIsCollapsed := False;
  FMatchingLine := -1;
  FIsVisible := True;
end;

destructor TJSONLine.Destroy;
var
  i: Integer;
begin
  for i := 0 to FTokens.Count - 1 do
    Dispose(PJSONToken(FTokens[i]));
  FTokens.Free;
  inherited;
end;

procedure TJSONLine.AddToken(TokenType: TJSONTokenType; const Value: string; ColIndex: Integer);
var
  Token: PJSONToken;
begin
  New(Token);
  Token^.TokenType := TokenType;
  Token^.Value := Value;
  Token^.ColumnIndex := ColIndex;
  FTokens.Add(Token);
end;

{ TJSONViewer }

constructor TJSONViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  // Set default properties
  Width := 320;
  Height := 240;
  Color := clWindow;
  
  // Initialize JSON-related members
  FJSON := nil;
  FSource := TStringList.Create;
  FColorizeJSON := True;
  FShowGutter := True;
  
  // Set default colors for syntax highlighting
  FBraceColor := clNavy;
  FKeyColor := clBlack;
  FStringColor := clGreen;
  FNumberColor := clMaroon;
  FKeywordColor := clBlue;
  FGutterColor := clWindow;
  FGutterBorderColor := RGB(200, 200, 200);
  
  // Create lines containers
  FLines := TList.Create;
  FVisibleLineIndices := TList<Integer>.Create;
  
  // Initialize display parameters
  FFirstLine := 0;
  FHScrollPos := 0;
  FMaxLineWidth := 0;
  FFontName := 'Consolas';
  FFontSize := 10;
  Font.Name := FFontName;
  Font.Size := FFontSize;
  FGutterWidth := 20;
  
  // Make sure we get input focus
  TabStop := True;
end;

destructor TJSONViewer.Destroy;
var
  i: Integer;
begin
  // Clean up lines
  for i := 0 to FLines.Count - 1 do
    TJSONLine(FLines[i]).Free;
  FLines.Free;
  FVisibleLineIndices.Free;
  FSource.Free;
  
  inherited;
end;

procedure TJSONViewer.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_VSCROLL or WS_HSCROLL;
end;

procedure TJSONViewer.CreateWnd;
begin
  inherited;
  CalculateLineHeight;
  UpdateDisplay;
end;

procedure TJSONViewer.CalculateLineHeight;
var
  TextSize: TSize;
begin
  Canvas.Font.Name := FFontName;
  Canvas.Font.Size := FFontSize;
  TextSize := Canvas.TextExtent('Wg');
  FLineHeight := TextSize.cy + 2; // Add a bit of padding
  FVisibleLines := (ClientHeight div FLineHeight) + 1;
end;

procedure TJSONViewer.SetFontName(const Value: string);
begin
  if FFontName <> Value then
  begin
    FFontName := Value;
    Font.Name := FFontName;
    CalculateLineHeight;
    UpdateDisplay;
  end;
end;

procedure TJSONViewer.SetFontSize(const Value: Integer);
begin
  if FFontSize <> Value then
  begin
    FFontSize := Value;
    Font.Size := FFontSize;
    CalculateLineHeight;
    UpdateDisplay;
  end;
end;

procedure TJSONViewer.SetJSON(const Value: IJSONObject);
begin
  if FJSON <> Value then
  begin
    FJSON := Value;
    if Assigned(FJSON) then
      if FSorted then
        FSource.Text := FJSON.AsJSON(TWhitespace.sorted)
      else
        FSource.Text := FJSON.AsJSON(TWhitespace.pretty)
    else
      FSource.Clear;
    UpdateDisplay;
  end;
end;

procedure TJSONViewer.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    if Assigned(FJSON) then
    begin
      if FSorted then
        FSource.Text := FJSON.AsJSON(TWhitespace.sorted)
      else
        FSource.Text := FJSON.AsJSON(TWhitespace.pretty);
      UpdateDisplay;
    end;
  end;
end;

procedure TJSONViewer.SetSource(const Value: TStrings);
var
  sJSON : string;
begin
  if FSource <> Value then
  begin
    sJSON := Value.Text;
    if TJSON.IsJSON(sJSON) then
      JSON := TJSON.From(sJSON)
    else
      JSON := nil;
    UpdateDisplay;
  end;
end;

procedure TJSONViewer.SetColorizeJSON(const Value: Boolean);
begin
  if FColorizeJSON <> Value then
  begin
    FColorizeJSON := Value;
    Invalidate;
  end;
end;

procedure TJSONViewer.SetShowGutter(const Value: Boolean);
begin
  if FShowGutter <> Value then
  begin
    FShowGutter := Value;
    Invalidate;
  end;
end;

procedure TJSONViewer.Refresh;
begin
  UpdateDisplay;
end;

procedure TJSONViewer.UpdateDisplay;
begin
  if not FSource.IsEmpty then
  begin
    ParseJSON;
    AnalyzeStructure;
    CalculateVisibleLines;
    CalculateMaxLineWidth;
    
    // Set scrollbar properties
    if HandleAllocated then
    begin
      Resize;
      Invalidate;
    end;
  end
  else
  begin
    if FLines <> nil then
    begin
      while FLines.Count > 0 do
      begin
        TJSONLine(FLines[0]).Free;
        FLines.Delete(0);
      end;
    end;
    FVisibleLineIndices.Clear;
    FMaxLineWidth := 0;
    FHScrollPos := 0;
    if HandleAllocated then
      Invalidate;
  end;
end;

procedure TJSONViewer.ParseJSON;
var
  i: Integer;
begin
  // Clear existing data
  if FLines <> nil then
  begin
    for i := 0 to FLines.Count - 1 do
      TJSONLine(FLines[i]).Free;
    FLines.Clear;
  end;
  
  // Create line objects and tokenize
  for i := 0 to FSource.Count - 1 do
    FLines.Add(TJSONLine.Create(FSource[i]));
  
  // Tokenize the JSON
  TokenizeJSON;
  
  // Reset horizontal scroll position when new content is loaded
  FHScrollPos := 0;
  FMaxLineWidth := 0;
end;

procedure TJSONViewer.TokenizeJSON;
var
  i, j, currentPos: Integer;
  line: TJSONLine;
  lineText: string;
  c: Char;
  tokenStart: Integer;
  inString, inKey: Boolean;
  tokenType: TJSONTokenType;
  token: string;
begin
  // Tokenize each line
  for i := 0 to FLines.Count - 1 do
  begin
    line := TJSONLine(FLines[i]);
    lineText := line.Text;
    currentPos := 1;
    inString := False;
    inKey := False;
    
    while currentPos <= Length(lineText) do
    begin
      c := lineText[currentPos];
      
      // Handle string literals
      if inString then
      begin
        tokenStart := currentPos - 1;
        while (currentPos <= Length(lineText)) do
        begin
          if (lineText[currentPos] = '"') and ((currentPos = 1) or (lineText[currentPos-1] <> '\')) then
          begin
            Inc(currentPos);
            break;
          end;
          Inc(currentPos);
        end;
        
        token := Copy(lineText, tokenStart, currentPos - tokenStart);
        if inKey then
          line.AddToken(jttKey, token, tokenStart)
        else
          line.AddToken(jttString, token, tokenStart);
        
        inString := False;
        inKey := False;
        continue; // Start next token
      end
      else if c = '"' then
      begin
        inString := True;
        
        // Check if this is a key (property name)
        inKey := False;
        for j := currentPos + 1 to Length(lineText) do
        begin
          if j <= Length(lineText) then
          begin
            if lineText[j] = ':' then
            begin
              inKey := True;
              break;
            end
            else if not (lineText[j] in [' ', #9, #10, #13, '"']) then
              break;
          end;
        end;
      end
      // Handle structural elements (braces, brackets, commas, colons)
      else if c in ['{', '}', '[', ']', ',', ':'] then
      begin
        line.AddToken(jttBrace, c, currentPos);
        Inc(currentPos);
        continue;
      end
      // Handle whitespace
      else if c in [' ', #9, #10, #13] then
      begin
        tokenStart := currentPos;
        while (currentPos <= Length(lineText)) and (lineText[currentPos] in [' ', #9, #10, #13]) do
          Inc(currentPos);
        
        token := Copy(lineText, tokenStart, currentPos - tokenStart);
        line.AddToken(jttWhitespace, token, tokenStart);
        continue;
      end
      // Handle numbers
      else if (c in ['0'..'9', '-', '.', 'e', 'E', '+']) then
      begin
        tokenStart := currentPos;
        while (currentPos <= Length(lineText)) and 
              (lineText[currentPos] in ['0'..'9', '-', '.', 'e', 'E', '+']) do
          Inc(currentPos);
        
        token := Copy(lineText, tokenStart, currentPos - tokenStart);
        line.AddToken(jttNumber, token, tokenStart);
        continue;
      end
      // Handle keywords (true, false, null)
      else if c in ['t', 'f', 'n'] then
      begin
        tokenStart := currentPos;
        
        if (currentPos + 3 <= Length(lineText)) and 
           (Copy(lineText, currentPos, 4) = 'true') then
        begin
          line.AddToken(jttKeyword, 'true', tokenStart);
          Inc(currentPos, 4);
          continue;
        end
        else if (currentPos + 4 <= Length(lineText)) and 
                (Copy(lineText, currentPos, 5) = 'false') then
        begin
          line.AddToken(jttKeyword, 'false', tokenStart);
          Inc(currentPos, 5);
          continue;
        end
        else if (currentPos + 3 <= Length(lineText)) and 
                (Copy(lineText, currentPos, 4) = 'null') then
        begin
          line.AddToken(jttKeyword, 'null', tokenStart);
          Inc(currentPos, 4);
          continue;
        end;
      end;
      
      // If we get here, move to next character
      Inc(currentPos);
    end;
  end;
end;

procedure TJSONViewer.Paint;
var
  i, j, x, y: Integer;
  lineIndexInList: Integer;
  actualLineIndex: Integer;
  line: TJSONLine;
  token: PJSONToken;
  textRect, gutterRect, ellipsisRect: TRect;
  isExpandable: Boolean;
  matchingLineIndex: Integer;
  matchingLine: TJSONLine;
  closeBraceStr: string;
  originalX: Integer;
  propertyNameDisplayed: Boolean;
begin
  // First clear the entire background with the same color
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  
  // Check if we have content to draw
  if (FLines = nil) or (FLines.Count = 0) or (FVisibleLineIndices.Count = 0) then
    Exit;
  
  // Draw gutter if enabled
  if FShowGutter then
  begin
    // Only draw the gutter border, not filling with a different color
    Canvas.Pen.Color := FGutterBorderColor;
    Canvas.MoveTo(FGutterWidth, 0);
    Canvas.LineTo(FGutterWidth, ClientHeight);
  end;
  
  // Setup canvas font
  Canvas.Font.Name := FFontName;
  Canvas.Font.Size := FFontSize;
  Canvas.Font.Style := [];
  
  // Apply horizontal scrolling offset
  SetWindowOrgEx(Canvas.Handle, FHScrollPos, 0, nil);
  
  // Draw the visible lines
  y := 0;
  for i := 0 to Min(FVisibleLines, FVisibleLineIndices.Count - FFirstLine) - 1 do
  begin
    lineIndexInList := i + FFirstLine;
    if (lineIndexInList >= 0) and (lineIndexInList < FVisibleLineIndices.Count) then
    begin
      // Get the actual line index from the visible lines list
      actualLineIndex := FVisibleLineIndices[lineIndexInList];
      line := TJSONLine(FLines[actualLineIndex]);
      
      // Draw expand/collapse icon in gutter if needed
      if FShowGutter then
      begin
        isExpandable := IsExpandableJSON(actualLineIndex);
        if isExpandable then
        begin
          DrawPlusMinusIcon(Canvas, (FGutterWidth - 16) div 2, y + (FLineHeight - 16) div 2, line.IsCollapsed);
        end;
      end;
      
      // Draw the text
      x := FGutterWidth + 4;  // Starting position with padding
      originalX := x;
      
      if (not FColorizeJSON) or (line.Tokens.Count = 0) then
      begin
        // Draw the whole line in one color
        Canvas.Font.Color := Font.Color;
        Canvas.TextOut(x, y, line.Text);
      end
      else
      begin
        // Check if this is a collapsed object/array
        if line.IsCollapsed and ((line.BraceType = brOpenObject) or (line.BraceType = brOpenArray)) then
        begin
          // Find matching closing brace
          matchingLineIndex := line.MatchingLine;
          if (matchingLineIndex >= 0) and (matchingLineIndex < FLines.Count) then
          begin
            matchingLine := TJSONLine(FLines[matchingLineIndex]);
            
            // First draw all tokens up to the brace normally to preserve indentation and property name
            propertyNameDisplayed := False;
            for j := 0 to line.Tokens.Count - 1 do
            begin
              token := PJSONToken(line.Tokens[j]);
              
              // Skip the token if it's an opening brace (we'll add it specially later)
              if (token.TokenType = jttBrace) and 
                 ((token.Value = '{') or (token.Value = '[')) then
              begin
                // Remember our position after the property name
                break;
              end;
              
              // Check if we're drawing a property name
              if token.TokenType = jttKey then
                propertyNameDisplayed := True;
              
              // Set the color for this token
              case token^.TokenType of
                jttBrace: Canvas.Font.Color := FBraceColor;
                jttKey: Canvas.Font.Color := FKeyColor;
                jttString: Canvas.Font.Color := FStringColor;
                jttNumber: Canvas.Font.Color := FNumberColor;
                jttKeyword: Canvas.Font.Color := FKeywordColor;
                jttWhitespace: Canvas.Font.Color := Font.Color;
              end;
              
              // Set font style based on token type
              if token^.TokenType in [jttKey, jttKeyword] then
                Canvas.Font.Style := [fsBold]
              else
                Canvas.Font.Style := [];
              
              // Draw the token
              Canvas.TextOut(x, y, token^.Value);
              x := x + Canvas.TextWidth(token^.Value);
            end;
            
            // Now draw the opening brace
            Canvas.Font.Color := FBraceColor;
            Canvas.Font.Style := [];
            
            // Draw the opening brace ('{' or '[')
            if line.BraceType = brOpenObject then
              Canvas.TextOut(x, y, '{')
            else
              Canvas.TextOut(x, y, '[');
              
            x := x + Canvas.TextWidth('{'); // Both braces have same width
            
            // Draw ellipsis in a rectangle
            Canvas.Pen.Color := clGray;
            Canvas.Brush.Style := bsClear;
            ellipsisRect := Rect(x + 4, y + 2, x + 24, y + FLineHeight - 2);
            Canvas.Rectangle(ellipsisRect);
            Canvas.TextOut(x + 10, y, '…');
            Canvas.Brush.Style := bsSolid;
            x := ellipsisRect.Right + 4;
            
            // Draw the closing brace
            closeBraceStr := '}';
            if line.BraceType = brOpenArray then
              closeBraceStr := ']';
              
            Canvas.Font.Color := FBraceColor;
            Canvas.TextOut(x, y, closeBraceStr);
          end;
        end
        else
        begin
          // Normal drawing of all tokens
          for j := 0 to line.Tokens.Count - 1 do
          begin
            token := PJSONToken(line.Tokens[j]);
            
            // Set the color for this token
            case token^.TokenType of
              jttBrace: Canvas.Font.Color := FBraceColor;
              jttKey: Canvas.Font.Color := FKeyColor;
              jttString: Canvas.Font.Color := FStringColor;
              jttNumber: Canvas.Font.Color := FNumberColor;
              jttKeyword: Canvas.Font.Color := FKeywordColor;
              jttWhitespace: Canvas.Font.Color := Font.Color;
            end;
            
            // Set font style based on token type
            if token^.TokenType in [jttKey, jttKeyword] then
              Canvas.Font.Style := [fsBold]
            else
              Canvas.Font.Style := [];
            
            // Draw the token
            Canvas.TextOut(x, y, token^.Value);
            x := x + Canvas.TextWidth(token^.Value);
          end;
        end;
      end;
    end;
    
    // Move to next line
    Inc(y, FLineHeight);
  end;
  
  // Reset coordinate system before exiting
  SetWindowOrgEx(Canvas.Handle, 0, 0, nil);
end;

procedure TJSONViewer.Resize;
begin
  inherited;
  CalculateLineHeight;
  // Calculate visible lines
  FVisibleLines := (ClientHeight div FLineHeight) + 1;
  
  // If we created new content, recalculate max width
  if FMaxLineWidth = 0 then
    CalculateMaxLineWidth;
  
  // Update scrollbar 
  UpdateScrollBars;
  
  Invalidate;
end;

procedure TJSONViewer.UpdateScrollBars;
var
  ScrollInfo: TScrollInfo;
  maxLines, maxWidth: Integer;
begin
  if HandleAllocated and (FVisibleLineIndices <> nil) and (FVisibleLineIndices.Count > 0) then
  begin
    // Vertical scrollbar
    maxLines := FVisibleLineIndices.Count;
    
    FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_ALL;
    ScrollInfo.nMin := 0;
    ScrollInfo.nMax := maxLines - 1;
    ScrollInfo.nPage := FVisibleLines;
    ScrollInfo.nPos := FFirstLine;
    
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
    
    // Horizontal scrollbar
    maxWidth := FMaxLineWidth;
    
    // Only show horizontal scrollbar if content wider than client area
    if maxWidth > ClientWidth then
    begin
      FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
      ScrollInfo.cbSize := SizeOf(ScrollInfo);
      ScrollInfo.fMask := SIF_ALL;
      ScrollInfo.nMin := 0;
      ScrollInfo.nMax := maxWidth - 1;
      ScrollInfo.nPage := ClientWidth;
      ScrollInfo.nPos := FHScrollPos;
      
      SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
      ShowScrollBar(Handle, SB_HORZ, True);
    end
    else
    begin
      // Hide horizontal scrollbar if content fits
      FHScrollPos := 0;
      ShowScrollBar(Handle, SB_HORZ, False);
    end;
  end;
end;

procedure TJSONViewer.WMVScroll(var Message: TWMVScroll);
var
  LineScrolled: Boolean;
  newFirstLine: Integer;
begin
  LineScrolled := True;
  newFirstLine := FFirstLine;
  
  case Message.ScrollCode of
    SB_LINEUP:        newFirstLine := FFirstLine - 1;
    SB_LINEDOWN:      newFirstLine := FFirstLine + 1;
    SB_PAGEUP:        newFirstLine := FFirstLine - FVisibleLines;
    SB_PAGEDOWN:      newFirstLine := FFirstLine + FVisibleLines;
    SB_THUMBPOSITION: newFirstLine := Message.Pos;
    SB_THUMBTRACK:    newFirstLine := Message.Pos;
    SB_TOP:           newFirstLine := 0;
    SB_BOTTOM:        newFirstLine := FVisibleLineIndices.Count - FVisibleLines;
    else
      LineScrolled := False;
  end;
  
  if LineScrolled then
  begin
    if newFirstLine < 0 then
      newFirstLine := 0;
      
    if newFirstLine > Max(0, FVisibleLineIndices.Count - FVisibleLines) then
      newFirstLine := Max(0, FVisibleLineIndices.Count - FVisibleLines);
      
    if newFirstLine <> FFirstLine then
    begin
      FFirstLine := newFirstLine;
      UpdateScrollBars;
      Invalidate;
    end;
  end;
  
  Message.Result := 0;
end;

procedure TJSONViewer.WMHScroll(var Message: TWMHScroll);
var
  ScrollPos: Integer;
begin
  ScrollPos := FHScrollPos;
  
  case Message.ScrollCode of
    SB_LINELEFT:      ScrollPos := ScrollPos - 10;
    SB_LINERIGHT:     ScrollPos := ScrollPos + 10;
    SB_PAGELEFT:      ScrollPos := ScrollPos - ClientWidth div 2;
    SB_PAGERIGHT:     ScrollPos := ScrollPos + ClientWidth div 2;
    SB_THUMBPOSITION: ScrollPos := Message.Pos;
    SB_THUMBTRACK:    ScrollPos := Message.Pos;
    SB_LEFT:          ScrollPos := 0;
    SB_RIGHT:         ScrollPos := FMaxLineWidth - ClientWidth;
  end;
  
  if ScrollPos < 0 then
    ScrollPos := 0;
    
  if ScrollPos > Max(0, FMaxLineWidth - ClientWidth) then
    ScrollPos := Max(0, FMaxLineWidth - ClientWidth);
    
  if ScrollPos <> FHScrollPos then
  begin
    FHScrollPos := ScrollPos;
    UpdateScrollBars;
    Invalidate;
  end;
  
  Message.Result := 0;
end;

function TJSONViewer.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  LinesToScroll: Integer;
begin
  // Determine how many lines to scroll
  LinesToScroll := Abs(WheelDelta) div WHEEL_DELTA;
  if LinesToScroll = 0 then LinesToScroll := 1;
  
  // Determine direction
  if WheelDelta > 0 then
    LinesToScroll := -LinesToScroll;
  
  // Adjust first visible line
  FFirstLine := FFirstLine + LinesToScroll;
  
  // Keep within bounds
  if FFirstLine < 0 then
    FFirstLine := 0;
  if FVisibleLineIndices.Count > 0 then
    if FFirstLine > FVisibleLineIndices.Count - FVisibleLines then
      FFirstLine := Max(0, FVisibleLineIndices.Count - FVisibleLines);
  
  // Update scrollbar position
  UpdateScrollBars;
  
  // Repaint
  Invalidate;
  
  Result := True;
end;

procedure TJSONViewer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  lineIndex: Integer;
  realLineIndex: Integer;
  line: TJSONLine;
  tokenX, ellipsisX, braceX: Integer;
  j: Integer;
  token: PJSONToken;
  ellipsisWidth, braceWidth: Integer;
  isEllipsisClick, isBraceClick: Boolean;
  currentX: Integer;
begin
  inherited;
  
  isEllipsisClick := False;
  isBraceClick := False;
  
  // Determine which line was clicked
  lineIndex := Y div FLineHeight;
  
  // Convert to real line index
  if (lineIndex >= 0) and (lineIndex < FVisibleLineIndices.Count - FFirstLine) then
  begin
    realLineIndex := FVisibleLineIndices[lineIndex + FFirstLine];
    
    if (Button = mbLeft) then
    begin
      // Check if click was in the gutter
      if FShowGutter and (X < FGutterWidth) then
      begin
        if IsExpandableJSON(realLineIndex) then
          ToggleCollapse(realLineIndex);
      end
      else
      begin
        line := TJSONLine(FLines[realLineIndex]);
        
        // Check if this is a collapsed object with ellipsis
        if line.IsCollapsed and ((line.BraceType = brOpenObject) or (line.BraceType = brOpenArray)) then
        begin
          // Calculate where the ellipsis and brace would be
          tokenX := FGutterWidth + 4; // Start with padding
          braceX := 0;
          
          // Add up all token widths until we reach the opening brace
          Canvas.Font.Name := FFontName;
          Canvas.Font.Size := FFontSize;
          
          for j := 0 to line.Tokens.Count - 1 do
          begin
            token := PJSONToken(line.Tokens[j]);
            
            if (token.TokenType = jttBrace) and 
              ((token.Value = '{') or (token.Value = '[')) then
            begin
              braceX := tokenX;
              break;
            end;
            
            tokenX := tokenX + Canvas.TextWidth(token.Value);
          end;
          
          // Calculate brace width
          braceWidth := Canvas.TextWidth('{');
          
          // Add brace width to get to ellipsis position
          tokenX := tokenX + braceWidth;
          
          // Calculate ellipsis area
          ellipsisX := tokenX + 4;
          ellipsisWidth := 24;
          
          // Check if click was on ellipsis
          if (X >= ellipsisX) and (X <= ellipsisX + ellipsisWidth) then
          begin
            ToggleCollapse(realLineIndex);
            isEllipsisClick := True;
          end
          // Check if click was on the opening brace
          else if (braceX > 0) and (X >= braceX) and (X <= braceX + braceWidth) then
          begin
            ToggleCollapse(realLineIndex);
            isBraceClick := True;
          end;
        end
        else if not line.IsCollapsed then
        begin
          // If not collapsed, check if user clicked on an opening brace
          currentX := FGutterWidth + 4; // Starting position with padding
          Canvas.Font.Name := FFontName;
          Canvas.Font.Size := FFontSize;
          
          for j := 0 to line.Tokens.Count - 1 do
          begin
            token := PJSONToken(line.Tokens[j]);
            
            // Calculate token width for click detection
            tokenX := currentX;
            currentX := currentX + Canvas.TextWidth(token.Value);
            
            // Check if click was on an opening brace
            if (X >= tokenX) and (X <= currentX) and
               (token.TokenType = jttBrace) and 
               ((token.Value = '{') or (token.Value = '[')) then
            begin
              if IsExpandableJSON(realLineIndex) then
              begin
                ToggleCollapse(realLineIndex);
                isBraceClick := True;
                Break;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TJSONViewer.ToggleCollapse(LineIndex: Integer);
var
  line: TJSONLine;
  matchingLineIndex: Integer;
begin
  if (LineIndex < 0) or (LineIndex >= FLines.Count) then
    Exit;
  
  line := TJSONLine(FLines[LineIndex]);
  if (line.BraceType = brOpenObject) or (line.BraceType = brOpenArray) then
  begin
    // Make sure we have a valid matching line
    if line.MatchingLine = -1 then
      line.MatchingLine := FindMatchingBrace(LineIndex);
      
    matchingLineIndex := line.MatchingLine;
    
    // Toggle collapsed state
    line.IsCollapsed := not line.IsCollapsed;
    
    // Recalculate visible lines
    CalculateVisibleLines;
    
    // Recalculate max line width as the collapsed state might affect it
    CalculateMaxLineWidth;
    
    // Update scrollbars and repaint
    UpdateScrollBars;
    Invalidate;
  end;
end;

function TJSONViewer.FindMatchingBrace(StartLineIndex: Integer): Integer;
var
  i: Integer;
  line, startLine: TJSONLine;
  level: Integer;
  searchForCloseBrace: Boolean;
begin
  Result := -1;
  
  if (StartLineIndex < 0) or (StartLineIndex >= FLines.Count) then
    Exit;
  
  startLine := TJSONLine(FLines[StartLineIndex]);
  searchForCloseBrace := (startLine.BraceType = brOpenObject) or (startLine.BraceType = brOpenArray);
  
  if not searchForCloseBrace then
    Exit;
  
  level := 0;
  
  for i := StartLineIndex to FLines.Count - 1 do
  begin
    line := TJSONLine(FLines[i]);
    
    if i = StartLineIndex then
      level := 1
    else if (line.BraceType = brOpenObject) or (line.BraceType = brOpenArray) then
      Inc(level)
    else if (line.BraceType = brCloseObject) or (line.BraceType = brCloseArray) then
    begin
      Dec(level);
      if level = 0 then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

procedure TJSONViewer.AnalyzeStructure;
var
  i, j: Integer;
  line: TJSONLine;
  braceLevel: Integer;
  hasOpenBrace: Boolean;
  token: PJSONToken;
  braceChar: Char;
  braceType: TJSONBraceType;
begin
  braceLevel := 0;
  
  // First pass: determine levels and brace types
  for i := 0 to FLines.Count - 1 do
  begin
    line := TJSONLine(FLines[i]);
    line.Level := braceLevel;
    hasOpenBrace := False;
    
    // Check tokens to find braces
    for j := 0 to line.Tokens.Count - 1 do
    begin
      token := PJSONToken(line.Tokens[j]);
      if (token.TokenType = jttBrace) and (Length(token.Value) > 0) then
      begin
        braceChar := token.Value[1];
        braceType := GetBraceTypeFromChar(braceChar);
        
        case braceType of
          brOpenObject, brOpenArray:
            begin
              line.BraceType := braceType;
              hasOpenBrace := True;
              Inc(braceLevel);
            end;
          brCloseObject, brCloseArray:
            begin
              Dec(braceLevel);
              if not hasOpenBrace then
                line.Level := braceLevel;
              line.BraceType := braceType;
            end;
        end;
      end;
    end;
  end;
  
  // Second pass: find matching braces
  for i := 0 to FLines.Count - 1 do
  begin
    line := TJSONLine(FLines[i]);
    if (line.BraceType = brOpenObject) or (line.BraceType = brOpenArray) then
      line.MatchingLine := FindMatchingBrace(i);
  end;
end;

procedure TJSONViewer.CalculateVisibleLines;
var
  i, j: Integer;
  line: TJSONLine;
  isVisible: Boolean;
  startLine, endLine: Integer;
begin
  FVisibleLineIndices.Clear;
  
  for i := 0 to FLines.Count - 1 do
  begin
    line := TJSONLine(FLines[i]);
    isVisible := True;
    
    // Check if this line should be hidden due to collapsed parents
    for j := 0 to i - 1 do
    begin
      if TJSONLine(FLines[j]).IsCollapsed then
      begin
        startLine := j;
        endLine := TJSONLine(FLines[j]).MatchingLine;
        
        if (i > startLine) and (i <= endLine) then
        begin
          isVisible := False;
          Break;
        end;
      end;
    end;
    
    line.IsVisible := isVisible;
    if isVisible then
      FVisibleLineIndices.Add(i);
  end;
end;

procedure TJSONViewer.DrawPlusMinusIcon(Canvas: TCanvas; X, Y: Integer; IsCollapsed: Boolean);
var
  IconRect: TRect;
begin
  // Define the icon rectangle
  IconRect := Rect(X, Y, X + 16, Y + 16);
  
  // Draw background and border (use control's background color)
  Canvas.Brush.Color := Color;
  Canvas.FillRect(IconRect);
  Canvas.Pen.Color := FGutterBorderColor;
  Canvas.Rectangle(IconRect);
  
  // Draw the horizontal line (minus sign)
  Canvas.Pen.Color := clBlack;
  Canvas.MoveTo(X + 4, Y + 8);
  Canvas.LineTo(X + 12, Y + 8);
  
  // If collapsed (plus sign), add the vertical line
  if IsCollapsed then
  begin
    Canvas.MoveTo(X + 8, Y + 4);
    Canvas.LineTo(X + 8, Y + 12);
  end;
end;

function TJSONViewer.GetBraceTypeFromChar(const C: Char): TJSONBraceType;
begin
  case C of
    '{': Result := brOpenObject;
    '}': Result := brCloseObject;
    '[': Result := brOpenArray;
    ']': Result := brCloseArray;
  else
    Result := brNone;
  end;
end;

function TJSONViewer.IsExpandableJSON(LineIndex: Integer): Boolean;
var
  line: TJSONLine;
begin
  Result := False;
  if (LineIndex < 0) or (LineIndex >= FLines.Count) then
    Exit;
  
  line := TJSONLine(FLines[LineIndex]);
  Result := (line.BraceType = brOpenObject) or (line.BraceType = brOpenArray);
end;

procedure TJSONViewer.CalculateMaxLineWidth;
var
  i, j, lineWidth: Integer;
  line: TJSONLine;
  token: PJSONToken;
begin
  FMaxLineWidth := 0;
  
  Canvas.Font.Name := FFontName;
  Canvas.Font.Size := FFontSize;
  
  for i := 0 to FLines.Count - 1 do
  begin
    line := TJSONLine(FLines[i]);
    
    if not line.IsVisible then
      Continue;
      
    lineWidth := FGutterWidth + 4; // Start with gutter width and padding
    
    if line.IsCollapsed and ((line.BraceType = brOpenObject) or (line.BraceType = brOpenArray)) then
    begin
      // For collapsed nodes, calculate width including the ellipsis
      for j := 0 to line.Tokens.Count - 1 do
      begin
        token := PJSONToken(line.Tokens[j]);
        
        if (token.TokenType = jttBrace) and 
          ((token.Value = '{') or (token.Value = '[')) then
        begin
          // Add brace width
          lineWidth := lineWidth + Canvas.TextWidth(token.Value);
          
          // Add ellipsis width + padding
          lineWidth := lineWidth + 4 + 24 + 4;
          
          // Add closing brace width
          lineWidth := lineWidth + Canvas.TextWidth(token.Value);
          
          Break;
        end;
        
        lineWidth := lineWidth + Canvas.TextWidth(token.Value);
      end;
    end
    else
    begin
      // For normal lines, add up all token widths
      for j := 0 to line.Tokens.Count - 1 do
      begin
        token := PJSONToken(line.Tokens[j]);
        lineWidth := lineWidth + Canvas.TextWidth(token.Value);
      end;
    end;
    
    // Update max width if this line is wider
    if lineWidth > FMaxLineWidth then
      FMaxLineWidth := lineWidth;
  end;
  
  // Add some padding
  FMaxLineWidth := FMaxLineWidth + 10;
end;

end.
