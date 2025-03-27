unit chimera.json.fmx.viewer;

interface

uses
  System.SysUtils, System.Classes, System.Math, System.Types, System.Generics.Collections, System.UITypes,
  FMX.Controls, FMX.Graphics, FMX.Forms, FMX.Dialogs, FMX.Types, FMX.Layouts, FMX.StdCtrls,
  chimera.json;

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
  TJSONViewer = class(TControl)
  private
    FJSON: IJSONObject;
    FSource: TStrings;
    FColorizeJSON: Boolean;
    FBraceColor: TAlphaColor;
    FKeyColor: TAlphaColor;
    FStringColor: TAlphaColor;
    FNumberColor: TAlphaColor;
    FKeywordColor: TAlphaColor;
    FLines: TList;
    FLineHeight: Single;
    FFirstLine: Integer;
    FVisibleLines: Integer;
    FFontFamily: string;
    FFontSize: Single;
    FGutterWidth: Single;
    FShowGutter: Boolean;
    FGutterColor: TAlphaColor;
    FGutterBorderColor: TAlphaColor;
    FVisibleLineIndices: TList<Integer>;
    FMaxLineWidth: Single;
    FHScrollPos: Single;
    FVScrollBar: TScrollBar;
    FHScrollBar: TScrollBar;
    FMouseDownPos: TPointF;
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
    procedure SetFontFamily(const Value: string);
    procedure SetFontSize(const Value: Single);
    procedure AnalyzeStructure;
    procedure CalculateVisibleLines;
    procedure ToggleCollapse(LineIndex: Integer);
    function GetBraceTypeFromChar(const C: Char): TJSONBraceType;
    function IsExpandableJSON(LineIndex: Integer): Boolean;
    function FindMatchingBrace(StartLineIndex: Integer): Integer;
    procedure DrawPlusMinusIcon(Canvas: TCanvas; X, Y: Single; IsCollapsed: Boolean);
    procedure UpdateScrollBars;
    procedure HScrollChangeHandler(Sender: TObject);
    procedure VScrollChangeHandler(Sender: TObject);

    procedure SetSorted(const Value: Boolean);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoRealign; override;
    
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
    /// Font family used for displaying the JSON
    /// </summary>
    property FontFamily: string read FFontFamily write SetFontFamily;
    
    /// <summary>
    /// Font size used for displaying the JSON
    /// </summary>
    property FontSize: Single read FFontSize write SetFontSize;
    
    /// <summary>
    /// Color for braces, brackets, and commas
    /// </summary>
    property BraceColor: TAlphaColor read FBraceColor write FBraceColor;
    
    /// <summary>
    /// Color for JSON keys (property names)
    /// </summary>
    property KeyColor: TAlphaColor read FKeyColor write FKeyColor;
    
    /// <summary>
    /// Color for string values
    /// </summary>
    property StringColor: TAlphaColor read FStringColor write FStringColor;
    
    /// <summary>
    /// Color for numeric values
    /// </summary>
    property NumberColor: TAlphaColor read FNumberColor write FNumberColor;
    
    /// <summary>
    /// Color for keywords (true, false, null)
    /// </summary>
    property KeywordColor: TAlphaColor read FKeywordColor write FKeywordColor;
    
    /// <summary>
    /// Properties should be sorted or in natural unspecified order
    /// </summary>
    property Sorted: Boolean read FSorted write SetSorted default False;

    // Standard control properties
    property Align;
    property Anchors;
    property ClipChildren;
    property ClipParent;
    property Cursor;
    property DragMode;
    property Enabled;
    property Height;
    property Hint;
    property HitTest;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TabOrder;
    property TabStop;
    property Visible;
    property Width;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnResized;
  end;

procedure Register;

implementation

uses 
  FMX.Platform, FMX.TextLayout, FMX.Effects;

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
  
  // Initialize JSON-related members
  FJSON := nil;
  FSource := TStringList.Create;
  FColorizeJSON := True;
  FShowGutter := True;

  // Set default colors for syntax highlighting
  FBraceColor := TAlphaColorRec.Navy;
  FKeyColor := TAlphaColorRec.Black;
  FStringColor := TAlphaColorRec.Green;
  FNumberColor := TAlphaColorRec.Maroon;
  FKeywordColor := TAlphaColorRec.Blue;
  FGutterColor := TAlphaColorRec.White;
  FGutterBorderColor := $FFD0D0D0; // Light gray

  // Create lines containers
  FLines := TList.Create;
  FVisibleLineIndices := TList<Integer>.Create;

  // Initialize display parameters
  FFirstLine := 0;
  FHScrollPos := 0;
  FMaxLineWidth := 0;
  FFontFamily := 'Courier New';
  FFontSize := 12;
  FGutterWidth := 20;

  // Create scrollbars
  FVScrollBar := TScrollBar.Create(Self);
  FVScrollBar.Parent := Self;
  FVScrollBar.Orientation := TOrientation.Vertical;
  FVScrollBar.Align := TAlignLayout.Right;
  FVScrollBar.Width := 16;
  FVScrollBar.Visible := True;
  FVScrollBar.OnChange := VScrollChangeHandler;
  FVScrollbar.Stored := False;

  FHScrollBar := TScrollBar.Create(Self);
  FHScrollBar.Parent := Self;
  FHScrollBar.Orientation := TOrientation.Horizontal;
  FHScrollBar.Align := TAlignLayout.Bottom;
  FHScrollBar.Height := 16;
  FHScrollBar.Visible := True;
  FHScrollBar.OnChange := HScrollChangeHandler;
  FHScrollbar.Stored := False;

  // Set default properties
  Width := 320;
  Height := 240;

  // Make sure we get input focus
  CanFocus := True;
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
  // Note: FVScrollBar and FHScrollBar will be destroyed by their Parent
  
  inherited;
end;

procedure TJSONViewer.DoRealign;
begin
  inherited;
  UpdateScrollBars;
end;

procedure TJSONViewer.CalculateLineHeight;
var
  TextLayout: TTextLayout;
begin
  TextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    TextLayout.BeginUpdate;
    TextLayout.Font.Family := FFontFamily;
    TextLayout.Font.Size := FFontSize;
    TextLayout.Text := 'Wg';
    TextLayout.EndUpdate;
    
    FLineHeight := TextLayout.Height + 2; // Add a bit of padding
    FVisibleLines := Trunc((Height - FHScrollBar.Height) / FLineHeight) + 1;
  finally
    TextLayout.Free;
  end;
end;

procedure TJSONViewer.SetFontFamily(const Value: string);
begin
  if FFontFamily <> Value then
  begin
    FFontFamily := Value;
    CalculateLineHeight;
    UpdateDisplay;
  end;
end;

procedure TJSONViewer.SetFontSize(const Value: Single);
begin
  if FFontSize <> Value then
  begin
    FFontSize := Value;
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
    UpdateDisplay;
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

procedure TJSONViewer.SetColorizeJSON(const Value: Boolean);
begin
  if FColorizeJSON <> Value then
  begin
    FColorizeJSON := Value;
    Repaint;
  end;
end;

procedure TJSONViewer.SetShowGutter(const Value: Boolean);
begin
  if FShowGutter <> Value then
  begin
    FShowGutter := Value;
    Repaint;
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
    
    // Update the UI
    Resize;
    Repaint;
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
    Repaint;
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
  i, j: Integer;
  lineIndexInList: Integer;
  actualLineIndex: Integer;
  line: TJSONLine;
  token: PJSONToken;
  textRect, gutterRect, ellipsisRect: TRectF;
  isExpandable: Boolean;
  matchingLineIndex: Integer;
  matchingLine: TJSONLine;
  closeBraceStr: string;
  x, y: Single;
  originalX: Single;
  propertyNameDisplayed: Boolean;
  textLayout: TTextLayout;
  tokenWidth: Single;
  clipRect: TRectF;
  gutterX: Single;
begin
  // Clear the background
  Canvas.Fill.Color := TAlphaColorRec.White;
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.FillRect(LocalRect, 0, 0, [], 1);
  
  // Check if we have content to draw
  if (FLines = nil) or (FLines.Count = 0) or (FVisibleLineIndices.Count = 0) then
    Exit;
  
  // Draw gutter if enabled
  if FShowGutter then
  begin
    // Only draw the gutter border, not filling with a different color
    Canvas.Stroke.Color := FGutterBorderColor;
    Canvas.Stroke.Kind := TBrushKind.Solid;
    Canvas.DrawLine(
      PointF(FGutterWidth, 0),
      PointF(FGutterWidth, Height - FHScrollBar.Height),
      1
    );
  end;
  
  // Create text layout
  textLayout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    textLayout.Font.Family := FFontFamily;
    textLayout.Font.Size := FFontSize;
    
    // Apply clipping for horizontal scrolling
    if FShowGutter then
      gutterX := FGutterWidth
    else
      gutterX := 0;
      
    clipRect := RectF(
      gutterX,
      0, 
      Width - FVScrollBar.Width, 
      Height - FHScrollBar.Height
    );
    
    // Apply horizontal scrolling offset by modifying X coordinates
    
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
            DrawPlusMinusIcon(Canvas, (FGutterWidth - 16) / 2, y + (FLineHeight - 16) / 2, line.IsCollapsed);
          end;
        end;
        
        // Draw the text
        x := FGutterWidth + 4 - FHScrollPos;  // Starting position with padding and scroll offset
        originalX := x;
        
        if (not FColorizeJSON) or (line.Tokens.Count = 0) then
        begin
          // Draw the whole line in one color
          textLayout.BeginUpdate;
          textLayout.Text := line.Text;
          textLayout.Font.Style := [];
          textLayout.Color := TAlphaColorRec.Black;
          textLayout.EndUpdate;
          
          textLayout.TopLeft := PointF(x, y);
          textLayout.RenderLayout(Canvas);
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
                
                // Set the color and style for this token
                textLayout.BeginUpdate;
                textLayout.Text := token^.Value;
                
                case token^.TokenType of
                  jttBrace:   textLayout.Color := FBraceColor;
                  jttKey:     begin
                                textLayout.Color := FKeyColor;
                                textLayout.Font.Style := [TFontStyle.fsBold];
                              end;
                  jttString:  textLayout.Color := FStringColor;
                  jttNumber:  textLayout.Color := FNumberColor;
                  jttKeyword: begin
                                textLayout.Color := FKeywordColor;
                                textLayout.Font.Style := [TFontStyle.fsBold];
                              end;
                  jttWhitespace: textLayout.Color := TAlphaColorRec.Black;
                end;
                
                textLayout.EndUpdate;
                
                // Draw the token
                textLayout.TopLeft := PointF(x, y);
                textLayout.RenderLayout(Canvas);
                
                // Move to next token position
                x := x + textLayout.Width;
              end;
              
              // Now draw the opening brace
              textLayout.BeginUpdate;
              textLayout.Color := FBraceColor;
              textLayout.Font.Style := [];
              
              // Draw the opening brace ('{' or '[')
              if line.BraceType = brOpenObject then
                textLayout.Text := '{'
              else
                textLayout.Text := '[';
                
              textLayout.EndUpdate;
              
              textLayout.TopLeft := PointF(x, y);
              textLayout.RenderLayout(Canvas);
              
              x := x + textLayout.Width;
              
              // Draw ellipsis in a rectangle
              Canvas.Stroke.Color := TAlphaColorRec.Gray;
              Canvas.Stroke.Kind := TBrushKind.Solid;
              Canvas.Fill.Kind := TBrushKind.None;
              ellipsisRect := RectF(x + 4, y + 2, x + 28, y + FLineHeight - 2);
              Canvas.DrawRect(ellipsisRect, 0, 0, [], 1);
              
              // Draw ellipsis
              textLayout.BeginUpdate;
              textLayout.Text := '...';
              textLayout.Color := TAlphaColorRec.Black;
              textLayout.EndUpdate;
              
              textLayout.TopLeft := PointF(x + 10, y);
              textLayout.RenderLayout(Canvas);
              
              x := ellipsisRect.Right + 4;
              
              // Draw the closing brace
              closeBraceStr := '}';
              if line.BraceType = brOpenArray then
                closeBraceStr := ']';
                
              textLayout.BeginUpdate;
              textLayout.Text := closeBraceStr;
              textLayout.Color := FBraceColor;
              textLayout.EndUpdate;
              
              textLayout.TopLeft := PointF(x, y);
              textLayout.RenderLayout(Canvas);
            end;
          end
          else
          begin
            // Normal drawing of all tokens
            for j := 0 to line.Tokens.Count - 1 do
            begin
              token := PJSONToken(line.Tokens[j]);
              
              // Set the color for this token
              textLayout.BeginUpdate;
              textLayout.Text := token^.Value;
              
              case token^.TokenType of
                jttBrace:   textLayout.Color := FBraceColor;
                jttKey:     begin
                              textLayout.Color := FKeyColor;
                              textLayout.Font.Style := [TFontStyle.fsBold];
                            end;
                jttString:  textLayout.Color := FStringColor;
                jttNumber:  textLayout.Color := FNumberColor;
                jttKeyword: begin
                              textLayout.Color := FKeywordColor;
                              textLayout.Font.Style := [TFontStyle.fsBold];
                            end;
                jttWhitespace: textLayout.Color := TAlphaColorRec.Black;
              end;
              
              textLayout.EndUpdate;
              
              // Draw the token
              textLayout.TopLeft := PointF(x, y);
              textLayout.RenderLayout(Canvas);
              
              // Move to next token position
              x := x + textLayout.Width;
            end;
          end;
        end;
      end;
      
      // Move to next line
      y := y + FLineHeight;
    end;
    
  finally
    textLayout.Free;
  end;
end;

procedure TJSONViewer.Resize;
begin
  inherited;
  CalculateLineHeight;
  
  // Adjust scrollbar positions
  if Assigned(FVScrollBar) then
  begin
    FVScrollBar.SetBounds(Width - FVScrollBar.Width, 0, FVScrollBar.Width, Height - FHScrollBar.Height);
  end;
  
  if Assigned(FHScrollBar) then
  begin
    FHScrollBar.SetBounds(0, Height - FHScrollBar.Height, Width - FVScrollBar.Width, FHScrollBar.Height);
  end;
  
  // Calculate visible lines
  FVisibleLines := Trunc((Height - FHScrollBar.Height) / FLineHeight) + 1;
  
  // If we created new content, recalculate max width
  if FMaxLineWidth = 0 then
    CalculateMaxLineWidth;
  
  // Update scrollbar 
  UpdateScrollBars;
  
  Repaint;
end;

procedure TJSONViewer.UpdateScrollBars;
var
  maxLines: Integer;
  maxWidth: Single;
begin
  if (FVisibleLineIndices <> nil) and (FVisibleLineIndices.Count > 0) and Assigned(FVScrollBar) and Assigned(FHScrollBar) then
  begin
    // Vertical scrollbar
    maxLines := FVisibleLineIndices.Count;
    
    FVScrollBar.Min := 0;
    FVScrollBar.Max := maxLines - 1;
    FVScrollBar.ViewportSize := FVisibleLines;
    FVScrollBar.Value := FFirstLine;
    FVScrollBar.SmallChange := 1;
    // Use normal page size for scrolling
    if FVisibleLines > 0 then
      FVScrollBar.SmallChange := 1;
    FVScrollBar.Visible := maxLines > FVisibleLines;
    
    // Horizontal scrollbar
    maxWidth := FMaxLineWidth;
    
    // Only show horizontal scrollbar if content wider than client area
    if maxWidth > (Width - FVScrollBar.Width) then
    begin
      FHScrollBar.Min := 0;
      FHScrollBar.Max := maxWidth;
      FHScrollBar.ViewportSize := Width - FVScrollBar.Width;
      FHScrollBar.Value := FHScrollPos;
      FHScrollBar.SmallChange := 10;
      // Use reasonable value for scrolling
      FHScrollBar.SmallChange := 10;
      FHScrollBar.Visible := True;
    end
    else
    begin
      // Hide horizontal scrollbar if content fits
      FHScrollPos := 0;
      FHScrollBar.Value := 0;
      FHScrollBar.Visible := False;
    end;
  end;
end;

procedure TJSONViewer.HScrollChangeHandler(Sender: TObject);
begin
  FHScrollPos := FHScrollBar.Value;
  Repaint;
end;

procedure TJSONViewer.VScrollChangeHandler(Sender: TObject);
begin
  if FFirstLine <> Round(FVScrollBar.Value) then
  begin
    FFirstLine := Round(FVScrollBar.Value);
    Repaint;
  end;
end;

procedure TJSONViewer.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
var
  LinesToScroll: Integer;
begin
  inherited;
  
  // Determine how many lines to scroll
  LinesToScroll := Abs(WheelDelta) div 120;
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
  if Assigned(FVScrollBar) then
    FVScrollBar.Value := FFirstLine;
  
  // Repaint
  Repaint;
  
  Handled := True;
end;

procedure TJSONViewer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  lineIndex: Integer;
  realLineIndex: Integer;
  line: TJSONLine;
  tokenX, ellipsisX, braceX: Single;
  j: Integer;
  token: PJSONToken;
  ellipsisWidth, braceWidth: Single;
  isEllipsisClick, isBraceClick: Boolean;
  currentX: Single;
  textLayout: TTextLayout;
begin
  inherited;
  
  FMouseDownPos := PointF(X, Y);
  isEllipsisClick := False;
  isBraceClick := False;
  
  // Skip if clicking on scrollbars
  if (X >= Width - FVScrollBar.Width) or (Y >= Height - FHScrollBar.Height) then
    Exit;
  
  // Determine which line was clicked
  lineIndex := Trunc(Y / FLineHeight);
  
  // Convert to real line index
  if (lineIndex >= 0) and (lineIndex < FVisibleLineIndices.Count - FFirstLine) then
  begin
    realLineIndex := FVisibleLineIndices[lineIndex + FFirstLine];
    
    if (Button = TMouseButton.mbLeft) then
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
        
        textLayout := TTextLayoutManager.DefaultTextLayout.Create;
        try
          textLayout.Font.Family := FFontFamily;
          textLayout.Font.Size := FFontSize;
          
          // Add horizontal scroll offset to X to get actual position
          X := X + FHScrollPos;
          
          // Check if this is a collapsed object with ellipsis
          if line.IsCollapsed and ((line.BraceType = brOpenObject) or (line.BraceType = brOpenArray)) then
          begin
            // Calculate where the ellipsis and brace would be
            tokenX := FGutterWidth + 4; // Start with padding
            braceX := 0;
            
            // Add up all token widths until we reach the opening brace
            for j := 0 to line.Tokens.Count - 1 do
            begin
              token := PJSONToken(line.Tokens[j]);
              
              if (token.TokenType = jttBrace) and 
                ((token.Value = '{') or (token.Value = '[')) then
              begin
                braceX := tokenX;
                break;
              end;
              
              textLayout.BeginUpdate;
              textLayout.Text := token^.Value;
              textLayout.EndUpdate;
              
              tokenX := tokenX + textLayout.Width;
            end;
            
            // Calculate brace width
            textLayout.BeginUpdate;
            textLayout.Text := '{';
            textLayout.EndUpdate;
            braceWidth := textLayout.Width;
            
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
            
            for j := 0 to line.Tokens.Count - 1 do
            begin
              token := PJSONToken(line.Tokens[j]);
              
              // Calculate token width for click detection
              textLayout.BeginUpdate;
              textLayout.Text := token^.Value;
              textLayout.EndUpdate;
              
              tokenX := currentX;
              currentX := currentX + textLayout.Width;
              
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
        finally
          textLayout.Free;
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
    Repaint;
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

procedure TJSONViewer.DrawPlusMinusIcon(Canvas: TCanvas; X, Y: Single; IsCollapsed: Boolean);
var
  IconRect: TRectF;
begin
  // Define the icon rectangle
  IconRect := RectF(X, Y, X + 16, Y + 16);
  
  // Draw background and border
  Canvas.Fill.Color := TAlphaColorRec.White;
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.FillRect(IconRect, 0, 0, [], 1);
  
  Canvas.Stroke.Color := FGutterBorderColor;
  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.DrawRect(IconRect, 0, 0, [], 1);
  
  // Draw the horizontal line (minus sign)
  Canvas.Stroke.Color := TAlphaColorRec.Black;
  Canvas.DrawLine(
    PointF(X + 4, Y + 8),
    PointF(X + 12, Y + 8),
    1
  );
  
  // If collapsed (plus sign), add the vertical line
  if IsCollapsed then
  begin
    Canvas.DrawLine(
      PointF(X + 8, Y + 4),
      PointF(X + 8, Y + 12),
      1
    );
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
  i, j: Integer;
  line: TJSONLine;
  token: PJSONToken;
  lineWidth: Single;
  textLayout: TTextLayout;
begin
  FMaxLineWidth := 0;
  
  textLayout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    textLayout.Font.Family := FFontFamily;
    textLayout.Font.Size := FFontSize;
    
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
            textLayout.BeginUpdate;
            textLayout.Text := token.Value;
            textLayout.EndUpdate;
            lineWidth := lineWidth + textLayout.Width;
            
            // Add ellipsis width + padding
            lineWidth := lineWidth + 4 + 24 + 4;
            
            // Add closing brace width
            lineWidth := lineWidth + textLayout.Width;
            
            Break;
          end;
          
          textLayout.BeginUpdate;
          textLayout.Text := token.Value;
          textLayout.EndUpdate;
          lineWidth := lineWidth + textLayout.Width;
        end;
      end
      else
      begin
        // For normal lines, add up all token widths
        for j := 0 to line.Tokens.Count - 1 do
        begin
          token := PJSONToken(line.Tokens[j]);
          
          textLayout.BeginUpdate;
          textLayout.Text := token.Value;
          textLayout.EndUpdate;
          
          lineWidth := lineWidth + textLayout.Width;
        end;
      end;
      
      // Update max width if this line is wider
      if lineWidth > FMaxLineWidth then
        FMaxLineWidth := lineWidth;
    end;
    
    // Add some padding
    FMaxLineWidth := FMaxLineWidth + 10;
  finally
    textLayout.Free;
  end;
end;

end.
