// *****************************************************************************
//
// chimera.json.parser;
//
// JSON Chimera project for Delphi
//
// Copyright (c) 2012 by Sivv LLC, All Rights Reserved
//
// Information about this product can be found at
// http://arcana.sivv.com/chimera
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
// *****************************************************************************

unit chimera.json.parser;

interface

{$I chimera.inc}

uses
  {$IFDEF FPC}
  SysUtils,
  Classes,
  Generics.Collections,
  Rtti,
  Types,
  {$ELSE}
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  System.Types,
  {$ENDIF}
  chimera.json
  {$IFDEF USEFASTCODE}, chimera.FastStringBuilder{$ENDIF};

type

{$SCOPEDENUMS ON}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

  EChimeraParseException = class(EChimeraException);

  TParser = class(TObject)
  type
    TParseToken = (&String, Colon, OpenObject, CloseObject, OpenArray, CloseArray, Comma, EOF, MaxOp, Value);
  private
    FFmt : TFormatSettings;
    //FText : string;
    FPText : PChar;
    FTextLength : integer;
    FIndex : integer;
    FToken : TParseToken;
    FTokenValue : TMultiValue;
    FOperatorStack : TStack<TParseToken>;
    FValueStack : TStack<TMultiValue>;
    FTmpValue : {$IFDEF USEFASTCODE}chimera.FastStringBuilder.{$ENDIF}TStringBuilder;
    FTmpIdent : {$IFDEF USEFASTCODE}chimera.FastStringBuilder.{$ENDIF}TStringBuilder;
    FDepth : Cardinal;
    function GetToken : boolean;
    function ParseArray: IJSONArray; overload;
    function ParseObject: IJSONObject;
    procedure ParseObjectTo(const Obj : IJSONObject);
  protected
    function OperatorToStr(Token : TParseToken) : string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ExecuteTo(const AText : PChar; const Obj : IJSONObject);
    function Execute(const AText : PChar) : IJSONObject;
    function ExecuteForArray(const AText : PChar) : IJSONArray;
    class function Parse(const AText : PChar) : IJSONObject;
    class procedure ParseTo(const AText : PChar; const Obj : IJSONObject);
    class function ParseArray(const AText : PChar) : IJSONArray; overload;
  end;

function CreateENUSFormatSettings : TFormatSettings; inline;

implementation

uses
  {$IFDEF FPC}
  Character,
  Variants,
  {$ELSE}
  System.Character,
  System.Variants,
  {$ENDIF}
  chimera.json.parser.builder;

function CreateENUSFormatSettings : TFormatSettings;
begin
  {$IFDEF FPC}
  Result.ThousandSeparator := ',';
  Result.DecimalSeparator := '.';
  {$ELSE}
  Result := TFormatSettings.Create('en-us');
  {$ENDIF}
end;


{ TParser }

constructor TParser.Create;
begin
  inherited Create;
  FDepth := 0;
  FOperatorStack := TStack<TParseToken>.Create;
  FValueStack := TStack<TMultiValue>.Create;

  FFmt := CreateENUSFormatSettings;

  FTmpValue := {$IFDEF USEFASTCODE}chimera.FastStringBuilder.{$ENDIF}TStringBuilder.Create;
  FTmpIdent := {$IFDEF USEFASTCODE}chimera.FastStringBuilder.{$ENDIF}TStringBuilder.Create;
end;

destructor TParser.Destroy;
begin
  FOperatorStack.Free;
  FValueStack.Free;
  FTmpValue.Free;
  FTmpIdent.Free;
  inherited;
end;

function TParser.OperatorToStr(Token : TParseToken) : string;
begin
  case Token of
    TParser.TParseToken.&String:
      Result := 'String';
    TParser.TParseToken.Colon:
      Result := 'Colon';
    TParser.TParseToken.OpenObject:
      Result := 'OpenObject';
    TParser.TParseToken.CloseObject:
      Result := 'CloseObject';
    TParser.TParseToken.OpenArray:
      Result := 'OpenArray';
    TParser.TParseToken.CloseArray:
      Result := 'CloseArray';
    TParser.TParseToken.Comma:
      Result := 'Comma';
    TParser.TParseToken.EOF:
      Result := 'EOF';
    TParser.TParseToken.MaxOp:
      Result := 'MaxOp';
    TParser.TParseToken.Value:
      Result := 'Value';
  end;
end;

function TParser.GetToken: boolean;
  function IsValueChar(c : {$IFDEF FPC}{$IFDEF UNICODE}Char{$ELSE}UnicodeChar{$ENDIF}{$ELSE}Char{$ENDIF}; fmt : TFormatSettings) : boolean; inline;
  begin
    Result := (c.IsLetterOrDigit) or (c = '-') or (c = '+') or (c = fmt.DecimalSeparator);
  end;
var
  c : Char;
  d : Double;
  b : boolean;
  iStart : integer;
  iCnt : integer;
  iBack : integer;
  sVal : string;
begin
  Result := False;
  inc(FIndex);
  while FIndex < FTextLength do
  begin
    c := FPText[FIndex];
    case c of
      '{': begin FToken := TParseToken.OpenObject; Exit; end;
      '}': begin FToken := TParseToken.CloseObject; Exit; end;
      '[': begin FToken := TParseToken.OpenArray; Exit; end;
      ']': begin FToken := TParseToken.CloseArray; Exit; end;
      ',': begin FToken := TParseToken.Comma; Exit; end;
      ':': begin FToken := TParseToken.Colon; Exit; end;
      '"':
      begin
        FToken := TParseToken.&String;
        iStart := FIndex + 1;
        FIndex := iStart;
        while FIndex < FTextLength do
        begin
          c := FPText[FIndex];
          if c = '"' then
          begin
            if (FIndex > iStart) and (FPText[FIndex - 1] = '\') then
            begin
              iCnt := 0;
              iBack := FIndex - 1;
              while (iBack >= iStart) and (FPText[iBack] = '\') do
              begin
                inc(iCnt);
                dec(iBack);
              end;
              if Odd(iCnt) then
              begin
                inc(FIndex);
                continue;
              end;
            end;
            break;
          end;
          inc(FIndex);
        end;
        SetString(sVal, FPText + iStart, FIndex - iStart);
        FTokenValue.Initialize(sVal);
        Exit;
      end;
      #1..#32:
      begin
        inc(FIndex);
        continue;
      end;
    else
      if IsValueChar(c, FFmt) then
      begin
        iStart := FIndex;
        inc(FIndex);
        while IsValueChar(FPText[FIndex], FFmt) do
          inc(FIndex);
        dec(FIndex);
        FToken := TParseToken.Value;
        SetString(sVal, FPText + iStart, FIndex - iStart + 1);
        if TryStrToFloat(sVal, d, FFmt) then
          FTokenValue.Initialize(d)
        else if TryStrToBool(sVal, b) then
          FTokenValue.Initialize(b)
        else if sVal = 'null' then
          FTokenValue.ClearToNull
        else
          raise EChimeraParseException.Create('Unexpected Value "' + sVal + '" at Index ' + FIndex.toString);
        Exit;
      end;
      inc(FIndex);
      continue;
    end;
  end;
  FToken := TParseToken.EOF;
end;

function TParser.ParseArray : IJSONArray;
begin
  if FToken <> TParseToken.OpenArray  then
    raise EChimeraParseException.Create('Array Expected');

  inc(FDepth);
  if FDepth >= TJSON.MaximumDepth then
    raise EChimeraParseException.Create('Maximum JSON Depth Exceeded');
  try
    Result := TJSONArray.New;
    TJSONParserBuilder.ArrayBegin(Result);
    try
      GetToken;
      while FToken <> TParseToken.CloseArray do
      begin
        case FToken of
          TParser.TParseToken.&String:
            TJSONParserBuilder.ArrayValue(Result, FTokenValue);
          TParser.TParseToken.OpenObject:
            TJSONParserBuilder.ArrayObject(Result, ParseObject);
          TParser.TParseToken.OpenArray:
            TJSONParserBuilder.ArrayArray(Result, ParseArray);
          TParser.TParseToken.Value:
            case FTokenValue.ValueType of
              TJSONValueType.&string,
              TJSONValueType.number,
              TJSONValueType.boolean:
                TJSONParserBuilder.ArrayValue(Result, FTokenValue);
              TJSONValueType.&array:
                TJSONParserBuilder.ArrayArray(Result, FTokenValue.ArrayValue);
              TJSONValueType.&object:
                TJSONParserBuilder.ArrayObject(Result, FTokenValue.ObjectValue);
              TJSONValueType.null:
                TJSONParserBuilder.ArrayNull(Result);
            end;
          TParser.TParseToken.CloseObject,
          TParser.TParseToken.CloseArray,
          TParser.TParseToken.Comma,
          TParser.TParseToken.EOF,
          TParser.TParseToken.MaxOp,
          TParser.TParseToken.Colon:
            if FToken <> TParseToken.Colon then
              raise EChimeraParseException.Create('Value Expected');
        end;
        GetToken;
        if not (FToken in [TParseToken.Comma, TParseToken.CloseArray]) then
        begin
          raise EChimeraParseException.Create('Comma or Close Array Expected');
        end;
        if FToken = TParseToken.Comma then
          GetToken;
      end;
    finally
      TJSONParserBuilder.ArrayEnd(Result);
    end;
  finally
    dec(FDepth);
  end;
end;

function TParser.ParseObject : IJSONObject;
begin
  Result := TJSON.New;
  ParseObjectTo(Result);
end;

procedure TParser.ParseObjectTo(const Obj: IJSONObject);
var
  sName : String;
begin
  if FToken <> TParseToken.OpenObject  then
    raise EChimeraParseException.Create('Object Expected');

  inc(FDepth);
  if FDepth >= TJSON.MaximumDepth then
    raise EChimeraParseException.Create('Maximum JSON Depth Exceeded');
  try
    TJSONParserBuilder.ObjectBegin(Obj);
    try
      GetToken;
      while FToken <> TParseToken.CloseObject do
      begin
        if FToken <> TParseToken.&String then
          raise EChimeraParseException.Create('String Expected');
        sName := FTokenValue.StringValue;
        GetToken;
        if FToken <> TParseToken.Colon then
          raise EChimeraParseException.Create('Colon Expected');
        GetToken;
        case FToken of
          TParser.TParseToken.&String:
            TJSONParserBuilder.ObjectValue(Obj, sName, FTokenValue);
          TParser.TParseToken.OpenObject:
            TJSONParserBuilder.ObjectObject(Obj, sName, ParseObject);
          TParser.TParseToken.OpenArray:
            TJSONParserBuilder.ObjectArray(Obj, sName, ParseArray);
          TParser.TParseToken.Value:
            case FTokenValue.ValueType of
              TJSONValueType.&string,
              TJSONValueType.number,
              TJSONValueType.boolean:
                TJSONParserBuilder.ObjectValue(Obj, sName, FTokenValue);
              TJSONValueType.&array:
                TJSONParserBuilder.ObjectArray(Obj, sName, FTokenValue.ArrayValue);
              TJSONValueType.&object:
                TJSONParserBuilder.ObjectObject(Obj, sName, FTokenValue.ObjectValue);
              TJSONValueType.null:
                TJSONParserBuilder.ObjectNull(Obj, sName);
            end;
          TParser.TParseToken.CloseObject,
          TParser.TParseToken.CloseArray,
          TParser.TParseToken.Comma,
          TParser.TParseToken.EOF,
          TParser.TParseToken.MaxOp,
          TParser.TParseToken.Colon:
            if FToken <> TParseToken.Colon then
              raise EChimeraParseException.Create('Value Expected');
        end;
        GetToken;
        if not (FToken in [TParseToken.Comma, TParseToken.CloseObject]) then
        begin
          raise EChimeraParseException.Create('Comma or Close Object Expected');
        end;
        if FToken = TParseToken.Comma then
          GetToken;
      end;
    finally
      TJSONParserBuilder.ObjectEnd(Obj);
    end;
  finally
    dec(FDepth);
  end;
end;

class procedure TParser.ParseTo(const AText: PChar;
  const Obj: IJSONObject);
var
  p : TParser;
begin
  p := TParser.Create;
  try
    p.ExecuteTo(AText, Obj);
  finally
    p.Free;
  end;
end;

function TParser.Execute(const AText: PChar): IJSONObject;
  function SimpleJSONValue : IJSONObject;
  begin
    Result := TJSON.New;
    case FToken of
      TParser.TParseToken.&String:
        Result.AsString := FTokenValue.StringValue;
      TParser.TParseToken.Value:
        case FTokenValue.ValueType of
          TJSONValueType.&string:
            Result.AsString := FTokenValue.StringValue;
          TJSONValueType.number:
            Result.AsNumber := FTokenValue.NumberValue;
          TJSONValueType.&array:
            Result.AsArray := FTokenValue.ArrayValue;
          TJSONValueType.&object:
            Result := FTokenValue.ObjectValue;
          TJSONValueType.boolean:
            Result.AsBoolean := FTokenValue.IntegerValue <> 0;
          TJSONValueType.null:
            Result.IsNull := True
        end;

      TParser.TParseToken.OpenArray:
        Result.AsArray := ParseArray;
    end;
  end;
begin
  if Trim(AText) = '' then
  begin
    Result := TJSON.New;
    exit;
  end;
  FIndex := -1;
  //FText := AText;
  FTextLength := StrLen(AText);
  FPText := AText;//PChar(AText);
  if GetToken then
    exit;
  case FToken of
    TParser.TParseToken.OpenObject:
      Result := ParseObject;
    TParser.TParseToken.&String,
    TParser.TParseToken.Value,
    TParser.TParseToken.OpenArray:
      Result := SimpleJSONValue;
    TParser.TParseToken.Colon,
    TParser.TParseToken.CloseObject,
    TParser.TParseToken.CloseArray,
    TParser.TParseToken.Comma,
    TParser.TParseToken.EOF,
    TParser.TParseToken.MaxOp:
      raise EParserError.Create('Invalid JSON string');
  end;
end;

function TParser.ExecuteForArray(const AText : PChar) : IJSONArray;
begin
  if Trim(AText) = '' then
  begin
    Result := TJSONArray.New;
    exit;
  end;
  FIndex := -1;
  //FText := AText;
  FTextLength := StrLen(AText);
  FPText := AText;//PChar(AText);
  if GetToken then
    exit;
  Result := ParseArray;
end;

procedure TParser.ExecuteTo(const AText: PChar; const Obj: IJSONObject);
begin
  if Trim(AText) = '' then
  begin
    Obj.Clear;
    exit;
  end;
  FIndex := -1;
  //FText := AText;
  FTextLength := StrLen(AText);
  FPText := AText;//PChar(AText);
  if GetToken then
    exit;
  ParseObjectTo(Obj);
end;

class function TParser.Parse(const AText: PChar): IJSONObject;
var
  p : TParser;
begin
  p := TParser.Create;
  try
    Result := p.Execute(AText);
  finally
    p.Free;
  end;
end;

class function TParser.ParseArray(const AText : PChar) : IJSONArray;
var
  p : TParser;
begin
  p := TParser.Create;
  try
    Result := p.ExecuteForArray(AText);
  finally
    p.Free;
  end;
end;

end.

