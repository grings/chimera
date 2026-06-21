unit chimera.json.jcs;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Generics.Defaults,
  chimera.json,
  chimera.json.jcs.number;

type
  EChimeraJCSException = class(EChimeraJSONException);

  TJCSKeyCompare = class
  public
    class function Compare(const Left, Right: string): Integer; static;
    class procedure Sort(var Keys: TArray<string>); static;
  end;

  TJCSStringEncoder = class
  public
    class procedure ValidateNoLoneSurrogates(const Value: string); static;
    class function Encode(const Value: string): string; static;
  end;

  TJCSSerializer = class
  public
    class function SerializeObject(const Obj: IJSONObject): string; static;
    class function SerializeArray(const Arr: IJSONArray): string; static;
    class function SerializeValue(const Value: PMultiValue): string; static;
    class function SerializeObjectBytes(const Obj: IJSONObject): TBytes; static;
    class function SerializeArrayBytes(const Arr: IJSONArray): TBytes; static;
  end;

function JCS(const JSONText: string): string;
function JCSBytes(const JSONText: string): TBytes;

implementation

const
  HexLower: array[0..15] of Char = (
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
  );

class function TJCSKeyCompare.Compare(const Left, Right: string): Integer;
var
  I, MinLen: Integer;
  LeftUnit, RightUnit: Word;
begin
  MinLen := Left.Length;
  if Right.Length < MinLen then
    MinLen := Right.Length;

  for I := 0 to MinLen - 1 do
  begin
    LeftUnit := Ord(Left.Chars[I]);
    RightUnit := Ord(Right.Chars[I]);
    if LeftUnit <> RightUnit then
      Exit(Integer(LeftUnit) - Integer(RightUnit));
  end;

  Result := Left.Length - Right.Length;
end;

class procedure TJCSKeyCompare.Sort(var Keys: TArray<string>);
begin
  TArray.Sort<string>(Keys, TComparer<string>.Construct(Compare));
end;

class procedure TJCSStringEncoder.ValidateNoLoneSurrogates(const Value: string);
var
  I: Integer;
  CodeUnit, NextUnit: Word;
begin
  I := 0;
  while I < Value.Length do
  begin
    CodeUnit := Ord(Value.Chars[I]);
    if (CodeUnit >= $D800) and (CodeUnit <= $DBFF) then
    begin
      if I + 1 >= Value.Length then
        raise EChimeraJCSException.Create('Lone surrogate in string data');
      NextUnit := Ord(Value.Chars[I + 1]);
      if (NextUnit < $DC00) or (NextUnit > $DFFF) then
        raise EChimeraJCSException.Create('Lone surrogate in string data');
      Inc(I, 2);
      Continue;
    end;

    if (CodeUnit >= $DC00) and (CodeUnit <= $DFFF) then
      raise EChimeraJCSException.Create('Lone surrogate in string data');

    Inc(I);
  end;
end;

class function TJCSStringEncoder.Encode(const Value: string): string;
var
  SB: TStringBuilder;
  I: Integer;
  C: Char;
  CodeUnit: Word;
begin
  ValidateNoLoneSurrogates(Value);

  SB := TStringBuilder.Create;
  try
    SB.Append('"');
    for I := 0 to Value.Length - 1 do
    begin
      C := Value.Chars[I];
      case C of
        '"':
          SB.Append('\"');
        '\':
          SB.Append('\\');
        #8:
          SB.Append('\b');
        #9:
          SB.Append('\t');
        #10:
          SB.Append('\n');
        #12:
          SB.Append('\f');
        #13:
          SB.Append('\r');
      else
        begin
          CodeUnit := Ord(C);
          if CodeUnit < 32 then
          begin
            SB.Append('\u');
            SB.Append(HexLower[(CodeUnit shr 12) and $0F]);
            SB.Append(HexLower[(CodeUnit shr 8) and $0F]);
            SB.Append(HexLower[(CodeUnit shr 4) and $0F]);
            SB.Append(HexLower[CodeUnit and $0F]);
          end
          else
            SB.Append(C);
        end;
      end;
    end;
    SB.Append('"');
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

class function TJCSSerializer.SerializeValue(const Value: PMultiValue): string;
var
  LogicalString: string;
begin
  case Value.ValueType of
    TJSONValueType.null:
      Result := 'null';
    TJSONValueType.boolean:
      if Value.IntegerValue <> 0 then
        Result := 'true'
      else
        Result := 'false';
    TJSONValueType.number:
      Result := TJCSNumberEncoder.Format(Value.NumberValue);
    TJSONValueType.&string:
      begin
        LogicalString := TJSON.Decode(Value.StringValue);
        Result := TJCSStringEncoder.Encode(LogicalString);
      end;
    TJSONValueType.&array:
      begin
        if Assigned(Value.ArrayValue) then
          Result := SerializeArray(Value.ArrayValue)
        else
          Result := 'null';
      end;
    TJSONValueType.&object:
      begin
        if Assigned(Value.ObjectValue) then
          Result := SerializeObject(Value.ObjectValue)
        else
          Result := 'null';
      end;
    TJSONValueType.code:
      raise EChimeraJCSException.Create('Code values cannot be canonicalized as JSON');
  else
    raise EChimeraJCSException.Create('Unsupported JSON value type');
  end;
end;

class function TJCSSerializer.SerializeArray(const Arr: IJSONArray): string;
var
  SB: TStringBuilder;
  I: Integer;
begin
  SB := TStringBuilder.Create;
  try
    SB.Append('[');
    for I := 0 to Arr.Count - 1 do
    begin
      if I > 0 then
        SB.Append(',');
      SB.Append(SerializeValue(Arr.Values[I]));
    end;
    SB.Append(']');
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

class function TJCSSerializer.SerializeObject(const Obj: IJSONObject): string;
var
  SB: TStringBuilder;
  Keys: TArray<string>;
  I: Integer;
  Name: string;
  SimpleValue: TMultiValue;
begin
  if Obj.IsSimpleValue then
  begin
    SimpleValue := Obj.AsValue;
    Exit(SerializeValue(@SimpleValue));
  end;

  SetLength(Keys, Obj.Count);
  for I := 0 to Obj.Count - 1 do
    Keys[I] := Obj.Names[I];
  TJCSKeyCompare.Sort(Keys);

  SB := TStringBuilder.Create;
  try
    SB.Append('{');
    for I := 0 to High(Keys) do
    begin
      if I > 0 then
        SB.Append(',');
      Name := Keys[I];
      SB.Append(TJCSStringEncoder.Encode(Name));
      SB.Append(':');
      SB.Append(SerializeValue(Obj.Values[Name]));
    end;
    SB.Append('}');
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

class function TJCSSerializer.SerializeObjectBytes(const Obj: IJSONObject): TBytes;
begin
  Result := TEncoding.UTF8.GetBytes(SerializeObject(Obj));
end;

class function TJCSSerializer.SerializeArrayBytes(const Arr: IJSONArray): TBytes;
begin
  Result := TEncoding.UTF8.GetBytes(SerializeArray(Arr));
end;

function JCS(const JSONText: string): string;
var
  Parsed: IJSONObject;
begin
  Parsed := TJSON.From(JSONText);
  Result := TJCSSerializer.SerializeObject(Parsed);
end;

function JCSBytes(const JSONText: string): TBytes;
var
  Parsed: IJSONObject;
begin
  Parsed := TJSON.From(JSONText);
  Result := TJCSSerializer.SerializeObjectBytes(Parsed);
end;

end.
