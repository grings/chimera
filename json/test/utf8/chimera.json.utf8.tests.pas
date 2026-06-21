unit chimera.json.utf8.tests;

interface

function RunUtf8SaveTests: Integer;

implementation

uses
  System.Classes,
  System.SysUtils,
  chimera.json;

function BytesEqual(const Left, Right: TBytes): Boolean;
var
  I: Integer;
begin
  Result := Length(Left) = Length(Right);
  if not Result then
    Exit;
  for I := 0 to High(Left) do
    if Left[I] <> Right[I] then
      Exit(False);
end;

function SaveCompactUtf8Bytes(const Obj: IJSONObject): TBytes;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Obj.SaveToStream(Stream, TWhitespace.compact);
    SetLength(Result, Stream.Size);
    if Stream.Size > 0 then
    begin
      Stream.Position := 0;
      Stream.ReadBuffer(Result[0], Stream.Size);
    end;
  finally
    Stream.Free;
  end;
end;

function ReferenceCompactUtf8Bytes(const Obj: IJSONObject): TBytes;
begin
  Result := TEncoding.UTF8.GetBytes(Obj.AsJSON(TWhitespace.compact));
end;

procedure ExpectBytes(const Name: string; const Actual, Expected: TBytes; var Failures: Integer);
begin
  if BytesEqual(Actual, Expected) then
    Writeln('  PASS: ', Name)
  else
  begin
    Writeln('  FAIL: ', Name, ' (actual=', Length(Actual), ' expected=', Length(Expected), ')');
    Inc(Failures);
  end;
end;

procedure ExpectRoundTrip(const Name, Value: string; var Failures: Integer);
var
  Source, Loaded: IJSONObject;
  Actual, Expected: TBytes;
begin
  Source := TJSON.New;
  Source.Strings['text'] := Value;
  Actual := SaveCompactUtf8Bytes(Source);
  Expected := ReferenceCompactUtf8Bytes(Source);
  ExpectBytes(Name + '.bytes', Actual, Expected, Failures);

  Loaded := TJSON.New;
  Loaded.Reload(TEncoding.UTF8.GetString(Actual));

  if Loaded.Strings['text'] = Value then
    Writeln('  PASS: ', Name, '.roundtrip')
  else
  begin
    Writeln('  FAIL: ', Name, '.roundtrip');
    Inc(Failures);
  end;
end;

procedure TestAsciiAndEscapes(var Failures: Integer);
var
  Obj: IJSONObject;
  Actual, Expected: TBytes;
begin
  Obj := TJSON.New;
  Obj.Strings['ascii'] := 'key"value\ntab';
  Actual := SaveCompactUtf8Bytes(Obj);
  Expected := ReferenceCompactUtf8Bytes(Obj);
  ExpectBytes('ascii-json', Actual, Expected, Failures);
end;

procedure TestMultibyteCharacters(var Failures: Integer);
begin
  ExpectRoundTrip('latin1-eacute', 'caf' + #$00E9, Failures);
  ExpectRoundTrip('euro-sign', #$20AC, Failures);
  ExpectRoundTrip('cjk', #$4E2D#$6587, Failures);
  ExpectRoundTrip('emoji', #$D83D#$DE00, Failures);
end;

procedure TestMixedContent(var Failures: Integer);
var
  Obj: IJSONObject;
  Actual, Expected: TBytes;
  Mixed: string;
begin
  Mixed := 'start-' + #$00E9 + '-mid-' + #$D83D#$DE00 + '-end';
  Obj := TJSON.New;
  Obj.Strings['a'] := 'plain';
  Obj.Strings['b'] := Mixed;
  Obj.Strings['c'] := 'tail';
  Actual := SaveCompactUtf8Bytes(Obj);
  Expected := ReferenceCompactUtf8Bytes(Obj);
  ExpectBytes('mixed-object', Actual, Expected, Failures);
end;

function RepeatChar(const C: Char; Count: Integer): string;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 1 to Count do
    Result[I] := C;
end;

procedure TestFlushBoundary(var Failures: Integer);
var
  Obj: IJSONObject;
  Actual, Expected: TBytes;
  Chunk, Value: string;
begin
  Chunk := RepeatChar('x', 30000) + #$00E9 + RepeatChar('y', 30000) + #$D83D#$DE00;
  Value := Chunk + Chunk;
  Obj := TJSON.New;
  Obj.Strings['payload'] := Value;
  Actual := SaveCompactUtf8Bytes(Obj);
  Expected := ReferenceCompactUtf8Bytes(Obj);
  ExpectBytes('flush-boundary', Actual, Expected, Failures);
end;

function RunUtf8SaveTests: Integer;
begin
  Result := 0;
  Writeln('UTF-8 save stream tests');
  TestAsciiAndEscapes(Result);
  TestMultibyteCharacters(Result);
  TestMixedContent(Result);
  TestFlushBoundary(Result);
  if Result = 0 then
    Writeln('All UTF-8 save tests passed.')
  else
    Writeln(Result, ' UTF-8 save test(s) failed.');
end;

end.
