unit chimera.json.pool.tests;

interface

function RunPoolTests: Integer;

implementation

uses
  System.SysUtils,
  chimera.json;

procedure ExpectTrue(const Name: string; Value: Boolean; var Failures: Integer);
begin
  if Value then
    Writeln('  PASS: ', Name)
  else
  begin
    Writeln('  FAIL: ', Name);
    Inc(Failures);
  end;
end;

procedure FillObject(Obj: IJSONObject; Count: Integer);
var
  I: Integer;
begin
  for I := 1 to Count do
    Obj.Strings['key' + IntToStr(I)] := 'value' + IntToStr(I);
end;

function RunPoolTests: Integer;
var
  Failures: Integer;
  Obj, Nested: IJSONObject;
  Arr: IJSONArray;
  I: Integer;
begin
  Failures := 0;
  Writeln('Pool tests');

  TJSON.EnableThreadCache;
  try
    Obj := TJSON.New;
    FillObject(Obj, 1000);
    ExpectTrue('generate.count', Obj.Count = 1000, Failures);
    ExpectTrue('generate.find', Obj.Strings['key500'] = 'value500', Failures);

    Obj.Clear;
    ExpectTrue('clear.count', Obj.Count = 0, Failures);

    FillObject(Obj, 1000);
    ExpectTrue('reuse.count', Obj.Count = 1000, Failures);
    ExpectTrue('reuse.find', Obj.Strings['key999'] = 'value999', Failures);

    Obj.Strings['nested'] := 'x';
    Nested := TJSON.New;
    Nested.Strings['child'] := 'y';
    Obj.Objects['childObj'] := Nested;
    Arr := TJSONArray.New;
    Arr.Add('a');
    Obj.Arrays['childArr'] := Arr;
    Obj.Clear;
    ExpectTrue('mixed.clear.count', Obj.Count = 0, Failures);

    Obj := TJSON.New;
    Arr := TJSONArray.New;
    for I := 1 to 50 do
      Arr.Add(I);
    for I := 0 to 49 do
      Arr.Delete(0);
    ExpectTrue('array.delete.count', Arr.Count = 0, Failures);
    Arr.Clear;
    ExpectTrue('array.clear.count', Arr.Count = 0, Failures);
  finally
    TJSON.DisableThreadCache;
  end;

  TJSON.DisableThreadCache;
  Obj := TJSON.New;
  FillObject(Obj, 100);
  Obj.Clear;
  ExpectTrue('uncached.clear.count', Obj.Count = 0, Failures);

  if Failures = 0 then
    Writeln('All pool tests passed.')
  else
    Writeln('Pool tests failed: ', Failures);
  Result := Failures;
end;

end.
