unit chimera.json.path;

interface

uses
  System.SysUtils,
  System.Classes,
  System.RegularExpressions,
  chimera.json.path.generators,
  chimera.json;

type
  EEOFException = class(Exception) end;
  ESyntaxError = class(Exception) end;

  TTokenType = (ttNULL, ttWhitespace, ttLength, ttIndex, ttLastIndex, ttFirstIndex, ttSlice, ttBracketProperty, ttDeep, ttWildcard, ttProperty, ttRoot);
  TToken = record
    Typ : TTokenType;
    Val : String;
    class function New(ATyp : TTokenType; const AVal : string) : TToken; static;
    class function Null : TToken; static;
  end;

  TJPathTokenizer = class
  private
    FSource : string;
    FCursor : cardinal;
    function Match(const Reg, Str : string; Typ : TTokenType) : TToken;
  public
    constructor Create(const Source : string); reintroduce;
    function GetNextToken : TToken;
    function HasMoreTokens : boolean;
  end;

  TJPathParser = class
  private
    FTokens : TJPathTokenizer;
    FLookahead : TToken;
    function Eat(const TokenType : TTokenType; BackTick : integer = 0) : TToken;
    function DoLength : IGenerator;
    function DoDeep : IGenerator;
    function DoProperty(FromBracket : Boolean) : IGenerator;
    function DoIndex : IGenerator;
    function DoFirstIndex: IGenerator;
    function DoLastIndex : IGenerator;
    function DoSlice : IGenerator;
    function DoRoot : IGenerator;
    function _Parse(const Source: string) : IGenerator;
    function Next: IGenerator;
    function DoWildcard: IGenerator;
  public
     class function AST(const Source: string) : string;
     class function Parse(const Root : IJSONObject; const Source: string) : IJSONArray;
     destructor Destroy; override;
  end;

implementation

type
  TSpec = record
    Reg : string;
    Typ : TTokenType;
  end;

  TTokenTypeHelper = record helper for TTokenType
    function ToString : string;
  end;

const
  SPEC : array [0..12] of TSpec = (
    (Reg: '^\s+';                           Typ: ttWHITESPACE),
    (Reg: '^\.length\(\)';                  Typ: ttLength),
    (Reg: '^\[(\d+|-\d+)(,(\d+|-\d+))*\]';  Typ: ttINDEX),
    (Reg: '^\[(\d+|-\d+):\]';               Typ: ttLASTINDEX),
    (Reg: '^\[:(\d+|-\d+)\]';               Typ: ttFIRSTINDEX),
    (Reg: '^\[(\d+|-\d+):(\d+|-\d+)\]';     Typ: ttSLICE),
    (Reg: '^\[''[^"]*''\]';                 Typ: ttBRACKETPROPERTY),
    (Reg: '^\[\*\]';                        Typ: ttWILDCARD),
    (Reg: '^\..\*';                         Typ: ttWILDCARD),
    (Reg: '^\.\*';                          Typ: ttWILDCARD),
    (Reg: '^\$';                            Typ: ttROOT),
    (Reg: '^\.\.\w+';                       Typ: ttDEEP),
    (Reg: '^\.\w+';                         Typ: ttPROPERTY)
  );

{ TJPathParser }

class function TJPathParser.AST(const Source: string): string;
begin
  var jpp := TJPathParser.Create;
  try
    var gen := jpp._Parse(Source);
    if gen.&Type <> 'ROOT' then
      raise Exception.Create('Path does not start with root.');
    Result := gen.AST;
  finally
    jpp.Free;
  end;
end;

destructor TJPathParser.Destroy;
begin
  FTokens.Free;
  inherited;
end;

function TJPathParser.DoIndex : IGenerator;
begin
  var Token := Eat(ttINDEX);
  var s := Token.Val;
  s := s.Substring(1,s.Length - 2);
  var ary := s.Split([',']);

  var indexes : TArray<Integer>;
  setLength(indexes, length(ary));
  for var i := Low(ary) to High(ary) do
    indexes[i] :=ary[i].ToInteger;
  Result := TIndexGenerator.Create(indexes, Next);
end;

function TJPathParser.DoLastIndex: IGenerator;
begin
  var Token := Eat(ttLASTINDEX);
  var s := Token.Val;
  s := s.Substring(1,s.Length - 2);
  var ary := s.Split([':']);

  Result := TFromIndexGenerator.Create(ary[0].Trim.ToInteger, Next);
end;

function TJPathParser.DoLength: IGenerator;
begin
  var Token := Eat(ttLENGTH);
  Result := TLengthGenerator.Create(Next);
end;

function TJPathParser.DoDeep: IGenerator;
begin
  var Token := Eat(ttDEEP);
  Result := TDeepGenerator.Create(Token.Val.Substring(2), Next);
end;

function TJPathParser.DoFirstIndex: IGenerator;
begin
  var Token := Eat(ttLASTINDEX);
  var s := Token.Val;
  s := s.Substring(1,s.Length - 2);
  var ary := s.Split([':']);

  Result := TToIndexGenerator.Create(ary[1].Trim.ToInteger, Next);
end;

function TJPathParser.DoProperty(FromBracket: Boolean): IGenerator;
var
  Token : TToken;
  prop : string;
begin
  if FromBracket then
    Token := Eat(ttBRACKETPROPERTY)
  else
    Token := Eat(ttPROPERTY);

  if FromBracket then
    prop := Token.Val.Substring(2, Token.Val.Length - 4)
  else
    prop := Token.Val.Substring(1);

  Result := TPropertyGenerator.Create(prop, Next);
end;

function TJPathParser.DoRoot: IGenerator;
begin
  Eat(ttROOT);
  Result := TRootGenerator.Create(Next);
end;

function TJPathParser.DoWildcard : IGenerator;
begin
  var Token := Eat(ttWildcard);
  var s := Token.Val;
  if s.StartsWith('[') then
    Result := TAllIndexesGenerator.Create(Next)
  else if s.StartsWith('..') then
    Result := TAllPropertiesGenerator.Create(Next, True)
  else
    Result := TAllPropertiesGenerator.Create(Next, False);
end;

function TJPathParser.DoSlice: IGenerator;
begin
  var Token := Eat(ttSLICE);
  var s := Token.Val;
  s := s.Substring(1,s.Length - 2);
  var ary := s.Split([':']);

  Result := TSliceGenerator.Create(ary[0].Trim.ToInteger, ary[1].Trim.ToInteger, Next);
end;

function TJPathParser.Eat(const TokenType: TTokenType; BackTick : integer = 0): TToken;
begin
  if FLookahead.Typ = ttNULL then
  begin
    raise EEOFException.Create('Unexpected End of Input. Expected '+TokenType.ToString);
  end;
  Result := FLookahead;
  if Result.Typ <> TokenType then
    raise ESyntaxError.Create('Unexpected Token. Found '+Result.Typ.ToString+' Expected '+TokenType.ToString);

  FLookahead := FTokens.GetNextToken;
end;

function TJPathParser.Next : IGenerator;
begin
  case FLookahead.Typ of
    ttNULL:
      Exit(nil);
    ttWhitespace:
      Result := Next;
    ttLength:
      Result := DoLength;
    ttIndex:
      Result := DoIndex;
    ttLastIndex:
      Result := DoLastIndex;
    ttFirstIndex:
      Result := DoFirstIndex;
    ttSlice:
      Result := DoSlice;
    ttBracketProperty:
      Result := DoProperty(True);
    ttDeep:
      Result := DoDeep;
    ttWildcard:
      Result := DoWildcard;
    ttProperty:
      Result := DoProperty(False);
    ttRoot:
      Result := DoRoot;
    else
    raise ESyntaxError.Create('Unexpected Token Type. Encountered '+FLookahead.Val);
  end;

end;

class function TJPathParser.Parse(const Root : IJSONObject; const Source: string): IJSONArray;
var
  jpp : TJPathParser;
  gen : IGenerator;
  ary : TMultiValues;
begin
  Result := TJSONArray.New;

  jpp := TJPathParser.Create;
  try
    gen := jpp._Parse(Source);
    if gen.&Type <> 'ROOT' then
      raise Exception.Create('Path does not start with root.');
    if Root.IsSimpleValue then
      ary := gen.Process([Root.AsValue])
    else
      ary := gen.Process([TMultiValue.Initialize(Root)]);
    Result := TJSONArray.From(ary);
  finally
    jpp.Free;
  end;
end;

function TJPathParser._Parse(const Source: string): IGenerator;
begin
  FTokens := TJPathTokenizer.Create(Source);
  FLookahead := FTokens.GetNextToken;
  Result := DoRoot;
end;

{ TJPathTokenizer }

constructor TJPathTokenizer.Create(const Source: string);
begin
  inherited Create;
  FSource := Source;
  FCursor := 0;
end;

function TJPathTokenizer.GetNextToken : TToken;
var
  i : integer;
  sp : TSpec;
begin
  if not HasMoreTokens then
    Exit(TToken.Null);

  var s := FSource.Substring(FCursor);

  for i := Low(SPEC) to High(SPEC) do
  begin
    sp := SPEC[i];
    var value := Match(sp.Reg, s, sp.Typ);
    if value.Typ <> ttNULL then
      Exit(value);
  end;
  raise Exception.Create('Syntax Error. Unexpected token '+s.Chars[0]);
end;

function TJPathTokenizer.HasMoreTokens: boolean;
begin
  Result := FCursor < FSource.Length;
end;

function TJPathTokenizer.Match(const Reg, Str : string; Typ : TTokenType): TToken;
begin
  var Match := TRegEx.Match(str,Reg);
  if not Match.Success then
    Exit(TToken.Null);

  inc(FCursor, Match.Length);
  Result := TToken.New(Typ, Match.Value);
end;

{ TToken }

class function TToken.New(ATyp: TTokenType; const AVal: string): TToken;
begin
  Result.Typ := ATyp;
  Result.Val := AVal;
end;

class function TToken.Null: TToken;
begin
  Result.Typ := ttNULL;
  Result.Val := '';
end;

{ TTokenTypeHelper }

function TTokenTypeHelper.ToString: string;
begin
  case Self of
    ttNULL:
      Result := 'NULL';
    ttWhitespace:
      Result := 'WHITESPACE';
    ttLength:
      Result := 'LENGTH';
    ttIndex:
      Result := 'INDEX';
    ttLastIndex:
      Result := 'INDEX';
    ttFirstIndex:
      Result := 'INDEX';
    ttSlice:
      Result := 'SLICE';
    ttBracketProperty:
      Result := 'BRACKET';
    ttDeep:
      Result := 'DEEP';
    ttWildcard:
      Result := 'WILDCARD';
    ttProperty:
      Result := 'PROPERTY';
    ttRoot:
      Result := 'ROOT';
  end;
end;

end.
