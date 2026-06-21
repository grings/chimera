// *****************************************************************************
//
// chimera.json.parser.builder;
//
// Internal parser-to-object-builder bridge for JSON Chimera.
//
// *****************************************************************************

unit chimera.json.parser.builder;

interface

{$I chimera.inc}

uses
  chimera.json;

type
  TParseObjectBeginProc = procedure(const Obj: IJSONObject);
  TParseObjectEndProc = procedure(const Obj: IJSONObject);
  TParseObjectValueProc = procedure(const Obj: IJSONObject; const Name: string; const Value: TMultiValue);
  TParseObjectObjectProc = procedure(const Obj: IJSONObject; const Name: string; const Value: IJSONObject);
  TParseObjectArrayProc = procedure(const Obj: IJSONObject; const Name: string; const Value: IJSONArray);
  TParseObjectNullProc = procedure(const Obj: IJSONObject; const Name: string);

  TParseArrayBeginProc = procedure(const Arr: IJSONArray);
  TParseArrayEndProc = procedure(const Arr: IJSONArray);
  TParseArrayValueProc = procedure(const Arr: IJSONArray; const Value: TMultiValue);
  TParseArrayObjectProc = procedure(const Arr: IJSONArray; const Value: IJSONObject);
  TParseArrayArrayProc = procedure(const Arr: IJSONArray; const Value: IJSONArray);
  TParseArrayNullProc = procedure(const Arr: IJSONArray);

  TJSONParserBuilder = record
  public
    class var ObjectBegin: TParseObjectBeginProc;
    class var ObjectEnd: TParseObjectEndProc;
    class var ObjectValue: TParseObjectValueProc;
    class var ObjectObject: TParseObjectObjectProc;
    class var ObjectArray: TParseObjectArrayProc;
    class var ObjectNull: TParseObjectNullProc;

    class var ArrayBegin: TParseArrayBeginProc;
    class var ArrayEnd: TParseArrayEndProc;
    class var ArrayValue: TParseArrayValueProc;
    class var ArrayObject: TParseArrayObjectProc;
    class var ArrayArray: TParseArrayArrayProc;
    class var ArrayNull: TParseArrayNullProc;
  end;

implementation

end.
