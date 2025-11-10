unit chimera.json.helpers.db;

interface

uses System.Classes, chimera.json, Data.DB;

type
  TDatasetHelper = class helper for TDataset
  private

    function GetAsJSONArray: IJSONArray;
    function GetAsJSON : IJSONObject;

  public

    property AsJSONArray : IJSONArray read GetAsJSONArray;

    procedure UpdateFields(obj : IJSONObject);

    property AsJSON : IJSONObject read GetAsJSON;

  end;

implementation

uses System.SysUtils;

{ TDatasetHelper }

function TDatasetHelper.GetAsJSON: IJSONObject;
  function FieldKindToString(FieldKind : TFieldKind) : string;
  begin
    case FieldKind of
      fkData:         Result := 'DATA';
      fkCalculated:   Result := 'CALCULATED';
      fkLookup:       Result := 'LOOKUP';
      fkInternalCalc: Result := 'INTERNAL_CALC';
      fkAggregate:    Result := 'AGGREGATE';
    end;
  end;
  function AlignmentToString(Alignment : TAlignment) : string;
  begin
    case Alignment of
      taLeftJustify:  Result := 'left';
      taRightJustify: Result := 'right';
      taCenter:       Result := 'center';
    end;
  end;
  function FieldTypeToString(FieldType : TFieldType) : string;
  begin
    case FieldType of
      ftUnknown: Result := 'Unknown';
      ftString: Result := 'String';
      ftSmallint: Result := 'Smallint';
      ftInteger: Result := 'Integer';
      ftWord: Result := 'Word';
      ftBoolean: Result := 'Boolean';
      ftFloat: Result := 'Float';
      ftCurrency: Result := 'Currency';
      ftBCD: Result := 'BCD';
      ftDate: Result := 'Date';
      ftTime: Result := 'Time';
      ftDateTime: Result := 'DateTime';
      ftBytes: Result := 'Bytes';
      ftVarBytes: Result := 'VarBytes';
      ftAutoInc: Result := 'AutoInc';
      ftBlob: Result := 'Blob';
      ftMemo: Result := 'Memo';
      ftGraphic: Result := 'Graphic';
      ftFmtMemo: Result := 'FmtMemo';
      ftParadoxOle: Result := 'ParadoxOle';
      ftDBaseOle: Result := 'DBaseOle';
      ftTypedBinary: Result := 'TypedBinary';
      ftCursor: Result := 'Cursor';
      ftFixedChar: Result := 'FixedChar';
      ftWideString: Result := 'WideString';
      ftLargeint: Result := 'Largeint';
      ftADT: Result := 'ADT';
      ftArray: Result := 'Array';
      ftReference: Result := 'Reference';
      ftDataSet: Result := 'DataSet';
      ftOraBlob: Result := 'OraBlob';
      ftOraClob: Result := 'OraClob';
      ftVariant: Result := 'Variant';
      ftInterface: Result := 'Interface';
      ftIDispatch: Result := 'IDispatch';
      ftGuid: Result := 'Guid';
      ftTimeStamp: Result := 'TimeStamp';
      ftFMTBcd: Result := 'FMTBcd';
      ftFixedWideChar: Result := 'FixedWideChar';
      ftWideMemo: Result := 'WideMemo';
      ftOraTimeStamp: Result := 'OraTimeStamp';
      ftOraInterval: Result := 'OraInterval';
      ftLongWord: Result := 'LongWord';
      ftShortint: Result := 'Shortint';
      ftByte: Result := 'Byte';
      ftExtended: Result := 'Extended';
      ftConnection: Result := 'Connection';
      ftParams: Result := 'Params';
      ftStream: Result := 'Stream';
      ftTimeStampOffset: Result := 'TimeStampOffset';
      ftObject: Result := 'Object';
      ftSingle: Result := 'Single';
    end;
  end;
  function DisplayFormatFromField(Field : TField) : string;
  begin
    if Field is TNumericField then
      Result := TNumericField(Field).DisplayFormat
    else if Field is TDateTimeField then
      Result := TDateTimeField(Field).DisplayFormat
    else if Field is TSQLTimeStampField then
      Result := TSQLTimeStampField(Field).DisplayFormat
    else if Field is TAggregateField then
      Result := TAggregateField(Field).DisplayFormat
    else
      Result := '';
  end;
  function GetFieldArray : IJSONArray;
  begin
    Result := TJSONArray.New;
    for var i := 0 to Fields.Count-1 do
    begin
      var field := TJSON.New;

      field.Strings['name'] := Fields[i].FieldName;
      field.Strings['kind'] := FieldKindToString(Fields[i].FieldKind);
      field.Strings['alignment'] := AlignmentToString(Fields[i].Alignment);
      field.Strings['display_label'] := Fields[i].DisplayLabel;
      field.Strings['display_name'] := Fields[i].DisplayName;
      field.Integers['display_width'] := Fields[i].DisplayWidth;
      field.Integers['index'] := Fields[i].Index;
      field.Booleans['read_only'] := Fields[i].ReadOnly;
      field.Booleans['required'] := Fields[i].Required;
      field.Booleans['visible'] := Fields[i].Visible;
      field.Integers['data_size'] := Fields[i].DataSize;
      field.Strings['type'] := FieldTypeToString(Fields[i].DataType);
      field.Integers['size'] := Fields[i].Size;
      field.Strings['display_format'] := DisplayFormatFromField(Fields[i]);
      Result.Add(field);
    end;

  end;
begin
  Result := TJSON.New;
  Result.Strings['name'] := Self.Name;
  Result.Strings['class_name'] := Self.ClassName;
  Result.Arrays['fields'] := GetFieldArray;
  Result.Arrays['data'] := Self.AsJSONArray;
end;

function TDatasetHelper.GetAsJSONArray: IJSONArray;
var
  jso : IJSONObject;
  i: Integer;
  bs : TBytesStream;
begin
  Result := JSONArray;
  while not EOF do
  begin
    jso := JSON;
    for i := 0 to FieldCount-1 do
    begin
      if not Fields[i].IsNull then
      begin
        case Fields[i].DataType of
          ftUnknown,
          ftString,
          ftFixedChar,
          ftWideString,
          ftVariant,
          ftFixedWideChar,
          ftWideMemo:
            jso.Strings[Fields[i].FieldName] := Fields[i].AsWideString;

          ftGuid:
            jso.Guids[Fields[i].FieldName] := StringToGuid(Fields[i].AsString);

          TFieldType.ftSingle,
          TFieldType.ftExtended,
          ftCurrency,
          ftFloat,
          ftFMTBcd,
          ftBCD:
            jso.Numbers[Fields[i].FieldName] := Fields[i].AsExtended;

          ftSmallint,
          ftInteger,
          ftWord,
          ftAutoInc,
          ftLargeint,
          ftOraInterval,
          ftLongWord,
          ftShortint,
          ftByte,
          ftTimeStampOffset:
            jso.Integers[Fields[i].FieldName] := Fields[i].AsLargeInt;

          ftBoolean:
            jso.Booleans[Fields[i].FieldName] := Fields[i].AsBoolean;

          ftDate,
          ftTime,
          ftDateTime,
          ftTimeStamp,
          ftOraTimeStamp:
            jso.Dates[Fields[i].FieldName] := Fields[i].AsDateTime;


          ftBytes,
          ftVarBytes:
            jso.Bytes[Fields[i].FieldName] := Fields[i].AsBytes;

          ftBlob,
          ftMemo,
          ftGraphic,
          ftFmtMemo:
          begin
            bs := TBytesStream.Create;
            try
              TBlobField(Fields[i]).SaveToStream(bs);
              bs.Position := 0;
              jso.Bytes[Fields[i].FieldName] := bs.Bytes;
            finally
              bs.Free;
            end;
          end;

          ftParadoxOle,
          ftDBaseOle,
          ftCursor,
          ftADT,
          ftArray,
          ftReference,
          ftDataSet,
          ftOraBlob,
          ftOraClob,
          ftInterface,
          ftIDispatch,
          ftConnection,
          ftParams,
          ftStream,
          ftObject,
          ftTypedBinary:
            begin
              // unsupported and skip
              Next;
              Continue;
            end;
        end;
      end else
        jso.AddNull(Fields[i].FieldName);
    end;
    Result.Add(jso);
    Next;
  end;
end;

procedure TDatasetHelper.UpdateFields(obj: IJSONObject);
begin
  obj.Each(
    procedure(const Name : string; const Value : PMultiValue)
    begin
      case Value.ValueType of
        TJSONValueType.&string:
          FieldByName(Name).AsString := Value.StringValue;
        TJSONValueType.number:
          if (Value.NumberValue = Round(Value.NumberValue)) and
             (Value.NumberValue <> Value.IntegerValue) then
            FieldByName(Name).AsLargeInt := Value.IntegerValue
          else
            FieldByName(Name).AsExtended := Value.NumberValue;
        TJSONValueType.&array,
        TJSONValueType.&object:
          // Currently Unsupported, skip
          ;
        TJSONValueType.boolean:
          FieldByName(Name).AsBoolean := Value.IntegerValue <> 0;
        TJSONValueType.null:
          FieldByName(Name).Clear;
      end;

    end
  );
end;

end.