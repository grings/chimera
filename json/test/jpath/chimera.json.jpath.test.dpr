program chimera.json.jpath.test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  chimera.json,
  chimera.json.path;

const
  DEFAULT_JPATH = '$.store.book[*].author';
var
  sPath : string;
begin
  try
    var jso := TJSON.From(
      '{                                                                        '#13#10+
      '    "store": {                                                           '#13#10+
      '        "book": [                                                        '#13#10+
      '            {                                                            '#13#10+
      '                "category": "reference",                                 '#13#10+
      '                "author": "Nigel Rees",                                  '#13#10+
      '                "title": "Sayings of the Century",                       '#13#10+
      '                "price": 8.95                                            '#13#10+
      '            },                                                           '#13#10+
      '            {                                                            '#13#10+
      '                "category": "fiction",                                   '#13#10+
      '                "author": "Evelyn Waugh",                                '#13#10+
      '                "title": "Sword of Honour",                              '#13#10+
      '                "price": 12.99                                           '#13#10+
      '            },                                                           '#13#10+
      '            {                                                            '#13#10+
      '                "category": "fiction",                                   '#13#10+
      '                "author": "Herman Melville",                             '#13#10+
      '                "title": "Moby Dick",                                    '#13#10+
      '                "isbn": "0-553-21311-3",                                 '#13#10+
      '                "price": 8.99                                            '#13#10+
      '            },                                                           '#13#10+
      '            {                                                            '#13#10+
      '                "category": "fiction",                                   '#13#10+
      '                "author": "J. R. R. Tolkien",                            '#13#10+
      '                "title": "The Lord of the Rings",                        '#13#10+
      '                "isbn": "0-395-19395-8",                                 '#13#10+
      '                "price": 22.99                                           '#13#10+
      '            }                                                            '#13#10+
      '        ],                                                               '#13#10+
      '        "bicycle": {                                                     '#13#10+
      '            "color": "red",                                              '#13#10+
      '            "price": 19.95                                               '#13#10+
      '        }                                                                '#13#10+
      '    },                                                                   '#13#10+
      '    "expensive": 10                                                      '#13#10+
      '}'
    );
    WriteLn('Source:');
    Write(jso.AsJSON(TWhitespace.pretty));

    sPath := DEFAULT_JPATH;
    repeat
      WriteLn('');
      WriteLn('Enter a jpath to test ('+sPath+'):');
      Readln(sPath);
      if sPath = '' then
        sPath := DEFAULT_JPATH;

      WriteLn(TJPathParser.AST(sPath));
      WriteLn(jso.Query(sPath).AsJSON(TWhitespace.pretty));
    until False;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
