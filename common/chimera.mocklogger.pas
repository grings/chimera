unit chimera.mocklogger;

interface

uses
  System.SysUtils;

type
  TLogger = class
  public
    class procedure Profile(MethodName : string);

    class procedure Trace(const Msg : string; const Details : string = '');

    class procedure Info(const Msg : string; const Details : string = '');

    class procedure Warning(const Msg : string; const Details : string = '');

    class procedure Error(e: Exception; const Details : string = '');

//    class property LoggingLevel : TLoggingLevel read FLoggingLevel write SetLoggingLevel;
  end;

implementation

{ TLogger }

class procedure TLogger.Error(e: Exception; const Details : string = '');
begin
  // DO NOTHING
end;

class procedure TLogger.Info(const Msg, Details: string);
begin
  // DO NOTHING
end;

class procedure TLogger.Profile(MethodName: string);
begin
  // DO NOTHING
end;

class procedure TLogger.Trace(const Msg, Details: string);
begin
  // DO NOTHING
end;

class procedure TLogger.Warning(const Msg, Details: string);
begin
  // DO NOTHING
end;

end.
