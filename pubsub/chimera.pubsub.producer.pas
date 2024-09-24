// *****************************************************************************
//
// chimera.pubsub.producer;
//
// PubSub Chimera project for Delphi
//
// Copyright (c) 2014 by Sivv Corp, All Rights Reserved
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

unit chimera.pubsub.producer;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.SyncObjs,
  Web.HTTPApp,
  chimera.json,
  chimera.pubsub.common,
  chimera.pubsub,
  chimera.pubsub.interfaces;

type
  TIDEvent = procedure(Sender : TObject; Request : TWebRequest; Response : TWebResponse; var ID : string) of object;

  TPubSubAuthEvent = procedure(Sender : TObject; Request : TWebRequest; const channel : string; var Permitted : boolean) of object;

  TParseChannelEvent = procedure(Sender : TObject; Request : TWebRequest; var Value : string) of object;

  TParseDataEvent = procedure(Sender : TObject; Request : TWebRequest; var Value : IJSONObject) of object;

  TPubSubProducer = class(TCustomContentProducer, IProduceContent)
  strict private type
    TSubscription = class(TObject)
    private
      FID : string;
      FChannel : string;
      FCS : TMultiReadExclusiveWriteSynchronizer;
      FMessages : TQueue<IJSONObject>;
      FHandler : TMessageHandler<IJSONObject>;
      FEvent : TEvent;
      FDestroying : boolean;
    protected
      procedure Subscribe;
      procedure Unsubscribe;
    public
      constructor Create(const AChannel, AID : string); reintroduce;
      destructor Destroy; override;
      function ExtractMessages(Timeout : Integer) : IJSONArray;
    end;
  strict private
    class var FPubSub : TPubSub<IJSONObject>;
    class var FSubscriptions : TDictionary<string, TSubscription>;
    class var FSubsCS : TMultiReadExclusiveWriteSynchronizer;
  private
    FOnSession: TIDEvent;
    FOnGetID : TIDEvent;
    FOnCanSubscribe: TPubSubAuthEvent;
    FOnCanPublish: TPubSubAuthEvent;
    FOnParseMessage: TParseDataEvent;
    FOnParseChannel: TParseChannelEvent;
    FTimeout: LongWord;
  protected
    function ParseChannel : string; virtual;
    function ParseMessage : IJSONObject; virtual;
    function CanPublish : boolean;
    function CanSubscribe : boolean;
    function DoGetID : string;

    function GenerateKey(const Channel, ID : string) : string; inline;

    function PersistentUnsubscribe(const Channel, ID : string) : boolean;
    function PersistentWaitForMessages(const Channel, ID : string; Timeout : integer) : IJSONArray;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Content: string; override;
    class function PubSub : TPubSub<IJSONObject>;
    class procedure UnsubscribeAll;
    class constructor Create;
    class destructor Destroy;
  published
    property Timeout : LongWord read FTimeout write FTimeout default High(LongWord);
    property OnSession : TIDEvent read FOnSession write FOnSession;
    property OnCanSubscribe : TPubSubAuthEvent read FOnCanSubscribe write FOnCanSubscribe;
    property OnCanPublish : TPubSubAuthEvent read FOnCanPublish write FOnCanPublish;
    property OnParseChannel : TParseChannelEvent read FOnParseChannel write FOnParseChannel;
    property OnParseMessage : TParseDataEvent read FOnParseMessage write FOnParseMessage;
    property OnGetID : TIDEvent read FOnGetID write FOnGetID;
  end;

implementation

{$IFDEF USELOGGER}
uses
  ideal.logger;
{$ELSE}
uses
  chimera.mocklogger;
{$ENDIF}

{ TPubSubProducer }

function TPubSubProducer.CanPublish: boolean;
begin
  Result := True;
  if Assigned(FOnCanPublish) then
  begin
    TLogger.Profile('TPubSubProducer.CanPublish');
    FOnCanPublish(Self, Dispatcher.Request, ParseChannel, Result);
  end;
end;

function TPubSubProducer.CanSubscribe: boolean;
begin
  Result := True;
  if Assigned(FOnCanSubscribe) then
  begin
    TLogger.Profile('TPubSubProducer.CanSubscribe');
    FOnCanSubscribe(Self, Dispatcher.Request, ParseChannel, Result);
  end;
end;

function TPubSubProducer.Content: string;
var
  sSession : string;
  i: Integer;
  jsa : IJSONArray;
begin
  TLogger.Profile('TPubSubProducer.Content '+Dispatcher.Request.Method);
  try
    case Dispatcher.Request.MethodType of
      TMethodType.mtPost,
      TMethodType.mtPut:
      begin
        if CanPublish then
          PubSub.Publish(ParseChannel, ParseMessage, DoGetID)
        else
          raise EPubSubSecurityException.Create(NOT_ALLOWED);
      end;
      TMethodType.mtGet:
      begin
        if CanSubscribe then
        begin
          sSession := '';
          if Assigned(FOnSession) then
          begin
            TLogger.Trace('TPubSubProducer.Content OnSession Assigned');
            // If a session is provided, then use queueing mechanism
            FOnSession(Self, Dispatcher.Request, Dispatcher.Response, sSession);
            jsa := TJSONArray.From<IJSONObject>(
                PubSub.ListenAndWait(ParseChannel, sSession, FTimeout, DoGetID)
              );
            TLogger.Trace('TPubSubProducer.Content Listen and wait returned '+jsa.AsJSON);
          end else
          begin
            TLogger.Trace('TPubSubProducer.Content NO OnSession Assigned');
            // If no session provided, just wait for next message
            jsa := PersistentWaitForMessages(ParseChannel, DoGetID, FTimeout);
            TLogger.Trace('TPubSubProducer.Content Persistent Wait returned '+jsa.AsJSON);
          end;

          Result := jsa.AsJSON;
          Dispatcher.Response.ContentType := 'application/json';

          if sSession <> '' then
          begin
            TLogger.Trace('TPubSubProducer.Content Clearing each response message for pubsub session');
            jsa.Each(
              procedure(const jsn: IJSONObject)
              begin
                PubSub.ClearMsg(ParseChannel,sSession,jsn);
              end
            );
            TLogger.Trace('TPubSubProducer.Content Finished Clearing each response message for pubsub session');
          end;
        end else
          raise EPubSubSecurityException.Create(NOT_ALLOWED);
      end;
      TMethodType.mtDelete:
      begin
        var id := DoGetID;
        var channel := ParseChannel;
        if not PersistentUnsubscribe(channel, id) then
          PubSub.Unsubscribe(channel, nil, id);
      end;
    end;
  except
    on e: exception do
    begin
      TLogger.Error(e, 'TPubSubProducer.Content Error');
      Dispatcher.Response.StatusCode := 501;
      Dispatcher.Response.ReasonString := e.Message;
      Result := e.Message;
    end;
  end;
end;

class constructor TPubSubProducer.Create;
begin
  FSubsCS := TMultiReadExclusiveWriteSynchronizer.Create;
  FSubscriptions := TDictionary<string, TSubscription>.Create;
end;

constructor TPubSubProducer.Create(AOwner: TComponent);
begin
  inherited;
  SetAppDispatcher(AOwner);
  FTimeout := High(LongWord);
end;

class destructor TPubSubProducer.Destroy;
begin
  FSubscriptions.Free;
  FSubsCS.Free;
end;

destructor TPubSubProducer.Destroy;
begin
  inherited;
end;

function TPubSubProducer.DoGetID: string;
begin
  Result := '';
  if Assigned(FOnGetID) then
  begin
    TLogger.Profile('TPubSubProducer.DoGetID');
    FOnGetID(Self, Dispatcher.Request, Dispatcher.Response, Result);
  end;
end;

function TPubSubProducer.GenerateKey(const Channel, ID: string): string;
begin
  Result := ID+'@'+Channel;
end;

function TPubSubProducer.ParseChannel: string;
begin
  TLogger.Profile('TPubSubProducer.ParseChannel');
  Result := String(Dispatcher.Request.PathInfo);
  if Assigned(FOnParseChannel) then
    FOnParseChannel(Self, Dispatcher.Request, Result);
end;

function TPubSubProducer.ParseMessage : IJSONObject;
begin
  TLogger.Profile('TPubSubProducer.ParseMessage');
  Result := TJSON.From(Dispatcher.Request.Content);
  if Assigned(FOnParseMessage) then
    FOnParseMessage(Self, Dispatcher.Request, Result);
end;

function TPubSubProducer.PersistentUnsubscribe(const Channel, ID: string): boolean;
begin
  TLogger.Profile('TPubSubProducer.PersistentUnsubscribe('+Channel+', '+ID+')');
  var key := GenerateKey(Channel, ID);
  FSubsCS.BeginRead;
  try
    result := FSubscriptions.ContainsKey(key);

    if result then
    begin
      FSubsCS.BeginWrite;
      try
        FSubscriptions.ExtractPair(key).Value.Free;
      finally
        FSubsCS.EndWrite;
      end;
    end;
  finally
    FSubsCS.EndRead;
  end;
end;

class function TPubSubProducer.PubSub: TPubSub<IJSONObject>;
begin
  if not Assigned(FPubSub) then
    FPubSub := TPubSub<IJSONObject>.Create;
  Result := FPubSub;
end;

class procedure TPubSubProducer.UnsubscribeAll;
begin
  TLogger.Profile('TPubSubProducer.UnsubscribeAll');
  FSubsCS.BeginWrite;
  try
    for var p in FSubscriptions.ToArray do
      p.Value.Free;

    FSubscriptions.Clear;
  finally
    FSubsCS.EndWrite;
  end;

end;

function TPubSubProducer.PersistentWaitForMessages(const Channel, ID: string; Timeout : Integer): IJSONArray;
var
  sub : TSubscription;
begin
  TLogger.Profile('TPubSubProducer.PersistentWaitForMessages('+Channel+', '+ID+', '+TImeout.ToString);
  try
    sub := nil;
    var key := GenerateKey(Channel, ID);

    FSubsCS.BeginRead;
    try
      if not FSubscriptions.TryGetValue(key, sub) then
      begin
        sub := TSubscription.Create(Channel, ID);
        FSubsCS.BeginWrite;
        try
          var sub2 := sub;
          if not FSubscriptions.TryGetValue(key, sub) then
          begin
            FSubscriptions.Add(key, sub2);
            sub := sub2;
          end else
            sub2.Free;
        finally
          FSubsCS.EndWrite;
        end;
      end;
    finally
      FSubsCS.EndRead;
    end;

    if Assigned(sub) then
    begin
      TLogger.Trace('Assigned sub');
      Result := sub.ExtractMessages(Timeout);
    end;
  except
    on E: Exception do
    begin
      TLogger.Error(E);
      raise;
    end;

  end;
end;

{ TPubSubProducer.TSubscription }

constructor TPubSubProducer.TSubscription.Create(const AChannel, AID : string);
begin
  inherited Create;
  FDestroying := False;
  FCS := TMultiReadExclusiveWriteSynchronizer.Create;
  FID := AID;
  FChannel := AChannel;
  FMessages := TQueue<IJSONObject>.Create;
  FEvent := TEvent.Create;
  FHandler :=
    procedure(const Msg : IJSONObject)
    begin
      FCS.BeginWrite;
      try
        FMessages.Enqueue(Msg);
      finally
        FCS.EndWrite;
      end;
      FEvent.SetEvent;
    end;
  Subscribe;
end;

destructor TPubSubProducer.TSubscription.Destroy;
begin
  FDestroying := True;
  FEvent.SetEvent;
  Sleep(10);
  Unsubscribe;
  FEvent.Free;
  FMessages.Free;
  FHandler := nil;
  FCS.Free;
  inherited;
end;

function TPubSubProducer.TSubscription.ExtractMessages(Timeout : Integer): IJSONArray;
var
  bNeedToWait : boolean;
  bHasData : boolean;
begin
  if FDestroying then
    exit;
  TLogger.Profile('TPubSubProducer.TSubscription.ExtractMessages '+Timeout.ToString);
  try
    Result := TJSONArray.New;
    bHasData := False;

    FCS.BeginRead;
    try
      bNeedToWait := FMessages.Count = 0;
    finally
      FCS.EndRead;
    end;
    TLogger.Trace('bNeedToWait = '+BoolToStr(bNeedToWait, True));

    if bNeedToWait then
      bHasData := FEvent.WaitFor(Timeout) = wrSignaled
    else
      bHasData := True;

    if FDestroying then
    begin
      TLogger.Trace('TPubSubProducer.TSubscription.ExtractMessages Exiting Early due to signal during destruction.');
      exit;
    end;

    if bHasData then
    begin
      TLogger.Trace('bHasData is True after signal');
      FCS.BeginWrite;
      try
        while FMessages.Count > 0 do
          Result.Add(FMessages.Dequeue);
      finally
        FCS.EndWrite;
      end;
    end else
      TLogger.Trace('bHasData is False after signal');

    TLogger.Trace('Resetting Event');
    FEvent.ResetEvent;
  except
    on E: Exception do
    begin
      TLogger.Error(E);
      raise;
    end;
  end;
end;

procedure TPubSubProducer.TSubscription.Subscribe;
begin
  if FDestroying then
    exit;
  TLogger.Profile('TPubSubProducer.TSubscription.Subscribe');
  PubSub.Subscribe(
    FChannel,
    FHandler,
    FID
  );
end;

procedure TPubSubProducer.TSubscription.Unsubscribe;
begin
  TLogger.Profile('TPubSubProducer.TSubscription.Unsubscribe');
  PubSub.Unsubscribe(FChannel, FHandler, FID);
end;

initialization
  //

finalization
  TPubsubProducer.UnsubscribeAll;
  Sleep(1000);

end.
