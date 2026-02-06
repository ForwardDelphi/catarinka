unit CatMsgCromis;

{
  Catarinka TCatMsgCromis - More reliable alternative to WM_COPYDATA messages
  Copyright (c) 2016-2023 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, Winapi.Windows, Winapi.Messages, System.SysUtils,
  Cromis.Comm.Custom, Cromis.Comm.IPC, Cromis.Threading, Cromis.AnyValue;
{$ELSE}
  Classes, Windows, Messages, SysUtils;
{$ENDIF}

type
  TCatMsgCromisOnDataMessage = procedure(const msgid: integer;
    const str: string) of object;
type
  TCatMsgCromisOnRequest = procedure(const Context: ICommContext;
      const Request, Response: IMessageData) of object;

type
  TCatMsgCromis = class
  private
    fIPCServer: TIPCServer;
    fMessageQueue: TThreadSafeQueue;
    fMsgHandle: HWND;
    fOnDataMessage: TCatMsgCromisOnDataMessage;
    fOnRequest: TCatMsgCromisOnRequest;
    fRequestCount: integer;
    function GetServerName: string;
    procedure crmMessage(var AMsg: TMessage);
    procedure OnExecuteRequest(const Context: ICommContext;
      const Request, Response: IMessageData);
    procedure HandleMessage(const AMessage: string);
  public
    constructor Create;
    destructor Destroy; override;
    property OnDataMessage: TCatMsgCromisOnDataMessage read fOnDataMessage
      write fOnDataMessage;
    property OnRequest: TCatMsgCromisOnRequest read fOnRequest write fOnRequest;
    property MsgHandle: HWND read fMsgHandle;
    property Server:TIPCServer read fIPCServer;
    property ServerName: string read GetServerName;
  end;

function SendCromisMessage(desthandle, msgid: integer; l: string):boolean;

implementation

uses
  CatJSON;

const
  WM_OnListBoxMessage = WM_USER + 1;
  WM_OnRequestFinished = WM_USER + 2;

function SendCromisMessageTry(desthandle, msgid: integer; l: string):boolean;
var
  IPCResult: IIPCData;
  Request: IIPCData;
  IPCClient: TIPCClient;
begin
  result := false;
  IPCClient := TIPCClient.Create;
  try
    IPCClient.ComputerName := EmptyStr;
    IPCClient.ServerName := 'CATMSGSERVER' + inttostr(desthandle);
    IPCClient.ConnectClient(cDefaultTimeout);
    try
      if IPCClient.IsConnected then
      begin
        Request := AcquireIPCData;
        Request.ID := DateTimeToStr(Now);
        Request.Data.WriteInteger('CmdID', msgid);
        Request.Data.WriteUTF8String('Command', AnsiString(l));
        IPCResult := IPCClient.ExecuteConnectedRequest(Request);
        result := IPCClient.AnswerValid;
      end;
    finally
      IPCClient.DisconnectClient;
    end;
  finally
    IPCClient.Free;
  end;
end;

function SendCromisMessage(desthandle, msgid: integer; l: string):boolean;
var
  i: Integer;
begin
 result := false;
 for i := 1 to 10 do
  begin
    if SendCromisMessageTry(desthandle, msgid, l) = True then
    begin
      result := true;
      // The function returned True, so break out of the loop
      Break;
    end;
  end;
end;

function TCatMsgCromis.GetServerName: string;
begin
  Result := fIPCServer.ServerName;
end;

procedure TCatMsgCromis.crmMessage(var AMsg: TMessage);
var
  LocalCount: integer;
  MessageValue: TAnyValue;
  j: TCatJSON;
begin
  try
    case AMsg.msg of
      WM_OnListBoxMessage:
        begin
          while fMessageQueue.Dequeue(MessageValue) do
          begin
            if Assigned(OnDataMessage) then
            begin
              j := TCatJSON.Create;
              j.Text := MessageValue.AsString;
              OnDataMessage(j.sObject.I['cmd'], j.sObject.S['s']);
              j.Free;
            end;
          end;
        end;
      WM_OnRequestFinished:
        begin
          InterlockedExchange(LocalCount, fRequestCount);
        end;
    end;
  except
    // Workaround for CEF windows focus related issue, when closing tab while loading
  end;
end;

procedure TCatMsgCromis.HandleMessage(const AMessage: string);
begin
  fMessageQueue.Enqueue(AMessage);
  PostMessage(fMsgHandle, WM_OnListBoxMessage, 0, 0);
end;

procedure TCatMsgCromis.OnExecuteRequest(const Context: ICommContext;
  const Request, Response: IMessageData);
var
  Command: AnsiString;
  CmdId: integer;
  // LocalCount: Integer;
  j: TCatJSON;
begin
  Command := Request.Data.ReadUTF8String('Command');
  CmdId := Request.Data.ReadInteger('CmdID');
  j := TCatJSON.Create;
  j['cmd'] := CmdId;
  j['s'] := Command;
  HandleMessage(j.Text);
  j.Free;
  if Assigned(fOnRequest) then
     fOnRequest(context,request,response);
  // HandleMessage(Format('%s %s request recieved from client %s (Sent at: %s)',
  // [IntToStr(CmdId), Command,Context.Client.ID,Request.ID]));
  // increase the request count thread safe way
  // LocalCount :=
  InterlockedIncrement(fRequestCount);
  PostMessage(fMsgHandle, WM_OnRequestFinished, 0, 0);
end;

constructor TCatMsgCromis.Create;
begin
  fMsgHandle :=
{$IFDEF DXE2_OR_UP}System.{$ENDIF}Classes.AllocateHWnd(crmMessage);
  fMessageQueue := TThreadSafeQueue.Create;
  fIPCServer := TIPCServer.Create;
  fIPCServer.ServerName := 'CATMSGSERVER' + inttostr(fMsgHandle);
  fIPCServer.OnExecuteRequest := OnExecuteRequest;
  fIPCServer.Start;
end;

destructor TCatMsgCromis.Destroy;
begin
{$IFDEF DXE2_OR_UP}System.{$ENDIF}Classes.DeallocateHWnd(fMsgHandle);
  FreeAndNil(fIPCServer);
  FreeAndNil(fMessageQueue);
  inherited;
end;

end.
