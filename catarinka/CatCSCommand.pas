unit CatCSCommand;

{
  Console Output Capturer
  Copyright (c) 2025 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
  Original code of callback function by David Heffernan (@davidheff)

  Changes:
  * 15.01.2025, FD - Revision making code more robust.
  * 28.09.2020, FD - Rewrite based on example by DH.
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, Vcl.Forms, System.Classes, System.SysUtils, Math;
{$ELSE}
  Windows, Forms, Classes, SysUtils, Math;
{$ENDIF}

type
  TCatCSCommandOutput = procedure(const s: String) of object;

{$IFDEF DXE2_OR_UP}
type
    TCmdOutputArg<T> = reference to procedure(const Arg: T);
{$ENDIF}

type
  TCatCSCommand = class
  private
    fOnOutput: TCatCSCommandOutput;
    fPID: Cardinal;
    fTimeout: Cardinal;
    procedure SetTimeout(const ms:Cardinal);
  public
    procedure Run(const Command, Parameters: String);
    constructor Create;
    destructor Destroy; override;
    property OnOutput:TCatCSCommandOutput read fOnOutput write fOnOutput;
    property PID: Cardinal read fPID;
    property Timeout: Cardinal read fTimeout write SetTimeout;
  end;

    {$IFDEF DXE2_OR_UP}
    procedure RunCmdWithCallBack(const Command: string; const Parameters: string;
  CallBack: TCmdOutputArg<string>;const Timeout:dword=100);
    {$ENDIF}

implementation

const
  InheritHandleSecurityAttributes: TSecurityAttributes =
    (nLength: SizeOf(TSecurityAttributes); bInheritHandle: True);

// Revised code, more robust
procedure TCatCSCommand.Run(const Command, Parameters: String);
var
  hReadStdout, hWriteStdout: THandle;
  si: TStartupInfo;
  pi: TProcessInformation;
  WaitRes, BytesRead: DWORD;
  AnsiBuffer: array [0 .. 819200 - 1] of AnsiChar;
 // AnsiBuffer: array [0 .. 4095] of AnsiChar; // 4 KB buffer
  OutputStr: string;
  SecurityAttributes: TSecurityAttributes;
begin
  // Initialize security attributes to allow handle inheritance
  SecurityAttributes.nLength := SizeOf(TSecurityAttributes);
  SecurityAttributes.lpSecurityDescriptor := nil;
  SecurityAttributes.bInheritHandle := True;

  // Create the pipe
  if not CreatePipe(hReadStdout, hWriteStdout, @SecurityAttributes, 0) then
    RaiseLastOSError;

  try
    // Initialize startup info
    ZeroMemory(@si, SizeOf(TStartupInfo));
    si.cb := SizeOf(TStartupInfo);
    si.dwFlags := STARTF_USESTDHANDLES;
    si.hStdOutput := hWriteStdout;
    si.hStdError := hWriteStdout;

    // Create the process
    if not CreateProcess(nil, PChar('"' + Command + '" ' + Parameters), nil, nil,
      True, CREATE_NO_WINDOW, nil, nil, si, pi) then
      RaiseLastOSError;

    fPID := pi.dwProcessId;

    try
      // Close the write end of the pipe in the parent process
      CloseHandle(hWriteStdout);

      while True do
      begin
        WaitRes := WaitForSingleObject(pi.hProcess, fTimeout);

        if WaitRes = WAIT_FAILED then
          RaiseLastOSError;

        // Read from the pipe
        repeat
          if not ReadFile(hReadStdout, AnsiBuffer, SizeOf(AnsiBuffer) - 1, BytesRead, nil) then
            Break;

          if BytesRead > 0 then
          begin
            AnsiBuffer[BytesRead] := #0; // Null-terminate the buffer
            OemToAnsi(AnsiBuffer, AnsiBuffer);
            OutputStr := string(AnsiBuffer);
            if Assigned(fOnOutput) then
              fOnOutput(OutputStr);
          end;

        until BytesRead = 0;

        // Exit if the process has finished
        if WaitRes = WAIT_OBJECT_0 then
          Break;
      end;

    finally
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
    end;

  finally
    CloseHandle(hReadStdout);
  end;
end;

{$IFDEF DXE2_OR_UP}

procedure RunCmdWithCallBack(const Command: string; const Parameters: string;
  CallBack: TCmdOutputArg<string>; const Timeout: DWORD = 100);
var
  hReadStdout, hWriteStdout: THandle;
  si: TStartupInfo;
  pi: TProcessInformation;
  WaitRes, BytesRead: DWORD;
  AnsiBuffer: array [0 .. 819200 - 1] of AnsiChar;
  //AnsiBuffer: array [0 .. 4095] of AnsiChar; // 4 KB buffer, original was 1024 -1
  OutputStr: string;
  SecurityAttributes: TSecurityAttributes;
begin
  // Set up security attributes to allow handle inheritance
  SecurityAttributes.nLength := SizeOf(TSecurityAttributes);
  SecurityAttributes.lpSecurityDescriptor := nil;
  SecurityAttributes.bInheritHandle := True;

  // Create the pipe
  if not CreatePipe(hReadStdout, hWriteStdout, @SecurityAttributes, 0) then
    RaiseLastOSError;

  try
    // Initialize startup info
    ZeroMemory(@si, SizeOf(TStartupInfo));
    si.cb := SizeOf(TStartupInfo);
    si.dwFlags := STARTF_USESTDHANDLES;
    si.hStdOutput := hWriteStdout;
    si.hStdError := hWriteStdout;

    // Create the process
    if not CreateProcess(nil, PChar('"' + Command + '" ' + Parameters), nil, nil,
      True, CREATE_NO_WINDOW, nil, nil, si, pi) then
      RaiseLastOSError;

    try
      CloseHandle(hWriteStdout); // Close write handle in parent process

      while True do
      begin
        WaitRes := WaitForSingleObject(pi.hProcess, Timeout);

        if WaitRes = WAIT_FAILED then
          RaiseLastOSError;

        // Read from the pipe
        repeat
          if not ReadFile(hReadStdout, AnsiBuffer, SizeOf(AnsiBuffer) - 1, BytesRead, nil) then
            Break;

          if BytesRead > 0 then
          begin
            AnsiBuffer[BytesRead] := #0; // Null-terminate the string
            OutputStr := string(AnsiBuffer);
            if Assigned(CallBack) then
              CallBack(OutputStr);
          end;

        until BytesRead = 0;

        if WaitRes = WAIT_OBJECT_0 then
          Break; // Process finished
      end;

    finally
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
    end;

  finally
    CloseHandle(hReadStdout);
  end;
end;
{$ENDIF}

procedure TCatCSCommand.SetTimeout(const ms:Cardinal);
begin
  fTimeout := ms;
end;

constructor TCatCSCommand.Create;
begin
  inherited Create;
  SetTimeout(100);
end;

destructor TCatCSCommand.Destroy;
begin
  inherited;
end;

end.
