unit CatTasks;
{
  Catarinka - Task Management library

  Copyright (c) 2003-2025 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, Vcl.Forms, System.SysUtils, System.Classes, Winapi.TlHelp32, Winapi.PSAPI;
{$ELSE}
  Windows, Forms, SysUtils, Classes, TlHelp32, PSAPI;
{$ENDIF}
function GetProcessWorkingSetSize:int64;
function KillTask(const ExeFileName: string;FullName:boolean=false): Integer;
function KillOtherTasks(const ExeFileName: string; FullName: boolean = false): Integer;
function KillChildTasks: boolean;
function MatchProcessFilename(pe:TProcessEntry32; const ExeFileName: string; FullName:boolean=false):boolean;
function RunTask(const ExeFileName: string; const Wait: boolean = false;
  const WindowState: Integer = SW_SHOW): Cardinal;
function RunTaskWaitMax(const ExeFileName: string; const MaxMinutes: Integer;
  const WindowState: Integer = SW_SHOW): Cardinal;
function TaskRunning(const ExeFileName: WideString;const FullName:boolean): boolean;
function TaskRunningCount(const ExeFileName: WideString;const FullName:boolean): integer;
function TaskRunningSingleInstance(const ExeFileName: WideString;const FullName:boolean): boolean;
function TaskRunningWithPID(const ExeFileName: WideString; const PID: Cardinal;const FullName:boolean): boolean;
procedure GetTaskList(ProcList: TStrings;FileMask:string='');
procedure GetTaskListEx(ProcList: TStrings;FileMask:string;FullName:boolean);
procedure KillTaskByMask(FileMask:string);
procedure KillTaskbyPID(const PID: Cardinal);
procedure KillTaskList(ProcList: TStringList);
procedure ResumeProcess(const ProcessID: DWORD);
procedure SuspendProcess(const ProcessID: DWORD);
function IsUserAnAdmin(): BOOL; external 'shell32.dll';
procedure RunProcessAsUser(const TargetPID:Cardinal;const CommandLine:string);
procedure RunProcessRestricted(const CommandLine:string);

implementation

uses CatStrings, CatMatch;

const
  THREAD_SUSPEND_RESUME = $00000002;
  DISABLE_MAX_PRIVILEGE = $1;
  cProcSep = '|pid=';


function CreateRestrictedToken(
  ExistingTokenHandle: THandle;
  Flags: DWORD;
  DisableSidCount: DWORD;
  SidsToDisable: PSidAndAttributes;
  DeletePrivilegeCount: DWORD;
  PrivilegesToDelete: PTokenPrivileges;
  RestrictedSidCount: DWORD;
  SidsToRestrict: PSidAndAttributes;
  out NewTokenHandle: THandle
): BOOL; stdcall; external 'advapi32.dll' name 'CreateRestrictedToken';

function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  dwThreadId: DWORD): DWORD; stdcall; external 'kernel32.dll';

// Gets the full filename of a process ID
function GetFilenameByPID(const PID: THandle): WideString;
const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;
type
  TQueryFullProcessImageNameW = function(hProcess: THandle; dwFlags: DWORD; lpExeName: PWideChar; nSize: PDWORD): BOOL; stdcall;
var
  hProcess: THandle;
  TargetName: WideString;
  QueryFullProcessImageNameW: TQueryFullProcessImageNameW;
  nSize: cardinal;
begin
  Result := '';
  nSize := MAX_PATH;
  SetLength(TargetName, nSize);
  if Win32MajorVersion >= 6 then begin
    hProcess := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, false, PID);
    if hProcess <> 0 then begin
      try
        @QueryFullProcessImageNameW := GetProcAddress(GetModuleHandle('kernel32'), 'QueryFullProcessImageNameW');
        if Assigned(QueryFullProcessImageNameW) then
          if QueryFullProcessImageNameW(hProcess, 0, PWideChar(TargetName), @nSize) then
            Result := PWideChar(TargetName);
      finally
        CloseHandle(hProcess);
      end;
    end;
  end else begin
    hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false, PID);
    if hProcess <> 0 then
      try
        if GetModuleFileNameExW(hProcess, 0, PWideChar(TargetName), nSize) <> 0 then
          Result := PWideChar(TargetName);
    finally
      CloseHandle(hProcess);
    end;
  end;
end;

// Gets a list of tasks, optionally based on a mask
procedure GetTaskList(ProcList: TStrings;FileMask:string='');
begin
  GetTaskListEx(ProcList, FileMask, false);
end;

// Gets a list of tasks, optionally based on a mask and with full filename
procedure GetTaskListEx(ProcList: TStrings;FileMask:string;FullName:boolean);
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
  fn: string;
  canadd: boolean;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  while Integer(ContinueLoop) <> 0 do
  begin
    canadd := true;
    fn := string(FProcessEntry32.szExeFile);
    if fullname = true then
      fn := GetFileNameByPID(FProcessEntry32.th32ProcessID);
    if (FileMask <> emptystr) and (MatchWildcard(ExtractFilename(fn),FileMask, true) = false) then
     canadd := false;
    if canadd = true then
      ProcList.Add(fn + cProcSep + IntToStr(FProcessEntry32.th32ProcessID));
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

// Note: full name may require admin privileges to work for certain processes
function MatchProcessFilename(pe:TProcessEntry32; const ExeFileName: string; FullName:boolean=false):boolean;
var
  fn, targetfn:string;
begin
  result := false;
    if FullName = true then begin
      fn := GetFileNameByPID(pe.th32ProcessID);
      targetfn := exefilename;
    end else begin
      fn := pe.szExeFile;
      targetfn := extractfilename(exefilename);
    end;
    if Uppercase(fn) = Uppercase(targetfn) = true then
    result := true;
end;

// Kills all child tasks from current process ID
function KillChildTasks: boolean;
var
  h: THandle;
  pe: TProcessEntry32;
  curpid:  {$IFDEF UNICODE}Cardinal{$ELSE}DWORD{$ENDIF};
begin
  result := false;
  curpid := GetCurrentProcessId;
  h := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  pe.dwSize := SizeOf(pe);
  if Process32First(h, pe) then
  begin

    while Process32Next(h, pe) do
    begin
      if pe.th32ParentProcessID = curpid then begin
        //if lowercase(exename) = lowercase(string(pe.szExeFile)) then
          KillTaskbyPID(pe.th32ProcessID);
          result := true;
      end;
    end;
  end;
end;

// Kills a process by its process ID
procedure KillTaskbyPID(const PID: Cardinal);
var
  h: THandle;
  lpExitCode: {$IFDEF UNICODE}Cardinal{$ELSE}DWORD{$ENDIF};
begin
  h := OpenProcess(PROCESS_TERMINATE or PROCESS_QUERY_INFORMATION, false, PID);
  if h = 0 then
    exit;
  if GetExitCodeProcess(h, lpExitCode) then
    TerminateProcess(h, lpExitCode)
  else
    CloseHandle(h);
end;

// Runs a comand and optionally waits for the execution to end
function RunTask(const ExeFileName: string; const Wait: boolean = false;
  const WindowState: Integer = SW_SHOW): Cardinal;
var
  Prog: array [0 .. 512] of char;
  CurDir: array [0 .. 255] of char;
  WorkDir: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  ExitCode: Cardinal;
begin
  StrPCopy(Prog, ExeFileName);
  GetDir(0, WorkDir);
  StrPCopy(CurDir, WorkDir);
  FillChar(StartupInfo, sizeof(StartupInfo), #0);
  StartupInfo.cb := sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := WindowState;
  if CreateProcess(nil, Prog, nil, nil, false, CREATE_NEW_CONSOLE or
    NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo) then
  begin
    Result := ProcessInfo.dwProcessId;
    if Wait = true then
    begin
      repeat
        application.ProcessMessages;
        GetExitCodeProcess(ProcessInfo.hProcess, ExitCode);
        WaitForSingleObject(ProcessInfo.hProcess, 10);
      until (ExitCode <> STILL_ACTIVE) or application.Terminated;
    end;
  end
  else
    Result := $FFFFFFFF; // -1
end;

// Runs a comand and waits for a specific number of minutes.
// If MaxMinutes is exceeded, the function stops waiting while the process
// continues running in the background
// Returns -1 if failed, returns -2 if timeout was reached, otherwise returns
// the process ID
function RunTaskWaitMax(const ExeFileName: string; const MaxMinutes: Integer;
  const WindowState: Integer = SW_SHOW): Cardinal;
var
  Prog: array [0 .. 512] of char;
  CurDir: array [0 .. 255] of char;
  WorkDir: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  ExitCode: Cardinal;
  StartTime, ElapsedTime: Cardinal;
begin
  StrPCopy(Prog, ExeFileName);
  GetDir(0, WorkDir);
  StrPCopy(CurDir, WorkDir);
  FillChar(StartupInfo, sizeof(StartupInfo), #0);
  StartupInfo.cb := sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := WindowState;

  if CreateProcess(nil, Prog, nil, nil, false, CREATE_NEW_CONSOLE or
    NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo) then
  begin
    Result := ProcessInfo.dwProcessId;

    StartTime := GetTickCount; // Get the starting time
    repeat
      application.ProcessMessages;
      GetExitCodeProcess(ProcessInfo.hProcess, ExitCode);
      WaitForSingleObject(ProcessInfo.hProcess, 10);
      ElapsedTime := (GetTickCount - StartTime) div 60000; // Convert to minutes
    until (ExitCode <> STILL_ACTIVE) or (ElapsedTime >= MaxMinutes) or application.Terminated;

    // If the time limit is reached but the process is still running, return -2
    if (ExitCode = STILL_ACTIVE) and (ElapsedTime >= MaxMinutes) then
      Result := $FFFFFFFE; // -2 means time limit reached but process still running
  end
  else
    Result := $FFFFFFFF; // -1 means process failed to start
end;



// Returns the number of running tasks
function TaskRunningCount(const ExeFileName: WideString;const FullName:boolean): integer;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  Result := 0;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  while Integer(ContinueLoop) <> 0 do
  begin
    if MatchProcessFilename(FProcessEntry32, ExeFilename, FullName) then
      Inc(Result);
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

// Returns true if a task is running, false otherwise
function TaskRunning(const ExeFileName: WideString;const FullName:boolean): boolean;
begin
  Result := TaskRunningCount(ExeFileName, FullName) <> 0;
end;

// Returns true if a task is running with a specific PID, false otherwise
function TaskRunningWithPID(const ExeFileName: WideString;const PID: Cardinal;const FullName:boolean): boolean;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  Result := false;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  while Integer(ContinueLoop) <> 0 do
  begin
    if (MatchProcessFilename(FProcessEntry32, ExeFilename, FullName) = true)
     and (FProcessEntry32.th32ProcessID = PID) then
      result := true;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

// Returns true if a single instance of a task is running, false otherwise
function TaskRunningSingleInstance(const ExeFileName: WideString;const FullName:boolean): boolean;
begin
  Result := not (TaskRunningCount(ExeFileName, FullName) >= 2);
end;

// Kills a process by its executable filename
// Note: full name may require admin privileges to work for certain processes
function KillTask(const ExeFileName: string; FullName:boolean=false): Integer;
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  Result := 0;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

  while Integer(ContinueLoop) <> 0 do
  begin
    if MatchProcessFilename(FProcessEntry32, exefilename, fullname) = true then
    Result := Integer(TerminateProcess(OpenProcess(PROCESS_TERMINATE, BOOL(0),
        FProcessEntry32.th32ProcessID), 0));
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

// Kills a process by its executable filename, with exception of itself
// Checks the process ID to make sure it is not terminating itself
function KillOtherTasks(const ExeFileName: string; FullName: boolean = false): Integer;
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
  CurrentProcessID: DWORD;
  ProcessHandle: THandle;
begin
  Result := 0;
  CurrentProcessID := GetCurrentProcessId(); // Get the ID of the current process

  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if FSnapshotHandle = INVALID_HANDLE_VALUE then
    Exit;

  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

  while ContinueLoop do
  begin
    // Check if the process has the same executable name but is NOT the current process
    if (FProcessEntry32.th32ProcessID <> CurrentProcessID) and
       MatchProcessFilename(FProcessEntry32, ExeFileName, FullName) then
    begin
      ProcessHandle := OpenProcess(PROCESS_TERMINATE, False, FProcessEntry32.th32ProcessID);
      if ProcessHandle <> 0 then
      begin
        Result := Integer(TerminateProcess(ProcessHandle, 0));
        CloseHandle(ProcessHandle);
      end;
    end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;


// Kills a task or multiple tasks by a wildcard filename, such as notep*.exe
procedure KillTaskByMask(FileMask:string);
var sl:TStringList;
begin
  if filemask = emptystr then
    Exit;
  sl := TStringList.Create;
  GetTaskList(sl, FileMask);
  KillTaskList(sl);
  sl.Free;
end;

// Kills a list of processes by their process IDs
// Expects a list of PIDs in the following format:
// process.exe|pid=111
procedure KillTaskList(ProcList: TStringList);
var
  i, c, pid: Integer;
  //fn: string;
begin
  c := ProcList.Count;
  for i := 0 to c do
  begin
    If i < c then
    begin
      //fn := before(ProcList.strings[i], cProcSep);
      pid := strtoint(after(ProcList.strings[i], cProcSep));
      KillTaskbyPID(pid);
    end;
  end;
end;

// Suspends a process
procedure SuspendProcess(const ProcessID: DWORD);
var
  ThreadsSnapshot, ThreadHandle: THandle;
  ThreadRecord: TThreadEntry32;
begin
  ThreadsSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  ThreadRecord.dwSize := sizeof(ThreadRecord);
  if Thread32First(ThreadsSnapshot, ThreadRecord) then
  begin
    repeat
      if ThreadRecord.th32OwnerProcessID = ProcessID then
      begin
        ThreadHandle := OpenThread(THREAD_SUSPEND_RESUME, false,
          ThreadRecord.th32ThreadID);
        if ThreadHandle = 0 then
          exit;
        SuspendThread(ThreadHandle);
        CloseHandle(ThreadHandle);
      end;
    until not Thread32Next(ThreadsSnapshot, ThreadRecord);
  end;
  CloseHandle(ThreadsSnapshot);
end;

// Resumes a process
procedure ResumeProcess(const ProcessID: DWORD);
var
  ThreadsSnapshot: THandle;
  ThreadRecord: TThreadEntry32;
  ThreadHandle: THandle;
begin
  ThreadsSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  ThreadRecord.dwSize := sizeof(ThreadRecord);
  if Thread32First(ThreadsSnapshot, ThreadRecord) then
  begin
    repeat
      if ThreadRecord.th32OwnerProcessID = ProcessID then
      begin
        ThreadHandle := OpenThread(THREAD_SUSPEND_RESUME, false,
          ThreadRecord.th32ThreadID);
        if ThreadHandle = 0 then
          exit;
        ResumeThread(ThreadHandle);
        CloseHandle(ThreadHandle);
      end;
    until not Thread32Next(ThreadsSnapshot, ThreadRecord);
  end;
  CloseHandle(ThreadsSnapshot);
end;

function GetProcessWorkingSetSize:int64; // bytes
var
  pmc: PPROCESS_MEMORY_COUNTERS;
  cb: Int64;
begin
  cb := SizeOf(_PROCESS_MEMORY_COUNTERS);
  GetMem(pmc, cb);
  pmc^.cb := cb;
  if GetProcessMemoryInfo(GetCurrentProcess(), pmc, cb) then
    result := pmc^.WorkingSetSize
    //Unable to get process info
  else
    result := -1;
  FreeMem(pmc);
end;

procedure RunProcessRestricted(const CommandLine:string);
var
  hToken, hRestrictedToken: THandle;
  si: TStartupInfo;
  pi: TProcessInformation;
begin
  // Open the current process token
  OpenProcessToken(GetCurrentProcess, TOKEN_DUPLICATE or TOKEN_ADJUST_DEFAULT or TOKEN_QUERY or TOKEN_ASSIGN_PRIMARY, hToken);

  // Create a restricted version of the token
  CreateRestrictedToken(hToken, DISABLE_MAX_PRIVILEGE, 0, nil, 0, nil, 0, nil, hRestrictedToken);

  // Prepare to start the new process
  ZeroMemory(@si, SizeOf(si));
  si.cb := SizeOf(si);
  ZeroMemory(@pi, SizeOf(pi));

  // Create the new process with the restricted token
  CreateProcessAsUser(hRestrictedToken, nil, PChar(CommandLine), nil, nil, False, 0, nil, nil, si, pi);
end;

procedure RunProcessAsUser(const TargetPID:Cardinal;const CommandLine:string);
var
  hProcess: THandle;
  hToken, hDupToken: THandle;
  si: TStartupInfo;
  pi: TProcessInformation;
begin
  // Open the target process, get its token, duplicate the token
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION, False, TargetPID);
  OpenProcessToken(hProcess, TOKEN_DUPLICATE, hToken);
  DuplicateTokenEx(hToken, MAXIMUM_ALLOWED, nil, SecurityImpersonation, TokenPrimary, hDupToken);

  // Prepare to start the new process
  ZeroMemory(@si, SizeOf(si));
  si.cb := SizeOf(si);
  ZeroMemory(@pi, SizeOf(pi));

  // Create the new process
  CreateProcessAsUser(hDupToken, nil, PChar(CommandLine), nil, nil, False, 0, nil, nil, si, pi);

end;

// ------------------------------------------------------------------------//
end.
