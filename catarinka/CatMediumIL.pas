unit CatMediumIL;

// Adaptation from C++ to Pascal by Elçin from Baku, Azerbaijan
// The original code was writen by Volirvag Yexela from France in C++ as part of
// a project licensed with Code Project Open License (CPOL) 1.02

interface

uses
  Winapi.Windows, Winapi.ShellAPI;

function CreateProcessIL(lpApplicationName: PWChar;
  lpCommandLine: PWChar;
  lpProcessAttributes: PSecurityAttributes;
  lpThreadAttributes: PSecurityAttributes;
  bInheritHandle: BOOL;
  dwCreationFlags: DWORD;
  lpEnvironment: LPVOID;
  lpCurrentDirectory: PWChar;
  const lpStartupInfo: TStartupInfoW;
  var lpProcessInformation: TProcessInformation): DWORD;

function ShellExecuteIL(hWnd: HWND; Operation, FileName, Parameters, Directory: PWideChar; ShowCmd: Integer): UInt64;


implementation

uses CatTasks;

type
  TOKEN_MANDATORY_LABEL = record
    Label_: SID_AND_ATTRIBUTES;
  end;

  PTOKEN_MANDATORY_LABEL = ^TOKEN_MANDATORY_LABEL;

  TTokenMandatoryLabel = TOKEN_MANDATORY_LABEL;
  PTokenMandatoryLabel = ^TTokenMandatoryLabel;

  TCreateProcessWithTokenW = function (hToken: THandle; dwLogonFlags: DWORD; lpApplicationName: LPCWSTR; lpCommandLine: LPWSTR; dwCreationFlags: DWORD; lpEnvironment: LPVOID; lpCurrentDirectory: LPCWSTR; const lpStartupInfo: TStartupInfoW; out lpProcessInfo: TProcessInformation): BOOL; stdcall;

const
  SECURITY_MANDATORY_UNTRUSTED_RID = $00000000;
  SECURITY_MANDATORY_LOW_RID = $00001000;
  SECURITY_MANDATORY_MEDIUM_RID = $00002000;
  SECURITY_MANDATORY_HIGH_RID = $00003000;
  SECURITY_MANDATORY_SYSTEM_RID = $00004000;
  SECURITY_MANDATORY_PROTECTED_PROCESS_RID = $00005000;


function GetShellWindow: HWND; stdcall; external 'user32.dll' name 'GetShellWindow';

// writes Integration Level of the process with the given ID into dwProcessIL
// returns Win32 API error or 0 if succeeded
function GetProcessIL(dwProcessID: DWORD; var dwProcessIL: DWORD): DWORD;
label
  _CleanUp;
var
  hProcess: THandle;
  hToken: THandle;
  dwSize: DWORD;
  pbCount: PByte;
  pdwProcIL: PDWORD;
  pTIL: PTokenMandatoryLabel;
  dwError: DWORD;
begin
  dwProcessIL := 0;

  pTIL := nil;

  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION, False, dwProcessID);
  if (hProcess = 0) then
    goto _CleanUp;

  if (not OpenProcessToken(hProcess, TOKEN_QUERY, hToken)) then
    goto _CleanUp;

  if (not GetTokenInformation(hToken, TokenIntegrityLevel, nil, 0, dwSize) and (GetLastError() <> ERROR_INSUFFICIENT_BUFFER)) then
    goto _CleanUp;

  pTIL := HeapAlloc(GetProcessHeap(), 0, dwSize);
  if (pTIL = nil) then
    goto _CleanUp;

  if (not GetTokenInformation(hToken, TokenIntegrityLevel, pTIL, dwSize, dwSize)) then
    goto _CleanUp;

  pbCount := PByte(GetSidSubAuthorityCount(pTIL^.Label_.Sid));
  if (pbCount = nil) then
    goto _CleanUp;

  pdwProcIL := GetSidSubAuthority(pTIL^.Label_.Sid, pbCount^ - 1);
  if (pdwProcIL = nil) then
    goto _CleanUp;

  dwProcessIL := pdwProcIL^;
  SetLastError(ERROR_SUCCESS);

  _CleanUp:
  dwError := GetLastError();
  if (pTIL <> nil) then
    HeapFree(GetProcessHeap(), 0, pTIL);
  if (hToken <> 0) then
    CloseHandle(hToken);
  if (hProcess <> 0) then
    CloseHandle(hProcess);
  Result := dwError;
end;

// Creates a new process lpApplicationName with the integration level of the Explorer process (MEDIUM IL)
// If you need this function in a service you must replace FindWindow() with another API to find Explorer process
// The parent process of the new process will be svchost.exe if this EXE was run "As Administrator"
// returns Win32 API error or 0 if succeeded
function CreateProcessIL(lpApplicationName: PWChar; lpCommandLine: PWChar; lpProcessAttributes: PSecurityAttributes; lpThreadAttributes: PSecurityAttributes; bInheritHandle: BOOL; dwCreationFlags: DWORD; lpEnvironment: LPVOID; lpCurrentDirectory: PWChar; const lpStartupInfo: TStartupInfoW; var lpProcessInformation: TProcessInformation): DWORD;
label
  _CleanUp;
var
  hProcess: THandle;
  hToken: THandle;
  hToken2: THandle;
  bUseToken: BOOL;
  dwCurIL: DWORD;
  dwErr: DWORD;
  f_CreateProcessWithTokenW: TCreateProcessWithTokenW;
  hProgman: HWND;
  dwExplorerPID: DWORD;
  dwError: DWORD;
begin
  bUseToken := False;

  // Detect Windows Vista, 2008, Windows 7 and higher
  if (GetProcAddress(GetModuleHandleA('Kernel32'), 'GetProductInfo') <> nil) then
  begin
    dwErr := GetProcessIL(GetCurrentProcessId(), dwCurIL);
    if (dwErr <> 0) then
    begin
      Result := dwErr;
      Exit;
    end;
      if (dwCurIL > SECURITY_MANDATORY_MEDIUM_RID) then
        bUseToken := True;
  end;

  // Create the process normally (before Windows Vista or if current process runs with a medium IL)
  if (not bUseToken) then
  begin
    if (not CreateProcessW(lpApplicationName, lpCommandLine, lpProcessAttributes, lpThreadAttributes, bInheritHandle, dwCreationFlags, lpEnvironment, lpCurrentDirectory, lpStartupInfo, lpProcessInformation)) then
    begin
      Result := GetLastError();
      Exit;
    end;

    CloseHandle(lpProcessInformation.hThread);
    CloseHandle(lpProcessInformation.hProcess);
    Result := ERROR_SUCCESS;
    Exit;
  end;

  f_CreateProcessWithTokenW := GetProcAddress(GetModuleHandleA('Advapi32'), 'CreateProcessWithTokenW');

  if (not Assigned(f_CreateProcessWithTokenW)) then // This will never happen on Vista!
  begin
    Result := ERROR_INVALID_FUNCTION;
    Exit;
  end;

  hProgman := GetShellWindow();

  dwExplorerPID := 0;
  GetWindowThreadProcessId(hProgman, dwExplorerPID);

  // ATTENTION:
  // If UAC is turned OFF all processes run with SECURITY_MANDATORY_HIGH_RID, also Explorer!
  // But this does not matter because to start the new process without UAC no elevation is required.
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION, False, dwExplorerPID);
  if (hProcess = 0) then
    goto _CleanUp;

  if (not OpenProcessToken(hProcess, TOKEN_DUPLICATE, hToken)) then
    goto _CleanUp;

  if (not DuplicateTokenEx(hToken, TOKEN_ALL_ACCESS or TOKEN_ADJUST_SESSIONID, nil, SecurityImpersonation, TokenPrimary, hToken2)) then
    goto _CleanUp;

  if (not f_CreateProcessWithTokenW(hToken2, 0, lpApplicationName, lpCommandLine, dwCreationFlags, lpEnvironment, lpCurrentDirectory, lpStartupInfo, lpProcessInformation)) then
    goto _CleanUp;

  SetLastError(ERROR_SUCCESS);

  _CleanUp:
  dwError := GetLastError();
  if (hToken <> 0) then
    CloseHandle(hToken);
  if (hToken2 <> 0) then
    CloseHandle(hToken2);
  if (hProcess <> 0) then
    CloseHandle(hProcess);
  CloseHandle(lpProcessInformation.hThread);
  CloseHandle(lpProcessInformation.hProcess);
  Result := dwError;
end;

// A quick ShellExecute replacement that calls process in non-elevated mode
function ShellExecuteIL(hWnd: HWND; Operation, FileName, Parameters, Directory: PWideChar; ShowCmd: Integer): UInt64;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  if IsUserAnAdmin = false then
    result := ShellExecute(hWnd, Operation, Filename, Parameters, Directory, ShowCmd)
  else begin
  // todo: pass hWnd and Operation, usually 0 and nil
    ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
    ZeroMemory(@ProcessInfo, SizeOf(ProcessInfo));
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := ShowCmd;
    result := CreateProcessIL(filename, pwidechar('"'+filename+'" '+parameters), nil, nil, False, 0, nil, Directory, StartupInfo, ProcessInfo);
  end;
end;

end.