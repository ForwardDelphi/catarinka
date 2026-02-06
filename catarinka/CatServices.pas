unit CatServices;
{
  Catarinka - Windows services helper functions

  Copyright (c) 2025 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  WinAPI.WinSvc, System.SysUtils;
{$ELSE}
  WinSvc, SysUtils;
{$ENDIF}
function ServiceExists(const ServiceName: string): Boolean;
function CreateWindowsService(const ServiceName, DisplayName, FilePath: string): Boolean;
function StopWindowsService(const ServiceName: string): Boolean;
function RestartWindowsService(const ServiceName: string): Boolean;

implementation

function StartWindowsService(const ServiceName: string): Boolean;
var
  SCMHandle, ServiceHandle: SC_HANDLE;
  Args: PWideChar; // Correct argument format for StartService
  ArgsPtr: PWideChar; // Pointer to the argument list
begin
  Result := False;

  // Open the service control manager
  SCMHandle := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if SCMHandle = 0 then Exit;

  try
    // Open the specified service
    ServiceHandle := OpenService(SCMHandle, PChar(ServiceName), SERVICE_START);
    if ServiceHandle = 0 then Exit;

    try
      // Properly define an empty argument list (nil-terminated)
      Args := nil;
      ArgsPtr := @Args; // Get pointer to the argument

      // Attempt to start the service
      Result := StartService(ServiceHandle, 0, ArgsPtr);
    finally
      CloseServiceHandle(ServiceHandle);
    end;
  finally
    CloseServiceHandle(SCMHandle);
  end;
end;

function RestartWindowsService(const ServiceName: string): Boolean;
begin
  Result := StopWindowsService(ServiceName) and StartWindowsService(ServiceName);
end;


function ServiceExists(const ServiceName: string): Boolean;
var
  SCMHandle, SvcHandle: SC_HANDLE;
begin
  Result := False;
  // Open the Service Control Manager with read access
  SCMHandle := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if SCMHandle = 0 then
    Exit;

  try
    // Try to open the service
    SvcHandle := OpenService(SCMHandle, PChar(ServiceName), SERVICE_QUERY_STATUS);
    if SvcHandle <> 0 then
    begin
      // The service exists
      Result := True;
      CloseServiceHandle(SvcHandle);
    end;
  finally
    // Close the Service Control Manager handle
    CloseServiceHandle(SCMHandle);
  end;
end;

function CreateWindowsService(const ServiceName, DisplayName, FilePath: string): Boolean;
var
  SCMHandle: SC_HANDLE;
  ServiceHandle: SC_HANDLE;
begin
  Result := False;

  // Open the Service Control Manager
  SCMHandle := OpenSCManager(nil, nil, SC_MANAGER_CREATE_SERVICE);
  if SCMHandle = 0 then
  begin
    RaiseLastOSError;
    Exit;
  end;

  try
    // Create the service
    ServiceHandle := CreateService(
      SCMHandle,              // Handle to the SCM database
      PChar(ServiceName),     // Name of the service
      PChar(DisplayName),     // Display name of the service
      SERVICE_ALL_ACCESS,     // Desired access
      SERVICE_WIN32_OWN_PROCESS, // Service type
      SERVICE_AUTO_START,     // Start type
      SERVICE_ERROR_NORMAL,   // Error control type
      PChar(FilePath),        // Path to the service binary
      nil,                    // No load ordering group
      nil,                    // No tag identifier
      nil,                    // No dependencies
      nil,                    // LocalSystem account
      nil                     // No password
    );

    if ServiceHandle = 0 then
    begin
      RaiseLastOSError;
    end
    else
    begin
      Result := True; // Service creation succeeded
      CloseServiceHandle(ServiceHandle);
    end;
  finally
    CloseServiceHandle(SCMHandle);
  end;
end;

function StopWindowsService(const ServiceName: string): Boolean;
var
  SCMHandle: SC_HANDLE;
  ServiceHandle: SC_HANDLE;
  ServiceStatus: TServiceStatus;
begin
  Result := False;

  // Open the Service Control Manager
  SCMHandle := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if SCMHandle = 0 then
  begin
    RaiseLastOSError;
    Exit;
  end;

  try
    // Open the service
    ServiceHandle := OpenService(SCMHandle, PChar(ServiceName), SERVICE_STOP);
    if ServiceHandle = 0 then
    begin
      RaiseLastOSError;
      Exit;
    end;

    try
      // Stop the service
      if not ControlService(ServiceHandle, SERVICE_CONTROL_STOP, ServiceStatus) then
      begin
        RaiseLastOSError;
        Exit;
      end;

      Result := True; // Service stopped successfully
    finally
      CloseServiceHandle(ServiceHandle);
    end;
  finally
    CloseServiceHandle(SCMHandle);
  end;
end;

end.
