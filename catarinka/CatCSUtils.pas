unit CatCSUtils;

{
  Console Utils
  Useful routines for console applications
  Copyright (c) 2013 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  Portions extracted from CRT32 written by Attila Szomor, based on
  Frank Zimmer's freeware CRT32 library
}

interface

{$I Catarinka.inc}

{$IFDEF LUA51}
  {$DEFINE INCLUDELUA}
{$ENDIF}
{$IFDEF LUAJIT}
  {$DEFINE INCLUDELUA}
{$ENDIF}

uses
{$IFDEF INCLUDELUA}
 plua, Lua,
{$ENDIF}
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, System.Classes, System.SysUtils;
{$ELSE}
  Windows, Classes, SysUtils;
{$ENDIF}




type
  TCatCSHelper = class
  private
    fEnableColors:boolean;
    fLastLnLength: integer;
    fScreenHandle: THandle;
    {$IFDEF INCLUDELUA}
    fLuaState: plua_State;
    {$ENDIF}
    procedure WriteLn_LuaPrint(s: string);
  public
    function ReadLn:string;
    function ReadPassword(const InputMask: Char = '*'): string;
    procedure ResetColor;
    procedure UpdateLn(s: string);
    procedure WriteDot;
    procedure WriteLn_Cyan(s: string);
    procedure WriteLn_Green(s: string);
    procedure WriteLn_Red(s: string);
    procedure WriteLn_Magenta(s: string);
    procedure WriteLn_White(s: string);
    constructor Create;
    destructor Destroy; override;
  // properties
    property EnableColors: boolean read fEnableColors write fEnableColors;
    {$IFDEF INCLUDELUA}
    property LuaState: plua_State read fLuaState write fLuaState;
    {$ENDIF}
  end;

procedure TextColor(Color: Byte);
procedure WriteLnC(s: string; Color: Byte);
procedure WriteC(s: string; Color: Byte);
function IsConsoleApp: boolean;

var
  CS: TCatCSHelper;

implementation

uses
  CatStrings;

{ const
  //Foreground and background color constants of original CRT unit
  bgBlack = 0;
  bgBlue = 1;
  bgGreen = 2;
  bgCyan = 3;
  bgRed = 4;
  bgMagenta = 5;
  bgBrown  = 6;
  bgLightGray = 7;
  //Foreground color constants of original CRT unit
  fgDarkGray = 8;
  fgLightBlue = 9;
  fgLightGreen = 10;
  fgLightCyan = 11;
  fgLightRed = 12;
  fgLightMagenta = 13;
  fgYellow = 14;
  fgWhite = 15;
  //Add-in for blinking of original CRT unit
  Blink = 128; }

const
  csclBLACK = 0;
  csclBLUE = FOREGROUND_BLUE;
  csclGREEN = FOREGROUND_GREEN;
  csclCYAN = FOREGROUND_GREEN + FOREGROUND_BLUE;
  csclRED = FOREGROUND_RED;
  csclMAGENTA = FOREGROUND_RED + FOREGROUND_BLUE;
  csclBROWN = FOREGROUND_RED + FOREGROUND_GREEN;
  csclLIGHTGRAY = FOREGROUND_RED + FOREGROUND_GREEN + FOREGROUND_BLUE;
  csclDARKGRAY = FOREGROUND_INTENSITY;
  csclLIGHTBLUE = FOREGROUND_BLUE + FOREGROUND_INTENSITY;
  csclLIGHTGREEN = FOREGROUND_GREEN + FOREGROUND_INTENSITY;
  csclLIGHTCYAN = FOREGROUND_GREEN + FOREGROUND_BLUE + FOREGROUND_INTENSITY;
  csclLIGHTRED = FOREGROUND_RED + FOREGROUND_INTENSITY;
  csclLIGHTMAGENTA = FOREGROUND_RED + FOREGROUND_BLUE + FOREGROUND_INTENSITY;
  csclYELLOW = FOREGROUND_RED + FOREGROUND_GREEN + FOREGROUND_INTENSITY;
  csclWHITE = FOREGROUND_RED + FOREGROUND_GREEN + FOREGROUND_BLUE +
    FOREGROUND_INTENSITY;

function IsConsoleApp: boolean;
var
  SI: TStartupInfo;
begin
  SI.cb := SizeOf(TStartupInfo);
  GetStartupInfo(SI);
  Result := ((SI.dwFlags and STARTF_USESHOWWINDOW) = 0);
end;

procedure WriteC(s: string; Color: Byte);
begin
  TextColor(Color);
  write(s);
  TextColor(csclLIGHTGRAY); // default console color
end;

procedure WriteLnC(s: string; Color: Byte);
begin
  TextColor(Color);
  writeln(s);
  TextColor(csclLIGHTGRAY); // default console color
end;

// Thanks Francesca Gaillard
function Console_ReadPassword(const InputMask: Char = '*'): string;
var
  OldMode: Cardinal;
  c: Char;
begin
  GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), OldMode);
  SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), OldMode and
    not(ENABLE_LINE_INPUT or ENABLE_ECHO_INPUT));
  try
    while not Eof do
    begin
      Read(c);
      if c = #13 then // Carriage Return
        Break;
      if (c = #8) and (Length(Result) > 0) then // Back Space
      begin
        Delete(Result, Length(Result), 1);
        Write(#8);
      end
      else
      begin
        Result := Result + c;
        Write(InputMask);
      end;
    end;
  finally
    WriteLn;
    SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), OldMode);
  end;
end;

procedure TextColor(Color: Byte);
begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), Color);
end;

procedure TCatCSHelper.UpdateLn(s: string);
var
  CSBufInf: TConsoleScreenBufferInfo;
  garbageIntegerVar: Cardinal;
  blankstr: string;
begin
  if fScreenHandle = 0 then
    fScreenHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(fScreenHandle, CSBufInf);
  if Length(s) > fLastLnLength then
    fLastLnLength := Length(s);
  blankstr := StringofChar(' ', fLastLnLength);
  WriteConsoleOutputCharacter(fScreenHandle,
{$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(s + blankstr),
    Length(s + blankstr), CSBufInf.dwCursorPosition, garbageIntegerVar);
end;

procedure TCatCSHelper.ResetColor;
begin
  TextColor(csclLIGHTGRAY); // default console color
end;

procedure TCatCSHelper.WriteLn_LuaPrint(s: string);
begin
{$IFDEF INCLUDELUA}
  lua_getglobal(fLuaState, 'print');
  lua_pushstring(fLuaState, s);
  lua_call(fLuaState, 1, 0);
{$ENDIF}
end;

procedure TCatCSHelper.WriteLn_Cyan(s: string);
begin
  if fEnableColors = true then
  WriteLnC(s, csclLIGHTCYAN) else
  WriteLn_LuaPrint(s);
end;

procedure TCatCSHelper.WriteLn_Magenta(s: string);
begin
  if fEnableColors = true then
  WriteLnC(s, csclMAGENTA) else
  WriteLn_LuaPrint(s);
end;

procedure TCatCSHelper.WriteLn_Green(s: string);
begin
  if fEnableColors = true then
  WriteLnC(s, csclLIGHTGREEN) else
  WriteLn_LuaPrint(s);
end;

procedure TCatCSHelper.WriteLn_Red(s: string);
begin
  if fEnableColors = true then
  WriteLnC(s, csclLIGHTRED) else
  WriteLn_LuaPrint(s);
end;

procedure TCatCSHelper.WriteLn_White(s: string);
begin
  if fEnableColors = true then
  WriteLnC(s, csclWHITE) else
  WriteLn_LuaPrint(s);
end;

function TCatCSHelper.ReadLn:string;
begin
  System.Readln(result);
end;

function TCatCSHelper.ReadPassword(const InputMask: Char = '*'): string;
begin
  result := Console_ReadPassword(inputmask);
end;

procedure TCatCSHelper.WriteDot;
begin
  if fEnableColors = true then
  System.Write('.') else
  WriteLn_LuaPrint('.');
end;

constructor TCatCSHelper.Create;
begin
  fEnableColors := true;
end;

destructor TCatCSHelper.Destroy;
begin
  inherited;
end;

initialization

CS := TCatCSHelper.Create;

finalization

CS.Free;

end.
