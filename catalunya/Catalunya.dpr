library Catalunya;

{
  Catalunya Lua Library
  Copyright (c) 2013-2025 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

uses
  Lua,
  CtkCore in 'units\CtkCore.pas';


{$I CatCompactLib.inc}

{$R *.res}

function luaopen_Catalunya(L: plua_State): integer; cdecl;
begin
  Result := RegisterCatalunya(L);
end;

Exports
  luaopen_Catalunya;

begin

end.
