unit CtkCore;

{
  Catarinka Core Registration Library
  Copyright (c) 2013-2023 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, System.Classes, System.SysUtils, Winapi.ShellAPI,
  System.TypInfo, CtkTimer,
{$ELSE}
  Windows, Classes, SysUtils, ShellAPI, TypInfo,
{$ENDIF}
  Lua, LuaObject;

function RegisterCatalunya(L: plua_State): integer; cdecl;

implementation

uses
  pLua, pLuaTable, CtkFunctions, CtkStrList, CtkStrListParser, CtkHTMLParser,
  CtkJSON, CtkTarman, CatStrings;

function pushctkmethod(L: plua_State; const name:string;
  func_table:array of luaL_Reg): integer;
begin
  result := plua_pushcfunction_fromarray(L, name, func_table);
  // if name is the special _info, return table with number of functions and classes
  if (result = 0) and (name = '_info') then begin
    result := 1;
    lua_newtable(L);
    plua_SetFieldValue(L, 'methodcount', high(func_table));
    plua_SetFieldValue(L, 'classcount', 0);
  end;
end;

function pushctkmethod_orobj(L: plua_State; const name:string;
  func_table:array of luaL_Reg;
  class_table:array of pluaObject_Reg): integer;
begin
  // pushes Lua object if there is one with the given name
  result := plua_PushObjectFromArray(L, name,class_table);
  // if no object is found, check for function with given name
  if (result = 0) then
    result := plua_pushcfunction_fromarray(L, name, func_table);
  // if name is the special _info, return table with number of functions and classes
  if (result = 0) and (name = '_info') then begin
    result := 1;
    lua_newtable(L);
    plua_SetFieldValue(L, 'methodcount', high(func_table));
    plua_SetFieldValue(L, 'classcount', high(class_table)+1);
  end;
end;

function get_string(L: plua_State): integer; cdecl;
const
   string_table : array [1..27] of luaL_Reg =
   (
   (name:'after';func:str_after),
   (name:'before';func:str_before),
   (name:'beginswith';func:str_beginswith),
   (name:'between';func:str_between),
   (name:'comparever';func:str_compareversion),
   (name:'decrease';func:str_decrease),
   (name:'endswith';func:str_endswith),
   (name:'gettoken';func:str_gettoken),
   (name:'increase';func:str_increase),
   (name:'ishex';func:str_ishex),
   (name:'isint';func:str_isint),
   (name:'lastchar';func:str_lastchar),
   (name:'match';func:str_wildmatch),
   (name:'matchx';func:str_wildmatchx),
   (name:'matchver';func:str_matchversion),
   (name:'matchverpat';func:str_matchversionex),
   (name:'maxlen';func:str_maxlen),
   (name:'occur';func:str_occur),
   (name:'random';func:str_random),
   (name:'replace';func:str_replace),
   (name:'replacefirst';func:str_replace_first),
   (name:'stripblanklines';func:str_stripblanklines),
   (name:'stripquotes';func:str_stripquotes),
   (name:'swapcase';func:str_swapcase),
   (name:'titlecase';func:str_titlecase),
   (name:'trim';func:str_trim),
   (name:nil;func:nil)
   );
const
  class_table: array [1..2] of pluaObject_Reg =
   (
   (name:'list';proc:RegisterCatarinkaStrList),
   (name:'loop';proc:RegisterCatarinkaStrListParser)
   );
begin
  result := pushctkmethod_orobj(L, lua_tostring(L,2), string_table, class_table);
end;

function get_jsonfields(L: plua_State): integer; cdecl;
const
   json_table : array [1..2] of luaL_Reg =
   (
   (name:'escape';func:json_escape),
   (name:nil;func:nil)
   );
const
  class_table: array [1..1] of pluaObject_Reg =
   (
   (name:'object';proc:RegisterCatarinkaJSON)
   );
begin
 result := pushctkmethod_orobj(L, lua_tostring(L,2), json_table, class_table);
end;

function get_htmlfields(L: plua_State): integer; cdecl;
const
   html_table : array [1..7] of luaL_Reg =
   (
   (name:'beautifycss';func:str_beautifycss),
   (name:'beautifyjs';func:str_beautifyjs),
   (name:'escape';func:html_escape),
   (name:'gettagcontents';func:str_extracttagcontent),
   (name:'striptags';func:html_striptags),
   (name:'unescape';func:html_unescape),
   (name:nil;func:nil)
   );
const
  class_table: array [1..1] of pluaObject_Reg =
   (
   (name:'parser';proc:RegisterCatarinkaHTMLParser)
   );
begin
 result := pushctkmethod_orobj(L, lua_tostring(L,2), html_table, class_table);
end;

function get_base64fields(L: plua_State): integer; cdecl;
const
   b64_table : array [1..3] of luaL_Reg =
   (
   (name:'encode';func:str_base64enc),
   (name:'decode';func:str_base64dec),
   (name:nil;func:nil)
   );
begin
 result := pushctkmethod(L, lua_tostring(L,2), b64_table);
end;

function get_convertfields(L: plua_State): integer; cdecl;
const
   conv_table : array [1..8] of luaL_Reg =
   (
   (name:'commatostr';func:conv_commatexttostr),
   (name:'hextoint';func:conv_hextoint),
   (name:'hextostr';func:conv_hextostr),
   (name:'inttohex';func:conv_inttohex),
   (name:'strtoalphanum';func:str_toalphanum),
   (name:'strtohex';func:conv_strtohex),
   (name:'strtocomma';func:conv_strtocommatext),
   (name:nil;func:nil)
   );
begin
 result := pushctkmethod(L, lua_tostring(L,2), conv_table);
end;

function get_cryptofields(L: plua_State): integer; cdecl;
const
   crypto_table : array [1..9] of luaL_Reg =
   (
   (name:'md5';func:conv_strtomd5),
   (name:'random';func:int_securerandom),
   (name:'randompwd';func:str_randompassword),
   (name:'randompwdadmin';func:str_randompasswordadmin),
   (name:'sha1';func:conv_strtosha1),
   (name:'sha256';func:conv_strtosha256),
   (name:'sha384';func:conv_strtosha384),
   (name:'sha512';func:conv_strtosha512),
   (name:nil;func:nil)
   );
begin
  result := pushctkmethod(L, lua_tostring(L,2), crypto_table);
end;

function get_dirfields(L: plua_State): integer; cdecl;
const
   dir_table : array [1..8] of luaL_Reg =
   (
   (name:'create';func:file_mkdir),
   (name:'delete';func:file_deldir),
   (name:'exists';func:file_direxists),
   (name:'getdirlist';func:file_getdirs),
   (name:'getfilelist';func:file_getdirfiles),
   (name:'packtotar';func:Lua_DirToTAR),
   (name:'unpackfromtar';func:Lua_TARToDir),
   (name:nil;func:nil)
   );
begin
 result := pushctkmethod(L, lua_tostring(L,2), dir_table);
end;

function get_filefields(L: plua_State): integer; cdecl;
const
   file_table : array [1..15] of luaL_Reg =
   (
   (name:'canopen';func:file_canopen),
   (name:'cleanname';func:file_cleanname),
   (name:'copy';func:file_copy),
   (name:'delete';func:file_delete),
   (name:'exec';func:file_exec),
   (name:'exechide';func:file_exechidden),
   (name:'exists';func:file_exists),
   (name:'getcontents';func:file_gettostr),
   (name:'getdir';func:file_extractdir),
   (name:'getfromtar';func:file_readfromtar),
   (name:'getname';func:file_extractname),
   (name:'getsize';func:file_getsize),
   (name:'getext';func:file_getext),
   (name:'getver';func:file_getversion),
   (name:nil;func:nil)
   );
begin
  result := pushctkmethod(L, lua_tostring(L,2), file_table);
end;

function get_httpfields(L: plua_State): integer; cdecl;
const
   http_table : array [1..4] of luaL_Reg =
   (
   (name:'crackrequest';func:http_crackrequest),
   (name:'getheader';func:http_gethdrfield),
   (name:'postdatatojson';func:http_postdatatojson),
   (name:nil;func:nil)
   );
begin
 result := pushctkmethod(L, lua_tostring(L,2), http_table);
end;

function get_netfields(L: plua_State): integer; cdecl;
const
   net_table : array [1..3] of luaL_Reg =
   (
   (name:'nametoip';func:net_nametoip),
   (name:'iptoname';func:net_iptoname),
   (name:nil;func:nil)
   );
begin
 result := pushctkmethod(L, lua_tostring(L,2), net_table);
end;

function get_taskfields(L: plua_State): integer; cdecl;
const
   task_table : array [1..3] of luaL_Reg =
   (
   (name:'isrunning';func:task_isrunning),
   (name:'kill';func:task_kill),
   (name:nil;func:nil)
   );
begin
 result := pushctkmethod(L, lua_tostring(L,2), task_table);
end;

function get_urlfields(L: plua_State): integer; cdecl;
const
   url_table : array [1..13] of luaL_Reg =
   (
   (name:'changepath';func:url_changepath),
   (name:'combine';func:url_combine),
   (name:'crack';func:url_crack),
   (name:'decode';func:url_decode),
   (name:'encode';func:url_encode),
   (name:'encodefull';func:url_encodefull),
   (name:'fileurltofilename';func:file_fileurltofilename),
   (name:'genfromhost';func:hostporttourl),
   (name:'getfileext';func:url_getfileext),
   (name:'getfilename';func:url_getfilename),
   (name:'gettiny';func:url_gettiny),
   (name:'open';func:url_open),
   (name:nil;func:nil)
   );
begin
 result := pushctkmethod(L, lua_tostring(L,2), url_table);
end;

function get_utilfields(L: plua_State): integer; cdecl;
const
   utils_table : array [1..8] of luaL_Reg =
   (
   (name:'delay';func:utils_delay),
   (name:'getarg';func:utils_getarg),
   (name:'hasarg';func:utils_hasarg),
   (name:'hassoftware';func:utils_hassoftwareinstalled),
   (name:'clipboard_gettext';func:utils_clipboard_gettext),
   (name:'clipboard_settext';func:utils_clipboard_settext),
   {$IFDEF DXE2_OR_UP}
   (name:'settimeout';func:utils_settimeout),
   {$ELSE}
   (name:'settimeout';func:utils_function_notavailable),
   {$ENDIF}
   (name:nil;func:nil)
   );
begin
 result := pushctkmethod(L, lua_tostring(L,2), utils_table);
end;

function get_regexfields(L: plua_State): integer; cdecl;
const
   re_table : array [1..4] of luaL_Reg =
   (
   (name:'find';func:str_regexfind),
   (name:'match';func:str_regexmatch),
   (name:'replace';func:str_regexreplace),
   (name:nil;func:nil)
   );
begin
 result := pushctkmethod(L, lua_tostring(L,2), re_table);
end;

function get_consolefields(L: plua_State): integer; cdecl;
const
   cs_table : array [1..7] of luaL_Reg =
   (
   (name:'printdisablecl';func:console_printdisablecolors),
   (name:'printgreen';func:console_printgreen),
   (name:'printred';func:console_printred),
   (name:'printwhite';func:console_printwhite),
   (name:'readln';func:console_readline),
   (name:'readpwd';func:console_readpassword),
   (name:nil;func:nil)
   );
begin
 result := pushctkmethod(L, lua_tostring(L,2), cs_table);
end;

function RegisterCatalunya(L: plua_State): integer; cdecl;
begin
  lua_newtable(L);
  plua_SetFieldValueRW(L, 'base64', @get_base64fields, nil);
  plua_SetFieldValueRW(L, 'convert', @get_convertfields, nil);
  plua_SetFieldValueRW(L, 'crypto', @get_cryptofields, nil);
  plua_SetFieldValueRW(L, 'cs', @get_consolefields, nil);
  plua_SetFieldValueRW(L, 'dir', @get_dirfields, nil);
  plua_SetFieldValueRW(L, 'file', @get_filefields, nil);
  plua_SetFieldValueRW(L, 'html', @get_htmlfields, nil);
  plua_SetFieldValueRW(L, 'http', @get_httpfields, nil);
  plua_SetFieldValueRW(L, 'json', @get_jsonfields, nil);
  plua_SetFieldValueRW(L, 'net', @get_netfields, nil);
  plua_SetFieldValueRW(L, 'task', @get_taskfields, nil);
  plua_SetFieldValueRW(L, 'url', @get_urlfields, nil);
  plua_SetFieldValueRW(L, 'utils', @get_utilfields, nil);
  plua_SetFieldValueRW(L, 're', @get_regexfields, nil);
  plua_SetFieldValueRW(L, 'string', @get_string, nil);
  result := 1;
end;

end.
