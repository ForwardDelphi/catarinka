unit CtkFunctions;

{
  Catarinka Lua Library
  Copyright (c) 2013-2020 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, System.Classes, System.SysUtils, Winapi.ShellAPI,
  System.Win.Registry, Vcl.Clipbrd, Vcl.Dialogs, CatCryptSyno,
{$ELSE}
  Windows, Classes, SysUtils, ShellAPI, Registry, Clipbrd, CatCryptSyno,
{$ENDIF}
  Lua;

function str_beginswith(L: plua_State): integer; cdecl;
function str_endswith(L: plua_State): integer; cdecl;
function str_regexfind(L: plua_State): integer; cdecl;
function str_regexreplace(L: plua_State): integer; cdecl;
function str_regexmatch(L: plua_State): integer; cdecl;
function str_base64enc(L: plua_State): integer; cdecl;
function str_base64dec(L: plua_State): integer; cdecl;
function str_trim(L: plua_State): integer; cdecl;
function str_after(L: plua_State): integer; cdecl;
function str_before(L: plua_State): integer; cdecl;
function str_random(L: plua_State): integer; cdecl;
function str_lastchar(L: plua_State): integer; cdecl;
function str_matchversion(L: plua_State): integer; cdecl;
function str_matchversionex(L: plua_State): integer; cdecl;
function str_compareversion(L: plua_State): integer; cdecl;
function str_maxlen(L: plua_State): integer; cdecl;
function str_extracttagcontent(L: plua_State): integer; cdecl;
function str_gettoken(L: plua_State): integer; cdecl;
function str_replace(L: plua_State): integer; cdecl;
function str_replace_first(L: plua_State): integer; cdecl;
function str_between(L: plua_State): integer; cdecl;
function str_ishex(L: plua_State): integer; cdecl;
function str_isint(L: plua_State): integer; cdecl;
function str_occur(L: plua_State): integer; cdecl;
function str_wildmatch(L: plua_State): integer; cdecl;
function str_wildmatchx(L: plua_State): integer; cdecl;
function str_toalphanum(L: plua_State): integer; cdecl;
function str_beautifyjs(L: plua_State): integer; cdecl;
function str_beautifycss(L: plua_State): integer; cdecl;
function str_stripquotes(L: plua_State): integer; cdecl;
function str_stripblanklines(L: plua_State): integer; cdecl;
function str_swapcase(L: plua_State): integer; cdecl;
function str_titlecase(L: plua_State): integer; cdecl;
function str_increase(L: plua_State): integer; cdecl;
function str_decrease(L: plua_State): integer; cdecl;
function str_randompassword(L: plua_State): integer; cdecl;
function str_randompasswordadmin(L: plua_State): integer; cdecl;

function file_getdirfiles(L: plua_State): integer; cdecl;
function file_getdirs(L: plua_State): integer; cdecl;
function file_extractdir(L: plua_State): integer; cdecl;
function file_extractname(L: plua_State): integer; cdecl;
function file_getext(L: plua_State): integer; cdecl;
function file_getversion(L: plua_State): integer; cdecl;
function file_exists(L: plua_State): integer; cdecl;
function file_canopen(L: plua_State): integer; cdecl;
function file_cleanname(L: plua_State): integer; cdecl;
function file_direxists(L: plua_State): integer; cdecl;
function file_mkdir(L: plua_State): integer; cdecl;
function file_copy(L: plua_State): integer; cdecl;
function file_getsize(L: plua_State): integer; cdecl;
function file_gettostr(L: plua_State): integer; cdecl;
function file_exec(L: plua_State): integer; cdecl;
function file_exechidden(L: plua_State): integer; cdecl;
function file_delete(L: plua_State): integer; cdecl;
function file_deldir(L: plua_State): integer; cdecl;
function file_fileurltofilename(L: plua_State): integer; cdecl;

function url_crack(L: plua_State): integer; cdecl;
function url_getfilename(L: plua_State): integer; cdecl;
function url_getfileext(L: plua_State): integer; cdecl;
function url_encode(L: plua_State): integer; cdecl;
function url_encodefull(L: plua_State): integer; cdecl;
function url_decode(L: plua_State): integer; cdecl;
function url_gethost(L: plua_State): integer; cdecl;
function url_getpath(L: plua_State): integer; cdecl;
function url_combine(L: plua_State): integer; cdecl;
function url_getport(L: plua_State): integer; cdecl;
function url_gettiny(L: plua_State): integer; cdecl;
function url_changepath(L: plua_State): integer; cdecl;
function url_open(L: plua_State): integer; cdecl;

function conv_inttohex(L: plua_State): integer; cdecl;
function conv_hextoint(L: plua_State): integer; cdecl;
function conv_hextostr(L: plua_State): integer; cdecl;
function conv_strtohex(L: plua_State): integer; cdecl;
function conv_strtomd5(L: plua_State): integer; cdecl;
function conv_strtosha1(L: plua_State): integer; cdecl;
function conv_strtosha256(L: plua_State): integer; cdecl;
function conv_strtosha384(L: plua_State): integer; cdecl;
function conv_strtosha512(L: plua_State): integer; cdecl;
function conv_strtocommatext(L: plua_State): integer; cdecl;
function conv_commatexttostr(L: plua_State): integer; cdecl;

function console_printgreen(L: plua_State): integer; cdecl;
function console_printred(L: plua_State): integer; cdecl;
function console_printwhite(L: plua_State): integer; cdecl;
function console_readline(L: plua_State): integer; cdecl;
function console_readpassword(L: plua_State): integer; cdecl;
function console_printdisablecolors(L: plua_State): integer; cdecl;

function html_escape(L: plua_State): integer; cdecl;
function html_unescape(L: plua_State): integer; cdecl;
function html_striptags(L: plua_State): integer; cdecl;

function json_escape(L: plua_State): integer; cdecl;

function http_crackrequest(L: plua_State): integer; cdecl;
function http_postdatatojson(L: plua_State): integer; cdecl;
function http_gethdrfield(L: plua_State): integer; cdecl;

function net_iptoname(L: plua_State): integer; cdecl;
function net_nametoip(L: plua_State): integer; cdecl;

function task_isrunning(L: plua_State): integer; cdecl;
function task_kill(L: plua_State): integer; cdecl;

function hostporttourl(L: plua_State): integer; cdecl;

function int_securerandom(L: plua_State): integer; cdecl;

function utils_delay(L: plua_State): integer; cdecl;
function utils_getarg(L: plua_State): integer; cdecl;
function utils_hasarg(L: plua_State): integer; cdecl;
function utils_hassoftwareinstalled(L: plua_State): integer; cdecl;
function utils_clipboard_gettext(L: plua_State): integer; cdecl;
function utils_clipboard_settext(L: plua_State): integer; cdecl;
function utils_function_notavailable(L: plua_State): integer; cdecl;

implementation

uses
  ExtPascalUtils, synacode, pLua, pLuaTable,
  CtkStrList, CtkStrListParser, CtkHTMLParser, CtkJSON, CtkTarman, CatHTML,
  CatStrings, CatJSON, CatMatch, CatFiles, CatUtils, CatHTTP,
  CatInet, CatTasks, CatCLUtils, CatCSUtils, CatHashes;

function int_securerandom(L: plua_State): integer; cdecl;
begin
 if plua_validateargs(L, result,
   [LUA_TBOOLEAN], [vaOptional1]).OK then begin
     if lua_isnone(L, 1) = true then
     plua_pushintnumber(L, securerandom) else
     plua_pushintnumber(L, securerandom(lua_toboolean(L,1)));
   end;
  result := 1;
end;

function console_readline(L: plua_State): integer; cdecl;
begin
  lua_pushstring(L, cs.readln);
  result := 1;
end;

function console_readpassword(L: plua_State): integer; cdecl;
begin
  lua_pushstring(L, cs.readpassword);
  result := 1;
end;

function console_printdisablecolors(L: plua_State): integer; cdecl;
begin
  if plua_validateargsets(L, result, [[LUA_TBOOLEAN]]).OK then begin
    cs.LuaState := L;
    cs.EnableColors := not lua_toboolean(L, 1);
  end;
end;

function console_printgreen(L: plua_State): integer; cdecl;
begin
  if plua_validateargsets(L, result, [[LUA_TSTRING,LUA_TNIL]]).OK then
    cs.writeln_green(lua_tostring(L, 1));
end;

function console_printred(L: plua_State): integer; cdecl;
begin
if plua_validateargsets(L, result, [[LUA_TSTRING,LUA_TNIL]]).OK then
    cs.writeln_red(lua_tostring(L, 1));
end;

function console_printwhite(L: plua_State): integer; cdecl;
begin
if plua_validateargsets(L, result, [[LUA_TSTRING,LUA_TNIL]]).OK then
   cs.writeln_white(lua_tostring(L, 1));
end;

function str_beginswith(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
   lua_pushboolean(L, beginswith(lua_tostring(L, 1), lua_tostring(L, 2)));
end;

function str_endswith(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
   lua_pushboolean(L, endswith(lua_tostring(L, 1), lua_tostring(L, 2)));
end;

function str_maxlen(L: plua_State): integer; cdecl;
var
  s: string;
  AddEllipsis: boolean;
begin
 if plua_validateargs(L, result,
   [LUA_TSTRING, LUA_TNUMBER, LUA_TBOOLEAN], [vaOptional1]).OK then begin
   AddEllipsis := false;
   if lua_isnone(L, 3) = false then
     AddEllipsis := lua_toboolean(L, 3);
   s := lua_tostring(L, 1);
   s := strmaxlen(s, lua_tointeger(L, 2), AddEllipsis);
   lua_pushstring(L, s);
  end;
end;

function str_regexfind(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
    lua_pushstring(L, RegExpFind(lua_tostring(L, 1), lua_tostring(L, 2)));
end;

function str_regexreplace(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING, LUA_TSTRING]).OK then
  lua_pushstring(L, RegExpReplace(lua_tostring(L, 1), lua_tostring(L, 2),
    lua_tostring(L, 3)));
end;

function str_regexmatch(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then begin
    if RegExpFind(lua_tostring(L, 1), lua_tostring(L, 2)) <> '' then
      lua_pushboolean(L, true)
    else
      lua_pushboolean(L, false);
  end;
end;

function str_base64enc(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, Base64Encode(lua_tostring(L, 1)));
end;

function str_base64dec(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, Base64Decode(lua_tostring(L, 1)));
end;

function str_trim(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, trim(lua_tostring(L, 1)));
end;

function str_after(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
    lua_pushstring(L, after(lua_tostring(L, 1), lua_tostring(L, 2)));
end;

function str_before(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
    lua_pushstring(L, before(lua_tostring(L, 1), lua_tostring(L, 2)));
end;

function str_random(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TNUMBER]).OK then
    lua_pushstring(L, RandomString(lua_tointeger(L, 1)));
end;

function str_lastchar(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
   lua_pushstring(L, lastchar(lua_tostring(L, 1)));
end;

function str_extracttagcontent(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
    lua_pushstring(L, extractfromtag(lua_tostring(L, 1), lua_tostring(L, 2)));
end;

function file_gettostr(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, getfiletostr(lua_tostring(L, 1)));
end;

function str_gettoken(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING, LUA_TNUMBER]).OK then
    lua_pushstring(L, gettoken(lua_tostring(L, 1), lua_tostring(L, 2),
    lua_tointeger(L, 3)));
end;

function str_replace(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING, LUA_TSTRING]).OK then
    lua_pushstring(L, replacestr(lua_tostring(L, 1), lua_tostring(L, 2),
    lua_tostring(L, 3)));
end;

function str_replace_first(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING, LUA_TSTRING]).OK then
    lua_pushstring(L, stringreplace(lua_tostring(L, 1), lua_tostring(L, 2),
    lua_tostring(L, 3), []));
end;

function str_between(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING, LUA_TSTRING]).OK then
    lua_pushstring(L, ExtractFromString(lua_tostring(L, 1), lua_tostring(L, 2),
    lua_tostring(L, 3)));
end;

function utils_delay(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TNUMBER]).OK then
    CatDelay(lua_tointeger(L, 1));
end;

function url_crack(L: plua_State): integer; cdecl;
var
  url: TURLParts;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then begin
   lua_newtable(L);
   url := CrackURL(lua_tostring(L, 1));
   plua_SetFieldValue(L, 'fileext', url.fileext);
   plua_SetFieldValue(L, 'filename', url.filename);
   plua_SetFieldValue(L, 'host', url.host);
   plua_SetFieldValue(L, 'path', url.path);
   plua_SetFieldValue(L, 'port', url.port);
   plua_SetFieldValue(L, 'proto', url.protocol);
  end;
end;

function url_getfilename(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, ExtractUrlFileName(lua_tostring(L, 1)));
end;

function url_getfileext(L: plua_State): integer; cdecl;
var
  ext: string;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TBOOLEAN], [vaOptional1]).OK then begin
    ext := ExtractUrlFileExt(lua_tostring(L, 1));
    if lua_isnone(L, 2) = false then
    begin
      if lua_toboolean(L, 2) = false then
      begin
        if beginswith(ext, '.') then
          ext := after(ext, '.');
      end;
    end;
    lua_pushstring(L, ext);
  end;
end;

function html_escape(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, CatHTML.htmlescape(lua_tostring(L, 1)));
end;

function html_unescape(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, CatHTML.htmlunescape(lua_tostring(L, 1)));
end;

function html_striptags(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, StripHtml(lua_tostring(L, 1)));
end;

function json_escape(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, CatHTML.jsonstringescape(lua_tostring(L, 1)));
end;

function str_stripquotes(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, RemoveQuotes(lua_tostring(L, 1)));
end;

function str_stripblanklines(L: plua_State): integer; cdecl;
var
  sl: tstringlist;
  s: string;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK = false then
    exit;
   sl := tstringlist.create;
   sl.text := lua_tostring(L, 1);
   StripBlankLines(sl);
   s := sl.text;
   sl.Free;
   lua_pushstring(L, s);
end;

function str_swapcase(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, swapcase(lua_tostring(L, 1)));
end;

function str_titlecase(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, titlecase(lua_tostring(L, 1)));
end;

function str_randompassword(L: plua_State): integer; cdecl;
var len:integer;
begin
  if plua_validateargs(L, result, [LUA_TNUMBER], [vaOptional1]).OK then begin
    len := 16;
    // accept 8 as minimal password length
    if (lua_isnone(L, 1) = false) and (lua_tointeger(L, 1) >= 8) then
      len := lua_tointeger(L, 1);
    lua_pushstring(L, randompassword(len));
  end;
end;

function str_randompasswordadmin(L: plua_State): integer; cdecl;
var len:integer;
begin
  if plua_validateargs(L, result, [LUA_TNUMBER], [vaOptional1]).OK then begin
    len := 26;
    // accept 16 as minimal admin password length
    if (lua_isnone(L, 1) = false) and (lua_tointeger(L, 1) >= 16) then
      len := lua_tointeger(L, 1);
    lua_pushstring(L, randompassword(len));
  end;
end;

function url_encode(L: plua_State): integer; cdecl;
begin
 if plua_validateargs(L, result, [LUA_TSTRING, LUA_TBOOLEAN], [vaOptional1]).OK then begin
  if lua_isnone(L, 2) = false then
  lua_pushstring(L, URLEncode(lua_tostring(L, 1), false)) else
  lua_pushstring(L, URLencode(lua_tostring(L, 1), lua_toboolean(L, 2)));
 end;
end;

function url_encodefull(L: plua_State): integer; cdecl;
begin
 if plua_validateargs(L, result, [LUA_TSTRING]).OK then
  lua_pushstring(L, URLencodefull(lua_tostring(L, 1)));
end;

function url_decode(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, URLDecode(lua_tostring(L, 1)));
end;

function url_gethost(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, ExtractUrlHost(lua_tostring(L, 1)));
end;

function url_getpath(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, ExtractUrlPath(lua_tostring(L, 1)));
end;

function url_combine(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
   lua_pushstring(L, getabsoluteurl(lua_tostring(L, 1), lua_tostring(L, 2)));
end;

function url_getport(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    plua_pushintnumber(L, extracturlport(lua_tostring(L, 1)));
end;

function net_iptoname(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, IPAddrToName(lua_tostring(L, 1)));
end;

function net_nametoip(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, NameToIPAddr(lua_tostring(L, 1)));
end;

function conv_inttohex(L: plua_State): integer; cdecl;
begin
 if plua_validateargs(L, result, [LUA_TNUMBER]).OK then
  lua_pushstring(L, IntToHex(lua_tointeger(L, 1), 1));
end;

function conv_hextoint(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
   plua_pushintnumber(L, HexToInt(lua_tostring(L, 1)));
end;

function str_ishex(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
   lua_pushboolean(L, IsHexStr(lua_tostring(L, 1)));
end;

function str_isint(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
   lua_pushboolean(L, IsInteger(lua_tostring(L, 1)));
end;

function str_occur(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
   plua_pushintnumber(L, Occurs(lua_tostring(L, 2), lua_tostring(L, 1)));
end;

function str_matchversionex(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
   lua_pushboolean(L, MatchVersionEx(lua_tostring(L, 1), lua_tostring(L, 2)));
end;

function str_matchversion(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
   lua_pushboolean(L, CompareVersionString(lua_tostring(L, 1), lua_tostring(L, 2)) = 0);
end;

function str_compareversion(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
   plua_pushintnumber(L, CompareVersionString(lua_tostring(L, 1), lua_tostring(L, 2)));
end;

function str_wildmatch(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
   lua_pushboolean(L, MatchWildcard(lua_tostring(L, 1), lua_tostring(L, 2)));
end;

function str_wildmatchx(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
    lua_pushboolean(L, MatchWildcardX(lua_tostring(L, 1), lua_tostring(L, 2)));
end;

function str_toalphanum(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
   lua_pushstring(L, StrToAlphaNum(lua_tostring(L, 1)));
end;

function conv_hextostr(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, HexToStr(lua_tostring(L, 1)));
end;

function conv_strtohex(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, StrToHex(lua_tostring(L, 1)));
end;

function conv_strtomd5(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
  lua_pushstring(L, MD5hash(lua_tostring(L, 1)));
end;

function conv_strtosha256(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
  lua_pushstring(L, SHA256(lua_tostring(L, 1)));
end;

function conv_strtosha384(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
  lua_pushstring(L, SHA384(lua_tostring(L, 1)));
end;

function conv_strtosha512(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
  lua_pushstring(L, SHA512(lua_tostring(L, 1)));
end;

function conv_strtosha1(L: plua_State): integer; cdecl;
var
  s: string;
  function StrToHex(const Value: AnsiString): string;
  var
    n: integer;
  begin
    result := '';
    for n := 1 to Length(Value) do
      result := result + IntToHex(Byte(Value[n]), 2);
  end;

begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then begin
   s := string(SHA1(AnsiString(lua_tostring(L, 1))));
   s := lowercase(StrToHex(AnsiString(s)));
   lua_pushstring(L, s);
  end;
end;

function conv_strtocommatext(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
   lua_pushstring(L, StrToCommaText(lua_tostring(L, 1)));
end;

function conv_commatexttostr(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, CommaTextToStr(lua_tostring(L, 1)));
end;

function file_getversion(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, GetFileVersion(lua_tostring(L, 1)));
end;

function file_getdirfiles(L: plua_State): integer; cdecl;
var
  d: string;
  function getdirfiles(dir: string): string;
  var
    list: tstringlist;
  begin
    list := tstringlist.create;
    getfiles(dir, list);
    result := list.text;
    list.Free;
  end;

begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then begin
   d := getdirfiles(lua_tostring(L, 1));
   lua_pushstring(L, d);
  end;
end;

function file_getdirs(L: plua_State): integer; cdecl;
  function GetDirs(dir: widestring): widestring; StdCall;
  var
    search: TSearchRec;
    ts: tstringlist;
    resultstr: string;
  begin
    ts := tstringlist.create;
    try
      if FindFirst(dir + '*.*', faDirectory, search) = 0 then
      begin
        repeat
          if ((search.Attr and faDirectory) = faDirectory) and
            (search.Name <> '.') and (search.Name <> '..') then
            ts.Add(search.Name);
        until FindNext(search) <> 0;
        FindClose(search);
      end;
      ts.sort;
      resultstr := ts.text;
    finally
      ts.Free;
    end;
    result := resultstr;
  end;

var
  d: string;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then begin
    d := GetDirs(lua_tostring(L, 1));
    lua_pushstring(L, d);
  end;
end;

function file_extractdir(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, extractfilepath(lua_tostring(L, 1)));
end;

function file_extractname(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, extractfilename(lua_tostring(L, 1)));
end;

function file_getext(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, extractfileext(lua_tostring(L, 1)));
end;

function file_getsize(L: plua_State): integer; cdecl;
begin
 if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    plua_pushintnumber(L, GetFileSize(lua_tostring(L, 1)));
end;

function file_exists(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushboolean(L, fileexists(lua_tostring(L, 1)));
end;

function file_direxists(L: plua_State): integer; cdecl;
begin
 if plua_validateargs(L, result, [LUA_TSTRING]).OK then
   lua_pushboolean(L, directoryexists(lua_tostring(L, 1)));
end;

function file_canopen(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushboolean(L, filecanbeopened(lua_tostring(L, 1)));
end;

function file_cleanname(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, cleanfilename(lua_tostring(L, 1)));
end;

function file_mkdir(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushboolean(L, ForceDir(lua_tostring(L, 1)));
end;

function file_copy(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
    FileCopy(lua_tostring(L, 1), lua_tostring(L, 2));
end;

function file_exec(L: plua_State): integer; cdecl;
var
  fn, dir, Params: string;
begin
 if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING, LUA_TSTRING],
   [vaOptional2]).OK = false then exit;
  fn := (lua_tostring(L, 1));
  dir := ExtractFileDir(fn);
  Params := lua_tostring(L, 2);
  if lua_isnone(L, 3) = false then
    dir := lua_tostring(L, 3);
  ShellExecute(0, nil, {$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(fn),
{$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(Params),
{$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(dir), SW_SHOWNORMAL);
end;

function file_exechidden(L: plua_State): integer; cdecl;
var
  fn, dir, Params: string;
begin
 if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING, LUA_TSTRING],
   [vaOptional2]).OK = false then exit;
  fn := (lua_tostring(L, 1));
  dir := ExtractFileDir(fn);
  Params := lua_tostring(L, 2);
  if lua_isnone(L, 3) = false then
    dir := lua_tostring(L, 3);
  ShellExecute(0, nil, {$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(fn),
{$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(Params),
{$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(dir), SW_HIDE);
  result := 1;
end;

function file_delete(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    DeleteFile(lua_tostring(L, 1));
end;

function file_deldir(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    DeleteFolder(lua_tostring(L, 1));
end;

function file_fileurltofilename(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, FileUrlToFilename(lua_tostring(L, 1)));
end;

function hostporttourl(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TNUMBER]).OK then
    lua_pushstring(L, generateurl(lua_tostring(L, 1), lua_tointeger(L, 2)));
end;

function http_crackrequest(L: plua_State): integer; cdecl;
var
  request: THTTPRequestParts;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then begin
   lua_newtable(L);
   request := CrackHTTPRequest(lua_tostring(L, 1));
   plua_SetFieldValue(L, 'method', request.method);
   plua_SetFieldValue(L, 'path', request.path);
   plua_SetFieldValue(L, 'data', request.Data);
  end;
end;

function http_postdatatojson(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, postdatatojson(lua_tostring(L, 1)));
end;

function http_gethdrfield(L: plua_State): integer; cdecl;
var
  s: string;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then begin
   s := getfield(lua_tostring(L, 2), lua_tostring(L, 1));
   s := trim(s);
   lua_pushstring(L, s);
  end;
end;

function str_increase(L: plua_State): integer; cdecl;
var
  s: string;
  step: integer;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TNUMBER], [vaOptional1]).OK then begin
   step := 1;
   if lua_isnone(L, 2) = false then
     step := lua_tointeger(L, 2);
   s := strincrease(lua_tostring(L, 1), step);
   lua_pushstring(L, s);
  end;
end;

function url_gettiny(L: plua_State): integer; cdecl;
var
  s: string;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then begin
    s := lua_tostring(L, 1);
    try
      s := gettinyurl(s);
    except
    end;
    lua_pushstring(L, s);
  end;
end;

function str_decrease(L: plua_State): integer; cdecl;
var
  s: string;
  step: integer;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TNUMBER], [vaOptional1]).OK then begin
   step := 1;
   if lua_isnone(L, 2) = false then
     step := lua_tointeger(L, 2);
   s := strdecrease(lua_tostring(L, 1), step);
   lua_pushstring(L, s);
  end;
end;

function url_changepath(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
    lua_pushstring(L, ChangeUrlPath(lua_tostring(L, 1), lua_tostring(L, 2)));
end;

function url_open(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    ShellExecute(0, 'open', PChar(lua_tostring(L, 1)), nil, nil, SW_SHOWNORMAL);
end;

function task_isrunning(L: plua_State): integer; cdecl;
var
  fullname: boolean;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TBOOLEAN],[vaOptional1]).OK then begin
   fullname := false;
   if lua_isnone(L, 2) = false then
     fullname := lua_toboolean(L, 2);
   lua_pushboolean(L, taskrunning(lua_tostring(L, 1), fullname));
  end;
end;

function str_beautifyjs(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, BeautifyJS(lua_tostring(L, 1)));
end;

function str_beautifycss(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushstring(L, BeautifyCSS(lua_tostring(L, 1)));
end;

function task_kill(L: plua_State): integer; cdecl;
var
  fullname: boolean;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TBOOLEAN],[vaOptional1]).OK then begin
    fullname := false;
    if lua_isnone(L, 2) = false then
      fullname := lua_toboolean(L, 2);
    KillTask(lua_tostring(L, 1), fullname);
  end;
end;

// Usage example: HasSoftwareInstalled('Python')
function HasSoftwareInstalled(s: string): boolean;
var
  reg: TRegistry;
begin
  result := false;
  reg := TRegistry.create(KEY_READ);
  try
    reg.Rootkey := HKEY_CURRENT_USER;
    if reg.OpenKey('Software\' + s, false) then
      result := true;
    reg.Rootkey := HKEY_LOCAL_MACHINE;
    if reg.OpenKey('Software\' + s, false) then
      result := true;
  finally
    reg.Free;
  end;
end;

function utils_hasarg(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushboolean(L, HasCmdParam(lua_tostring(L, 1)));
end;

function utils_getarg(L: plua_State): integer; cdecl;
var
  ltype: integer;
begin
  result := 1;
  ltype := lua_type(L, 1);
  case ltype of
    LUA_TSTRING:
      if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING],[vaOptional1]).OK then
      lua_pushstring(L, trim(GetCmdParamQuoted(lua_tostring(L, 1),
        lua_tostring(L, 2))));
    LUA_TNUMBER:
      if plua_validateargs(L, result, [LUA_TNUMBER, LUA_TSTRING],[vaOptional1]).OK then
      begin
        if paramstr(lua_tointeger(L, 1)) = EmptyStr then
          lua_pushstring(L, lua_tostring(L, 2))
        else
          lua_pushstring(L, paramstr(lua_tointeger(L, 1)));
      end;
    LUA_TNONE:
      lua_pushstring(L, trim(GetCmdLine));
  else
    result := 0;
  end;
end;

function utils_hassoftwareinstalled(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    lua_pushboolean(L, HasSoftwareInstalled(lua_tostring(L, 1)));
end;

function utils_clipboard_settext(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    Clipboard.AsText := lua_tostring(L, 1);
end;

function utils_clipboard_gettext(L: plua_State): integer; cdecl;
begin
  lua_pushstring(L, Clipboard.AsText);
  result := 1;
end;

function utils_function_notavailable(L: plua_State): integer; cdecl;
begin
  lual_error(L, 'function not available in legacy library');
  result := 1;
end;

end.
