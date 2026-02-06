unit CtkJSON;

{
  Catarinka Lua Library - JSON Object
  Copyright (c) 2013-2015 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

uses
  Classes, SysUtils, Lua, LuaObject, SuperObject, CatStrings, Variants,
  CatJSON;

type
  TCatarinkaJSON = class(TLuaObject)
  private
  public
    obj: TCatJSON;
    function GetPropValue(propName: String): Variant; override;
    function SetPropValue(propName: String; const AValue: Variant)
      : Boolean; override;
    constructor Create(LuaState: PLua_State; AParent: TLuaObject = nil);
      overload; override;
    destructor Destroy; override;
  end;

function RegisterCatarinkaJSON(L: PLua_State):TLuaObjectRegResult;

implementation

uses pLua;

function method_settext(L: PLua_State): Integer; cdecl;
var
  o: TCatarinkaJSON;
  json:string;
begin
  o := TCatarinkaJSON(LuaToTLuaObject(L, 1));
  if plua_validatemethodargs(L, result, [LUA_TSTRING]).OK then begin
   json := lua_tostring(L, 2);
   o.obj.text := json;
  end;
end;

function method_loadfromfile(L: PLua_State): Integer; cdecl;
var
  o: TCatarinkaJSON;
begin
  o := TCatarinkaJSON(LuaToTLuaObject(L, 1));
  if plua_validatemethodargs(L, result, [LUA_TSTRING]).OK then
   o.obj.LoadFromFile(lua_tostring(L, 2));
end;

function method_savetofile(L: PLua_State): Integer; cdecl;
var
  o: TCatarinkaJSON;
begin
  o := TCatarinkaJSON(LuaToTLuaObject(L, 1));
  if plua_validatemethodargs(L, result, [LUA_TSTRING]).OK then
   o.obj.SaveToFile(lua_tostring(L, 2));
end;

function method_gettext(L: PLua_State): Integer; cdecl;
var
  o: TCatarinkaJSON;
begin
  o := TCatarinkaJSON(LuaToTLuaObject(L, 1));
  lua_pushstring(L, o.obj.text);
  result := 1;
end;

function method_getvalue(L: PLua_State): Integer; cdecl;
var
  o: TCatarinkaJSON;
begin
  o := TCatarinkaJSON(LuaToTLuaObject(L, 1));
  plua_pushvariant(L, o.obj.GetValue(lua_tostring(L, 2),plua_tovariant(L, 3)));
  result := 1;
end;

function method_gettext_withunquotedkeys(L: PLua_State): Integer; cdecl;
var
  o: TCatarinkaJSON;
begin
  o := TCatarinkaJSON(LuaToTLuaObject(L, 1));
  lua_pushstring(L, o.obj.TextUnquoted);
  result := 1;
end;

procedure register_methods(L: PLua_State; classTable: Integer);
begin
  RegisterMethod(L, '__tostring', @method_gettext, classTable);
  RegisterMethod(L, 'getvalue', @method_getvalue, classTable);
  RegisterMethod(L, 'getjson', @method_gettext, classTable);
  RegisterMethod(L, 'getjson_unquoted', @method_gettext_withunquotedkeys,
    classTable);
  RegisterMethod(L, 'load', @method_settext, classTable);
  RegisterMethod(L, 'loadfromfile', @method_loadfromfile, classTable);
  RegisterMethod(L, 'savetofile', @method_savetofile, classTable);
end;

const
  cObjectName = 'ctk_json';

function new_callback(L: PLua_State; AParent: TLuaObject = nil): TLuaObject;
begin
  result := TCatarinkaJSON.Create(L, AParent);
end;

function Create(L: PLua_State): Integer; cdecl;
var
  p: TLuaObjectNewCallback;
begin
  p := @new_callback;
  result := new_LuaObject(L, cObjectName, p);
end;

function RegisterCatarinkaJSON(L: PLua_State):TLuaObjectRegResult;
begin
  result := RegisterTLuaObjectAlt(L, cObjectName, @Create, @register_methods);
end;

constructor TCatarinkaJSON.Create(LuaState: PLua_State; AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
  obj := TCatJSON.Create;
end;

destructor TCatarinkaJSON.Destroy;
begin
  obj.Free;
  inherited Destroy;
end;

function TCatarinkaJSON.GetPropValue(propName: String): Variant;
begin
  if obj.sobject.o[propName] <> nil then
  begin
    case obj.sobject.o[propName].DataType of
      stNull:
        result := Null;
      stBoolean:
        result := obj.sobject.b[propName];
      stDouble:
        result := obj.sobject.d[propName];
      stInt:
        result := obj.sobject.i[propName];
      stString:
        result := obj.sobject.s[propName];
      // stObject,stArray, stMethod:
    end;
  end;
end;

function TCatarinkaJSON.SetPropValue(propName: String;
  const AValue: Variant): Boolean;
var
  ltype: Integer;
begin
  result := true;
  ltype := lua_type(L, 3);
  case ltype of
    LUA_TSTRING:
      obj.sobject.s[propName] := lua_tostring(L, 3);
    LUA_TBOOLEAN:
      obj.sobject.b[propName] := lua_toboolean(L, 3);
    LUA_TNUMBER:
      begin
        if TVarData(AValue).vType = varDouble then
          obj.sobject.d[propName] := lua_tonumber(L, 3)
        else
          obj.sobject.i[propName] := lua_tointeger(L, 3);
      end;
  else
    obj[propName] := AValue;
  end;
end;

end.
