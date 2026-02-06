unit CatJSON;

{
  Catarinka TCatJSON - JSON Manipulation Object
  Copyright (c) 2010-2025 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  06.09.2025:
  - Added PrettyPrintJson function
  15.02.2025:
  - Added methods RawGetValueStr, RawGetValueInt and RawGetValueBool. These
    methods use the iterator to access the desired key, which is useful fo
    keys containing dot in the name. Also added TreatMissingAsFalse property,
    useful for operations when the omission of key must be treated as a false
  31.08.2023:
  - Added GetValueDataType method
  05.06.2023:
  - Added support for Currency
  - Fix GetTextUnquoted method returning comma before closing JSON
  09.03.2022:
  - Added GetKeyList method
  02.10.2021:
  - Use int64 in TCatJSON_Int class and IncValue method
  11.09.2021:
  - Add AddToValue method
  16.08.2021:
  - Perform unicode decode when getting string
  14.08.2019:
  - Added compatibility with xsuperobject
  - Added RemovePath() method
  23.07.2017:
  - Added Count property and fixed handling of integer types
  - Added SetValues() for setting the value of multiple paths at the same time
    Renamed the SetVal() to SetValue() and added result to IncValue()
  - Added TCatJSON_Bool and TCatJSON_Int classes (practical if you want to 
    manipulate a JSON string that will only or mostly contain boolean or
    integer types)
  25.11.2015:
  - Set empty JSON string when calling SetText('')
  2013:
  - Added the HasPath method
  - Changed the default property from string to variant
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils, System.Variants,
{$ELSE}
  Classes, SysUtils, Variants,
{$ENDIF}
{$IFDEF USEXSUPEROBJECT}
  XSuperObject, XSuperJSON
{$ELSE}
  SuperObject
{$ENDIF}
  ;

const
  EmptyJSONStr = '{}';

type
  TCatJSON = class
  private
    fUseUnicode: boolean;
    fDefaultValue: Variant;
    fTreatMissingAsFalse: boolean;
    fObject: ISuperObject;
    function GetCount:integer;
    function GetText: string;
    function GetTextUnquoted: string;
    function GetValue_(const Name: string): Variant;
    procedure SetText(const Text: string);
  public
    function GetKeyList: string;
    function GetValue(const Name: string; DefaultValue: Variant): Variant;
    function GetValueStr(const Name: string; DefaultValue: string): string;
    function GetValueDataType(const Name:string):TSuperType;
    function HasPath(const Name: string): Boolean;
    function IncValue(const Name: string; Int: int64 = 1) : Int64;
    function RawGetValueStr(const Name: string; DefaultValue: string): string;
    function RawGetValueBool(const Name: string; DefaultValue: boolean): boolean;
    function RawGetValueInt(const Name: string; DefaultValue: Int64): Int64;
    procedure RemovePath(const Name: string);
    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);
    procedure AddToValue(const Name: string; const Value: string;
      const AddIfRepeated:boolean);
    procedure SetValue(const Name: string; const Value: Variant);
    procedure SetValues(const Name:array of string; const Value: Variant);
    procedure Clear;
    constructor Create(const JSON: string = '');
    destructor Destroy; override;
    // properties
    property UseUnicode: boolean read fUseUnicode write fUseUnicode;
    property Count: integer read GetCount;
    property GlobalDefaultValue: Variant read fDefaultValue write fDefaultValue;
    property sObject: ISuperObject read fObject;
    property Text: string read GetText write SetText;
    property TextUnquoted:string read GetTextUnquoted; // JSON with UnquotedKeys
    property TreatMissingAsFalse:boolean read fTreatMissingAsFalse write fTreatMissingAsFalse;
    property Values[const Name: string]: Variant read GetValue_ write SetValue; default;
  end;
  
type
  TCatJSON_Bool = class
  private
    fDefaultValue: boolean;
    fJSON: TCatJSON;
    function GetValue_(const Name: string): boolean;
  public
    function GetValue(const Name: string; const DefaultValue: boolean): boolean;
    procedure Clear;
    procedure SetValue(const Name: string; const DefaultValue: boolean);
    constructor Create(JSON: string = ''); overload;
    constructor Create(const DefaultValue: boolean;
      const JSON: string = ''); overload;
    destructor Destroy; override;
    // properties
    property JSON: TCatJSON read fJSON;
    property Values[const Name: string]: boolean read GetValue_
      write SetValue; default;
  end;

type
  TCatJSON_Int = class
  private
    fDefaultValue: int64;
    fJSON: TCatJSON;
    function GetValue_(const Name: string): int64;
  public
    function GetValue(const Name: string; const DefaultValue: int64): int64;
    function IncValue(const Name: string; Int: int64 = 1): int64;
    procedure Clear;
    procedure SetValue(const Name: string; const DefaultValue: int64);
    constructor Create(JSON: string = ''); overload;
    constructor Create(const DefaultValue: int64;
      const JSON: string = ''); overload;
    destructor Destroy; override;
    // properties
    property JSON: TCatJSON read fJSON;
    property Values[const Name: string]: int64 read GetValue_
      write SetValue; default;
  end;

{$IFDEF USEXSUPEROBJECT}
  type
    TSuperObjectIter = IMember;
{$ENDIF}

function PrettyPrintJson(const Json: string; Indent: Integer = 2): string;
function GetJSONVal(const JSON, Name: string;const DefaultValue: Variant): Variant;
function IsValidJSONName(const S: string): Boolean;
function JSONStringUnescape(const s:string):string;

implementation

uses CatFiles, CatStrings;

function PrettyPrintJson(const Json: string; Indent: Integer = 2): string;
var
  I, Level: Integer;
  InString: Boolean;
  C: Char;
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    Level := 0;
    InString := False;
    for I := 1 to Length(Json) do
    begin
      C := Json[I];
      case C of
        '"':
          begin
            SB.Append(C);
            if (I = 1) or (Json[I-1] <> '\') then
              InString := not InString;
          end;
        '{','[':
          begin
            SB.Append(C);
            if not InString then
            begin
              Inc(Level);
              SB.AppendLine;
              SB.Append(StringOfChar(' ', Level * Indent));
            end;
          end;
        '}',']':
          begin
            if not InString then
            begin
              Dec(Level);
              SB.AppendLine;
              SB.Append(StringOfChar(' ', Level * Indent));
              SB.Append(C);
            end
            else
              SB.Append(C);
          end;
        ',':
          begin
            SB.Append(C);
            if not InString then
            begin
              SB.AppendLine;
              SB.Append(StringOfChar(' ', Level * Indent));
            end;
          end;
        ':':
          begin
            SB.Append(C);
            if not InString then SB.Append(' ');
          end;
      else
        SB.Append(C);
      end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function JSONStringUnescape(const s:string):string;
var
 j:TCatJSON;
 jsonvalue:string;
begin
 jsonvalue := RemoveSurroundingChar(s,['"']);
 j := TCatJSON.Create('{"v":"'+jsonvalue+'"}');
 result := j.GetValue('v',emptystr);
 j.Free;
end;

function IsValidJSONName(const s: string): Boolean;
const
  cJSONChars = ['-', '_', 'a' .. 'z', 'A' .. 'Z', '0' .. '9'];
var
  i: integer;
begin
  Result := True;
  for i := 1 to Length(S) do
    if not(CharInSet(S[i], cJSONChars)) then
    begin
      Result := False;
      Break;
    end;
end;

function GetJSONVal(const JSON, Name: string;const DefaultValue: Variant): Variant;
var
  d: TCatJSON;
begin
  d := TCatJSON.Create(JSON);
  Result := d.GetValue(Name, DefaultValue);
  d.Free;
end;

function TCatJSON.GetCount:integer;
var
  ite: TSuperObjectIter;
begin
{$IFDEF USEXSUPEROBJECT}
  Result := fObject.Count;
{$ELSE}
  Result := 0;
  if ObjectFindFirst(fObject, ite) then
    repeat
      Inc(Result)
    until not ObjectFindNext(ite);
  ObjectFindClose(ite);
{$ENDIF}
end;

function TCatJSON.GetTextUnquoted: string;
var
  ite: TSuperObjectIter;
begin
  Result := '{';
{$IFDEF USEXSUPEROBJECT}
 for ite in fObject do
   Result := Result + crlf + ite.Name + ': ' + ite.ToString + ',';
{$ELSE}
  if ObjectFindFirst(fObject, ite) then
    repeat
      Result := Result + crlf + ite.key + ': ' + ite.Val.AsJson + ',';
    until not ObjectFindNext(ite);
  ObjectFindClose(ite);
{$ENDIF}
  if LastChar(result) = ',' then
  Result := RemoveLastChar(Result);
  Result := Result + '}';
end;

function TCatJSON.GetKeyList: string;
var
  ite: TSuperObjectIter;
  sl:TStringList;
begin
  sl := TStringList.Create;
{$IFDEF USEXSUPEROBJECT}
 for ite in fObject do
    sl.Add(ite.Name);
{$ELSE}
  if ObjectFindFirst(fObject, ite) then
    repeat
      sl.Add(ite.key);
    until not ObjectFindNext(ite);
  ObjectFindClose(ite);
{$ENDIF}
  result := sl.Text;
  sl.Free;
end;

function TCatJSON.GetText: string;
begin
  Result := fObject.AsJson(True);
end;

procedure TCatJSON.LoadFromFile(const Filename: string);
var
  sl: tstringlist;
begin
  sl := tstringlist.Create;
  SL_LoadFromFile(sl, Filename);
  SetText(sl.Text);
  sl.Free;
end;

procedure TCatJSON.SaveToFile(const Filename: string);
var
  sl: tstringlist;
begin
  sl := tstringlist.Create;
  sl.Text := GetText;
  SL_SaveToFile(sl, Filename);
  sl.Free;
end;

procedure TCatJSON.SetText(const Text: string);
var
  JSON: string;
begin
  JSON := trim(Text);
  if JSON = emptystr then
    JSON := EmptyJSONStr;
  if beginswith(json,'{') = false then
    JSON := EmptyJSONStr;
  if endswith(json,'}') = false then
    JSON := EmptyJSONStr;


  fObject := nil;
{$IFDEF USEXSUPEROBJECT}
  fObject := TSuperObject.Create(JSON, False);
{$ELSE}
  fObject := TSuperObject.ParseString(PWideChar(WideString(JSON)), False);
{$ENDIF}
end;

procedure TCatJSON.Clear;
begin
  SetText(EmptyJSONStr);
end;

constructor TCatJSON.Create(const JSON: string = '');
begin
  fTreatMissingAsFalse := false;
{$IFDEF UNICODE}
  fUseUnicode := true;
{$ELSE}
  fUseUnicode := false;
{$ENDIF}
  fDefaultValue := null;
{$IFDEF USEXSUPEROBJECT}
  if json<>emptystr then
  fObject := TSuperObject.Create(JSON) else
  fObject := TSuperObject.Create;
{$ELSE}
  fObject := TSuperObject.Create(stObject);
  Text := JSON;
{$ENDIF}
end;

destructor TCatJSON.Destroy;
begin
  fObject := nil;
  inherited;
end;

function TCatJSON.IncValue(const Name: string; Int: int64 = 1):int64;
begin
  result := GetValue(Name,0) + Int;
  fObject.I[name] := result;
end;

procedure TCatJSON.AddToValue(const Name: string; const Value: string;
      const AddIfRepeated:boolean);
var
  sl: TStringlist;
begin
  sl := TStringlist.Create;
  sl.commatext := GetValue(name, emptystr);
  if AddIfRepeated = true then
    sl.Add(Value)
  else
  begin
    if sl.indexof(Value) = -1 then
      sl.Add(Value);
  end;
  SetValue(name, sl.commatext);
  sl.Free;
end;

procedure TCatJSON.SetValue(const Name: string; const Value: Variant);
begin
  case TVarData(Value).vType of
    varString, {$IFDEF UNICODE}varUString, {$ENDIF}varOleStr:
      fObject.S[name] := Value;
    varBoolean:
      fObject.b[name] := Value;
    varInteger, varByte, varSmallInt, varShortInt, varWord, varLongWord,
    {$IFDEF UNICODE}varUInt64, {$ENDIF} varInt64:
      fObject.i[name] := Value;
    varCurrency:
      {$IFDEF USEXSUPEROBJECT}
      fObject.d[name] := CurrencyToDouble(VarAsType(Value, varCurrency));
     {$ELSE}
      fObject.c[name] := VarAsType(Value, varCurrency);
     {$ENDIF}
    varDouble: begin
     {$IFDEF USEXSUPEROBJECT}
      fObject.f[name] := VarAsType(Value, varDouble);
     {$ELSE}
      fObject.d[name] := VarAsType(Value, varDouble);
     {$ENDIF}
    end;
  end;
end;

procedure TCatJSON.SetValues(const Name:array of string; const Value: Variant);
var b: integer;
begin
  for b := Low(Name) to High(Name) do
    if (Name[b] <> emptystr) then
      SetValue(Name[b],Value);
end;

function TCatJSON.HasPath(const Name: string): Boolean;
begin
 {$IFDEF USEXSUPEROBJECT}
 Result := fObject.Contains(name);
 {$ELSE}
 Result := (fObject.O[name] <> nil);
 {$ENDIF}
end;

procedure TCatJSON.RemovePath(const Name: string);
begin
{$IFDEF USEXSUPEROBJECT}
  fObject.Remove(Name);
{$ELSE}
  fObject.O[Name].Clear;
{$ENDIF}
end;

function TCatJSON.GetValueStr(const Name: string;
  DefaultValue: string): string;
begin
  Result := DefaultValue;
  if HasPath(Name) then
  begin
    {$IFDEF USEXSUPEROBJECT}
      result := fObject.V[name];
    {$ELSE}
    case fObject.O[name].DataType of
      stNull:
        Result := DefaultValue;
      stString:
        {$IFDEF UNICODE}
        if fUseUnicode = true then
        Result := UnicodeString(fObject.S[Name]) else
        {$ENDIF}
        Result := fObject.S[Name];
      stObject, stArray, stMethod:
        Result := DefaultValue;
    end;
    {$ENDIF}
  end;
end;

function TCatJSON.RawGetValueStr(const Name: string;
  DefaultValue: string): string;
var
  {$IFNDEF USEXSUPEROBJECT}
  ite: TSuperObjectIter;
{$ENDIF}
begin
  Result := DefaultValue;
{$IFDEF USEXSUPEROBJECT}
 // ToDo
{$ELSE}
  if ObjectFindFirst(fObject, ite) then
    repeat
      if ite.key = Name then begin
        Result := ite.val.AsString;
        break;
      end;
    until not ObjectFindNext(ite);
  ObjectFindClose(ite);
{$ENDIF}
end;

function TCatJSON.RawGetValueBool(const Name: string;
  DefaultValue: boolean): boolean;
var
  {$IFNDEF USEXSUPEROBJECT}
  ite: TSuperObjectIter;
{$ENDIF}
begin
  Result := DefaultValue;
  if (fTreatMissingAsFalse = true) and (DefaultValue = true) then
    Result := false;

{$IFDEF USEXSUPEROBJECT}
 // ToDo
{$ELSE}
  if ObjectFindFirst(fObject, ite) then
    repeat
      if ite.key = Name then begin
        Result := ite.val.AsBoolean;
        break;
      end;
    until not ObjectFindNext(ite);
  ObjectFindClose(ite);
{$ENDIF}
end;

function TCatJSON.RawGetValueInt(const Name: string;
  DefaultValue: int64): int64;
var
  {$IFNDEF USEXSUPEROBJECT}
  ite: TSuperObjectIter;
{$ENDIF}
begin
  Result := DefaultValue;
{$IFDEF USEXSUPEROBJECT}
 // ToDo
{$ELSE}
  if ObjectFindFirst(fObject, ite) then
    repeat
      if ite.key = Name then begin
        Result := ite.val.AsInteger;
        break;
      end;
    until not ObjectFindNext(ite);
  ObjectFindClose(ite);
{$ENDIF}
end;

function TCatJSON.GetValueDataType(const Name:string):TSuperType;
begin
 if HasPath(Name) then
   result := fObject.O[name].DataType else
   result := stNull;
end;

function TCatJSON.GetValue(const Name: string;
  DefaultValue: Variant): Variant;
begin
  Result := DefaultValue;
  if HasPath(Name) then
  begin
    {$IFDEF USEXSUPEROBJECT}
    result := fObject.V[name];
    {$ELSE}
    case fObject.O[name].DataType of
      stNull:
        Result := DefaultValue;
      stBoolean:
        Result := fObject.b[Name];
      stDouble:
        Result := fObject.d[Name];
      stCurrency:
        Result := fObject.c[Name];
      stInt:
        Result := fObject.i[Name];
      stString:
        {$IFDEF UNICODE}
        if fUseUnicode = true then
        Result := UnicodeString(fObject.S[Name]) else
        {$ENDIF}
        Result := fObject.S[Name];
      stObject, stArray, stMethod:
        Result := DefaultValue;
    end;
    {$ENDIF}
  end;
end;

function TCatJSON.GetValue_(const Name: string): Variant;
begin
  Result := GetValue(name, fDefaultValue);
end;

// ----------------------------------------------------------------------------//

procedure TCatJSON_Bool.Clear;
begin
  fJSON.Clear;
end;

procedure TCatJSON_Bool.SetValue(const Name: string;
  const DefaultValue: boolean);
begin
  fJSON.SetValue(Name, DefaultValue);
end;

function TCatJSON_Bool.GetValue(const Name: string;
  const DefaultValue: boolean): boolean;
begin
  result := fJSON.GetValue(Name, DefaultValue)
end;

function TCatJSON_Bool.GetValue_(const Name: string): boolean;
begin
  result := fJSON.GetValue(Name, fDefaultValue);
end;

constructor TCatJSON_Bool.Create(JSON: string = '');
begin
  inherited Create;
  fJSON := TCatJSON.Create(JSON);
  fDefaultValue := false;
end;

constructor TCatJSON_Bool.Create(const DefaultValue: boolean;
  const JSON: string = '');
begin
  Create(JSON);
  fDefaultValue := DefaultValue;
end;

destructor TCatJSON_Bool.Destroy;
begin
  fJSON.Free;
  inherited;
end;

// ----------------------------------------------------------------------------//

procedure TCatJSON_Int.Clear;
begin
  fJSON.Clear;
end;

procedure TCatJSON_Int.SetValue(const Name: string;
  const DefaultValue: int64);
begin
  fJSON.SetValue(Name, DefaultValue);
end;

function TCatJSON_Int.GetValue(const Name: string;
  const DefaultValue: int64): int64;
begin
  result := fJSON.GetValue(Name, DefaultValue)
end;

function TCatJSON_Int.GetValue_(const Name: string): int64;
begin
  result := fJSON.GetValue(Name, fDefaultValue);
end;

function TCatJSON_Int.IncValue(const Name: string; Int: int64 = 1): int64;
begin
  result := fJSON.IncValue(Name, Int)
end;

constructor TCatJSON_Int.Create(JSON: string = '');
begin
  inherited Create;
  fJSON := TCatJSON.Create(JSON);
  fDefaultValue := 0;
end;

constructor TCatJSON_Int.Create(const DefaultValue: int64;
  const JSON: string = '');
begin
  Create(JSON);
  fDefaultValue := DefaultValue;
end;

destructor TCatJSON_Int.Destroy;
begin
  fJSON.Free;
  inherited;
end;

end.
