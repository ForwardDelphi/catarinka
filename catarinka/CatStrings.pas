unit CatStrings;
{
  Catarinka - String Operation functions

  Copyright (c) 2003-2025 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  IScan, SplitString, GetTextBetweenTags functions by Peter Below

  Note: random functions included with this library are not suitable
  for cryptographic purposes.
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  {$IFDEF USECROSSVCL}
  WinAPI.Windows,
  {$ENDIF} 
  System.Classes, System.SysUtils, System.StrUtils
  {$IFDEF MSWINDOWS}
  ,System.AnsiStrings;
  {$ELSE}
  ;
  {$ENDIF}
{$ELSE}
  Classes, SysUtils, StrUtils;
{$ENDIF}

// Useful for declaring a single record variable, instead of declaring two 
// variables like var somename, somevalue:string;
type
  TCatNameValue = record
    name: string;
    value: string;
  end;
  
// Useful for returning a boolean value together with a string like an error 
// message or an integer when calling a function
type
 TCatFuncResult = record
   B: boolean;
   I: int64;
   S: string;
   {$IFDEF DXE2_OR_UP}
   procedure SetTrue(const reason:string='');
   procedure SetFalse(const reason:string='');
   {$ENDIF}
 end;

type
  TCatCaseLabel = record
    name: string;
    id: integer;
  end;

function After(const s, substr: string): string;
function ASCIIToInt(const s: string): integer;
function Base64Encode(const s: string): string;
function Base64Decode(const s: string): string;
function Before(const s, substr: string): string;
function BeforeChars(const s: string; const aSet: TSysCharSet): string;
function BeginsWith(const s, prefix: string; IgnoreCase: Boolean = false)
  : Boolean; overload;
function BeginsWith(const s: string; const prefixes: array of string;
  IgnoreCase: Boolean = false): Boolean; overload;
function BoolToStr(const b: Boolean): string;
function BoolToYN(const b: Boolean): string;
procedure CatAppendStr(var s:string;const astr:string;const sep:string=',');
procedure CatAppendTag(var s:string; const tag:string; const sep:string=',');
function CatPadLeft(const s: string; const c: Char; const len: integer): string;
function CatPadRight(const s: string; const c: Char; const len: integer): string;
function CatWrapText(const text: string; const chars: integer): TStringList;
function CharSetToStr(const c: TSysCharSet): string;
function CombineIntArray(const p:array of integer):integer;
function CommaTextToStr(const s: string): string;
function ContainsAnyOfChars(const s: string; const aSet: TSysCharSet): Boolean;
function ContainsAnyCharsOtherThan(const s: string; const aSet: TSysCharSet): Boolean;
function ContainsAnyOfStrings(s: string; aArray: array of string;
  IgnoreCase: Boolean = false): Boolean;
function ContainsAllOfStrings(s: string; aArray: array of string;
  IgnoreCase: Boolean = false): Boolean;
function ContainsEmptyStr(const list: array of string): boolean;
function EndsWith(const s, prefix: string; IgnoreCase: Boolean = false)
  : Boolean; overload;
function EndsWith(const s: string; const prefixes: array of string;
  IgnoreCase: Boolean = false): Boolean; overload;
function ExtractEnclosed(const s: string;const beginChar,endChar:char): string;
function ExtractEnclosedSpecial(const s: string;const beginChar,beginChar2,endChar:char): string;
function ExtractFromString(const s, startstr, endstr: string): string;
function ExtractChars(const s:string;const aSet: TSysCharSet):string;
function ExtractNumbers(const s: string): string;
function GetLineNumberByPos(const s: string; const Position: integer): integer;
function GetStringLine(const s:string;const line:integer):TCatFuncResult;
function GetToken(const aString, SepChar: string; const TokenNum: Integer): string;
function GetTokenFromRight(const aString, SepChar: string; const TokenNum: Integer): string;
function GetRandomStringFromList(sl:TStringList):string;
function GetValidCompName(const s: string): string;
function HexToInt(const Hex: string; const WarnError: Boolean = false): integer;
function HexToStr(const s: string): string;
function Hex16ToStr(const s: string): string;
function IIF(const Cond: Boolean; const TrueStr: String;
  const FalseStr: string = ''): string; overload;
function IIF(const Cond: Boolean; const TrueInt: integer;
  const FalseInt: integer = 0): integer; overload;
function IScan(ch: Char; const s: string; fromPos: integer): integer;
function IsAlpha(const s: string): Boolean;
function IsAlphaNumeric(const s: string): Boolean;
function IsAlphaOrNumeric(const c: Char): Boolean;
function IsHexStr(const s: string): Boolean;
function IsInteger(const s: string): Boolean;
function IsIntBetween(const i, i1, i2:int64):boolean;
function IsLowercase(const s: string): Boolean;
function IsUppercase(const s: string): Boolean;
function IsRoman(const s: string): Boolean;
function IsValidVersion(const S: string): Boolean;
function LastChar(const s: string): Char;
function LeftStr(s: string; c: longword): string;
function MatchIntInArray(const i: integer; aArray: array of integer): Boolean;
function MatchStrInArray(s: string; aArray: array of string;
  IgnoreCase: Boolean = false): Boolean;
function MatchStrInArrayIdx(s: string; aArray: array of string;
  IgnoreCase: Boolean = false): integer;
function MemStreamToStr(m: TMemoryStream): String;
function NumToWords(dblValue : Double) : String;
function NumToWordsShort(dblValue : Double) : string;
function Occurs(substr, s: string): integer;
function RandomizeTokenizedStr(const input: string;aseparator:char=','): string;
function RandomCase(const s: string;
  const ToUpperSet: TSysCharSet = ['a' .. 'z'];
  const ToLowerSet: TSysCharSet = ['A' .. 'Z']): string;
function RandomString(const len: integer;
  const chars: string = 'abcdefghijklmnopqrstuvwxyz'): string;
function RemoveDuplicatedTags(const s:string):string;
function RemoveLastChar(const s: string): string;
function RemoveNumbers(const s: string): string;
function RemoveQuotes(const s: string): string;
function RemoveShortcuts(const s: string): string;
function RemoveSurroundingChar(const s: string;const aSet:TSysCharSet): string;
function RepeatString(const s: string; count: cardinal): string;
function ReplaceChars(const s: string; const aSet: TSysCharSet;
  const repwith: Char = '_'): string;
function ReplaceStr(const s, substr, repstr: string): string;
function RestStr(const s: string; const index: longword): string;
procedure SL_RemoveDuplicates(const sl : TStringList);
function StrDecrease(const s: string; const step: integer = 1): string;
function StrIncrease(const s: string; const step: integer = 1): string;
function StripChars(const s: string; const aSet: TSysCharSet): string;
function StripEnclosed(const s: string;const beginChar,endChar: Char): string;
function StripEnclosedPreserve(const s: string; const beginChar, endChar: char): string;
function StrMaxLen(const s: string; const MaxLen: integer;
  const AddEllipsis: Boolean = false): string;
function StrToAlphaNum(const s: string): string;
function StrArrayToCommaText(aArray: array of string):string;
function StrArrayToText(aArray: array of string):string;
function StrArrayToSeparatedStr(aArray: array of string;aSepStr:string=' '):string;
function StrToBool(const s: string): Boolean;
function StrToCharSet(const s: string): TSysCharSet;
function StrToCommaText(const s: string): string;
function StrDef(const S, Default: string): string;
function StrToHex(const s: string): string;
function StrToHex16(const s: string): string;
function StrToNameValue(const s: string; const aSeparator:string='='): TCatNameValue;
function StrToYN(const s:string):string;
function SwapCase(const s: string): string;
function TitleCase(const s: string): string;
procedure WriteUtf8StringToStream(const S: string; Dest: TStream);
function ReadUtf8StringFromStream(Src: TStream): string;

{$IFDEF MSWINDOWS}
// TODO: not compatible for crosscompilation
procedure GetTextBetweenTags(const s, tag1, tag2: string; const list: TStrings;
  const includetags: Boolean = false);
{$ENDIF}
procedure MergeStrings(const dest, source: TStrings);
procedure SplitString(const s: string; separator: Char;
  substrings: TStringList);

{$IFDEF CHARINSET_UNAVAILABLE}
function CharInSet(c: Char; CharSet: TSysCharSet): Boolean;
{$ENDIF}


// string list related functions
function CompareStrings(sl: TStringList; Index1, Index2: integer): integer;
procedure StripBlankLines(const sl: TStringList);

{ 
  Case Statement with String:
  Since Delphi 7 to XE and most Pascal compilers don't support case statement 
  with string, many developers created workaround methods that work similar, 
  but I never found one I really liked, so I came up with this.
  It is ugly, but works very well.
   
  CatCaseLabelOf() requires an array of TCatCaseLabel
  Usage example:

  procedure TForm1.Button1Click(Sender: TObject);
  const
   c_ana = 1;
   c_roberto = 2;
   c_lucia = 3;
  const
   labels : array [1..3] of TCatCaseLabel =
   (
   (name:'ana';id:c_ana),
   (name:'lucia';id:c_lucia),   
   (name:'roberto';id:c_roberto)
   );
  begin
   case CatCaseLabelOf(edit1.text,labels) of
    c_ana: form1.caption:='ana!';
    c_roberto: form1.caption:='roberto!';
    c_lucia: form1.caption:='lucia!';
   else
    form1.Caption:=edit1.text+' not in case list!';
   end;
  end;  
  
  CatCaseOf() is a simple alternative of the above and works with an array of
  strings
  Usage example:
  case CatCaseOf(inputstring, ['1:dog', '2:cat', '3:bird', '3:horse']) of
   1: result := 'dog';
   2: result := 'cat';
   3: result := 'bird or horse';
  else
   result := 'nomatch';  
  end;  
  
  CatCaseWildOf() in CatMatch.pas allows case statement based on wildcard strings
  
}
function CatCaseOf(const s: string; labels: array of string;
  const casesensitive: Boolean = true): integer; 
function CatCaseLabelOf(const s: string; labels: array of TCatCaseLabel;
  const casesensitive: Boolean = true): integer; 
function CatCaseLabelOf_GetName(const id: integer; labels: array of TCatCaseLabel): string;
function CatCaseLabelOf_EndsWith(const s: string; labels: array of TCatCaseLabel;
  const casesensitive: Boolean = true): integer; 

const
  CRLF = #13 + #10;

implementation

uses
  CatBase64;

{$IFDEF CHARINSET_UNAVAILABLE}

// Before D2009
function CharInSet(c: Char; CharSet: TSysCharSet): Boolean;
begin
  if c in CharSet then
    result := true
  else
    result := false;
end;
{$ENDIF}

{$IFDEF DXE2_OR_UP}
procedure TCatFuncResult.SetTrue(const reason:string='');
begin
  self.B := true;
  self.S := reason;
end;

procedure TCatFuncResult.SetFalse(const reason:string='');
begin
  self.B := false;
  self.S := reason;
end;
{$ENDIF}

procedure WriteUtf8StringToStream(const S: string; Dest: TStream);
var
  L: Integer;
  Bytes: TBytes;
begin
  Bytes := TEncoding.UTF8.GetBytes(S);
  L := Length(Bytes);
  Dest.WriteBuffer(L, SizeOf(L));
  if L > 0 then
    Dest.WriteBuffer(Bytes[0], L);
end;

function ReadUtf8StringFromStream(Src: TStream): string;
var
  L: Integer;
  Bytes: TBytes;
begin
  Result := '';
  if Src.Read(L, SizeOf(L)) <> SizeOf(L) then
    Exit;
  if L <= 0 then
    Exit;
  SetLength(Bytes, L);
  if Src.Read(Bytes[0], L) <> L then
    Exit;
  Result := TEncoding.UTF8.GetString(Bytes);
end;

function After(const s, substr: string): string;
var
  i: integer;
begin
  i := pos(substr, s);
  if i = 0 then
    result := emptystr
  else
    result := Copy(s, i + length(substr), length(s));
end;

function ASCIIToInt(const s: string): integer;
var
  i, len: integer;
  c: Char;
begin
  result := 0;
  len := length(s);
  for i := len downto 1 do
  begin
    c := s[i];
    result := result + ord(c) shl ((len - i) shl 8);
  end;
end;

function Base64Encode(const s: string): string;
begin
  Result := CatBase64.Base64Encode(s);
end;

function Base64Decode(const s: string): string;
begin
  Result := CatBase64.Base64Decode(s);
end;

function Before(const s, substr: string): string;
var
  i: integer;
begin
  i := pos(substr, s);
  if i = 0 then
    result := s
  else
    result := Copy(s, 1, i - 1);
end;

function BeforeChars(const s: string; const aSet: TSysCharSet): string;
var
  i, P: integer;
begin
  P := 0;
  SetLength(result, length(s));
  for i := 1 to length(s) do
  begin
    if (CharInSet(s[i], aSet)) then
    begin
      inc(P);
      result[P] := s[i];
    end else
    break;
  end;
  SetLength(result, P);
end;

function BeginsWith(const s, prefix: string;
  IgnoreCase: Boolean = false): Boolean;
begin
  if IgnoreCase = false then
  result := AnsiStartsStr(prefix, s) else
  result := AnsiStartsText(prefix, s);
end;

function BeginsWith(const s: string; const prefixes: array of string;
  IgnoreCase: Boolean = false): Boolean;
var
  b: integer;
begin
  result := false;
  for b := Low(prefixes) to High(prefixes) do
  begin
    if BeginsWith(s, prefixes[b], IgnoreCase) = true then
    begin
      result := true;
      break;
    end;
  end;
end;

function BoolToStr(const b: Boolean): string;
begin
  if b = true then
    result := 'True'
  else
    result := 'False';
end;

function BoolToYN(const b: Boolean): string;
begin
  if b = true then
    result := 'Yes'
  else
    result := 'No';
end;

function StrToYN(const s:string):string;
begin
  result := BoolToYN(StrToBool(s));
end;

// Appends a string with a separator string
procedure CatAppendStr(var s:string; const astr:string; const sep:string=',');
begin
 if s = emptystr then
   s := astr else begin
   s := s + sep + astr;
 end;
end;

// Similar to CatAppendStr, but does not allow empty string or duplicated entries
procedure CatAppendTag(var s:string; const tag:string; const sep:string=',');
var atag:string;
begin
  atag := trim(tag);
  if atag = sep then
    atag := emptystr;
  if s = emptystr then begin
    s := atag;
  end else begin
    if (atag <> emptystr) then begin
      s := s + sep + atag;
      if sep = ',' then
      s := RemoveDuplicatedTags(s);
    end;
  end;
end;

// Removes duplicated entries from string (comma-separated)
function RemoveDuplicatedTags(const s:string):string;
var sl: TStringList;
begin
  sl := TStringList.Create;
  sl.CommaText := s;
  SL_RemoveDuplicates(sl);
  result := trim(sl.CommaText);
  sl.Free;
end;

// Removes duplicate items in string list
procedure SL_RemoveDuplicates(const sl : TStringList);
var
   buf: TStringList;
   i: integer;
begin
   sl.Sort;
   buf := TStringList.Create;
   try
     buf.Sorted := True;
     buf.Duplicates := dupIgnore;
     buf.BeginUpdate;
     for i := 0 to sl.Count - 1 do
       buf.Add(sl[i]) ;
     buf.EndUpdate;
     sl.Assign(buf) ;
   finally
     FreeandNil(buf);
   end;
end;

function CatCaseOf(const s: string; labels: array of string;
  const casesensitive: Boolean = true): integer;
var
  i: integer;
  astr: string;
begin
  result := -1; // label not found
  astr := s;
  if casesensitive = false then begin
    astr := lowercase(astr);
    for i := low(labels) to high(labels) do
      labels[i] := lowercase(labels[i]);
  end;
  for i := low(labels) to high(labels) do
  begin
    if astr = after(labels[i],':') then
      result := StrToIntDef(before(labels[i],':'), -1);
    if result <> -1 then
      break;
  end;
end;

function CatCaseLabelOf(const s: string; labels: array of TCatCaseLabel;
  const casesensitive: Boolean = true): integer; overload;
var
  i: integer;
  astr: string;
begin
  result := -1; // label not found
  astr := s;
  if casesensitive = false then begin
    astr := lowercase(astr);
    for i := low(labels) to high(labels) do
      labels[i].name := lowercase(labels[i].name);
  end;
  for i := low(labels) to high(labels) do
  begin
    if astr = labels[i].name then
      result := labels[i].id;
    if result <> -1 then
      break;
  end;
end;

function CatCaseLabelOf_EndsWith(const s: string; labels: array of TCatCaseLabel;
  const casesensitive: Boolean = true): integer; overload;
var
  i: integer;
  astr: string;
begin
  result := -1; // label not found
  astr := s;
  if casesensitive = false then begin
    astr := lowercase(astr);
    for i := low(labels) to high(labels) do
      labels[i].name := lowercase(labels[i].name);
  end;
  for i := low(labels) to high(labels) do
  begin
    if endswith(astr,labels[i].name) then
      result := labels[i].id;
    if result <> -1 then
      break;
  end;
end;

function CatCaseLabelOf_GetName(const id: integer; labels: array of TCatCaseLabel): string;
var
  i: integer;
begin
  result := EmptyStr; // label not found
  for i := low(labels) to high(labels) do
  begin
    if id = labels[i].id then
      result := labels[i].name;
    if result <> EmptyStr then
      break;
  end;
end;

// Wraps a text and returns it as a stringlist
function CatWrapText(const text: string; const chars: integer): TStringList;
var
  sl: TStringList;
  P, ln: integer;
  s, newln: string;
begin
  sl := TStringList.Create;
  result := sl;
  if length(text) = 0 then
    Exit;
  ln := 0;
  sl.Add(emptystr);
  s := text + ' ';
  P := pos(' ', s);
  while P <> 0 do
  begin
    newln := Copy(s, 1, P);
    if (length(sl.Strings[ln]) + length(newln)) < (chars + 1) then
      sl.Strings[ln] := sl.Strings[ln] + newln
    else
    begin
      sl.Add(newln);
      inc(ln);
    end;
    Delete(s, 1, P);
    P := pos(' ', s);
  end;
end;

function ContainsAnyOfChars(const s: string; const aSet: TSysCharSet): Boolean;
var
  i: integer;
begin
  result := false;
  for i := 1 to length(s) do
  begin
    if (CharInSet(s[i], aSet)) then
    begin
      result := true;
      break;
    end;
  end;
end;

function ContainsAnyCharsOtherThan(const s: string; const aSet: TSysCharSet): Boolean;
var
  i: integer;
begin
  result := false;
  for i := 1 to length(s) do
  begin
    if not (CharInSet(s[i], aSet)) then
    begin
      result := true;
      break;
    end;
  end;
end;

function ContainsAllOfStrings(s: string; aArray: array of string;
  IgnoreCase: Boolean = false): Boolean;
var
  b: integer;
begin
  result := true;
  if IgnoreCase then
  begin
    s := lowercase(s);
    for b := Low(aArray) to High(aArray) do
      aArray[b] := lowercase(aArray[b]);
  end;
  for b := Low(aArray) to High(aArray) do
  begin
    if pos(aArray[b], s) = 0 then
    begin
      result := false;
      break;
    end;
  end;
end;

function ContainsAnyOfStrings(s: string; aArray: array of string;
  IgnoreCase: Boolean = false): Boolean;
var
  b: integer;
begin
  result := false;
  if IgnoreCase then
  begin
    s := lowercase(s);
    for b := Low(aArray) to High(aArray) do
      aArray[b] := lowercase(aArray[b]);
  end;
  for b := Low(aArray) to High(aArray) do
  begin
    if pos(aArray[b], s) <> 0 then
    begin
      result := true;
      break;
    end;
  end;
end;

// Checks if an array of strings contains an empty string
function ContainsEmptyStr(const list: array of string): boolean;
var
  i: integer;
begin
  result := false;
    for i := low(list) to high(list) do begin
      if list[i] = emptystr then begin
        result := true;
        break;
      end;
    end;
end;

// Useful for sorting a string list containing filenames
// Usage sl.CustomSort(CompareStrings);
function CompareStrings(sl: TStringList; Index1, Index2: integer): integer;
begin
  if Length(sl[Index1]) = Length(sl[Index2]) then
  begin
    if sl[Index1] = sl[Index2] then
      result := 0
    else if sl[Index1] < sl[Index2] then
      result := -1
    else
      result := 1;
  end
  else if Length(sl[Index1]) < Length(sl[Index2]) then
    result := -1
  else
    result := 1;
end;

function CombineIntArray(const p:array of integer):integer;
var i:integer; s:string;
begin
  s := EmptyStr;
  for i := low(p) to high(p) do begin
    if (p[i] > -1) then
    s := s+IntToStr(p[i]);
  end;
  result := StrToInt(s);
end;

function EndsWith(const s, prefix: string; IgnoreCase: Boolean = false)
  : Boolean;
begin
  if IgnoreCase = false then
    result := AnsiEndsStr(prefix, s)
  else
    result := AnsiEndsText(prefix, s);
end;

function EndsWith(const s: string; const prefixes: array of string;
  IgnoreCase: Boolean = false): Boolean;
var
  b: integer;
begin
  result := false;
  for b := Low(prefixes) to High(prefixes) do
  begin
    if EndsWith(s, prefixes[b], IgnoreCase) then
    begin
      result := true;
      break;
    end;
  end;
end;

function GetLineNumberByPos(const s: string; const Position: integer): integer;
var
  i, ln: integer;
begin
  result := -1;
  if (Position = -1) then
    Exit;

  i := 1;
  ln := 0;
  while i < Position do
  begin
    if (s[i] = #13) then
      ln := ln + 1;
    i := i + 1;
  end;
  result := ln;
end;

function GetStringLine(const s:string;const line:integer):TCatFuncResult;
var
  sl: TStringList;
  i: integer;
begin
  result.B := false;
  result.S := emptystr;
  sl := TStringList.Create;
  sl.Text := s;
  for i := 0 to sl.Count -1 do begin
    if i = line then begin
      result.B := true;
      result.S := sl[i];
      break;
    end;
  end;
  sl.Free;
end;

function GetRandomStringFromList(sl:TStringList):string;
var
  RandomIndex: Integer;
begin
  Randomize;
  RandomIndex := Random(sl.Count);
  if sl.Count <> 0 then
  result := sl[RandomIndex];
end;

// Returns a valid Pascal component name (stripping invalid chars)
function GetValidCompName(const s: string): string;
var
  i: integer;
begin
  result := emptystr;
  for i := 1 to length(s) do
  begin
    if (CharInSet(s[i], ['0' .. '9', 'A' .. 'Z', 'a' .. 'z', '_'])) then
      result := result + Copy(s, i, 1);
  end;
end;

function HexToInt(const Hex: string; const WarnError: Boolean = false): integer;
begin
  if IsHexStr(Hex) then
    result := StrToInt('$' + Hex)
  else
  begin
    if WarnError = true then
      raise EConvertError.Create('Invalid character in hex string')
    else
      result := 0;
  end;
end;

function IIF(const Cond: Boolean; const TrueStr: String;
  const FalseStr: String = ''): string; overload;
begin
  if Cond = true then
    result := TrueStr
  else
    result := FalseStr;
end;

function IIF(const Cond: Boolean; const TrueInt: integer;
  const FalseInt: integer = 0): integer; overload;
begin
  if Cond = true then
    result := TrueInt
  else
    result := FalseInt;
end;

function IsAlpha(const s: string): Boolean;
var
  i: integer;
begin
  result := true;
  for i := 1 to length(s) do
    if CharInSet(s[i], ['A' .. 'Z', 'a' .. 'z']) = false then
    begin
      result := false;
      break;
    end;
end;

// String must contain a least 1 letter and 1 number
function IsAlphaNumeric(const s: string): Boolean;
var
  i: integer;
  alpha, num: Boolean;
begin
  alpha := false;
  num := false;
  for i := 1 to length(s) do
  begin
    if CharInSet(s[i], ['A' .. 'Z', 'a' .. 'z']) then
      alpha := true
    else if CharInSet(s[i], ['0' .. '9']) then
      num := true;
  end;
  result := alpha and num;
end;

// Character must be alpha or numeric
function IsAlphaOrNumeric(const c: Char): Boolean;
begin
  result := CharInSet(c, ['A' .. 'Z', 'a' .. 'z', '0' .. '9']);
end;

function IsInteger(const s: string): Boolean;
var
  v: int64;
begin
  Result := TryStrToInt64(s, v);
end;

function IsIntBetween(const i, i1, i2:int64):boolean;
begin
  Result := (i >= i1) and (i <= i2);
end;

// Returns true if the string contains valid hexadecimal digits
function IsHexStr(const s: string): Boolean;
var
  i: integer;
begin
  result := true;
  for i := 1 to length(s) do
    if not(CharInSet(s[i], ['0' .. '9', 'A' .. 'F', 'a' .. 'f'])) then
    begin
      result := false;
      break;
    end;
end;

function IsUppercase(const s: string): Boolean;
var
  i: integer;
begin
  result := false;
  for i := 1 to length(s) do
    if CharInSet(s[i], ['a' .. 'z']) then
      Exit;
  result := true;
end;

function IsLowercase(const s: string): Boolean;
var
  i: integer;
begin
  result := false;
  for i := 1 to length(s) do
    if CharInSet(s[i], ['A' .. 'Z']) then
      Exit;
  result := true;
end;

function IsRoman(const s: string): Boolean;
var
  i: integer;
begin
  result := true;
  for i := 1 to length(s) do
  begin
    if CharInSet(UpCase(s[i]), ['I', 'V', 'X', 'L', 'C', 'D', 'M']) = false then
    begin
      result := false;
      Exit;
    end;
  end;
end;

// Check if a String is a Version Number
function IsValidVersion(const S: string): Boolean;
var
  Parts: TArray<string>;
  I, Num: Integer;
begin
  Result := False;
  Parts := S.Split(['.']);
  if Length(Parts) = 0 then
    Exit;
  for I := 0 to High(Parts) do
  begin
    if not TryStrToInt(Parts[I], Num) then
      Exit;
    if Num < 0 then
      Exit;
  end;
  Result := True;
end;

function LastChar(const s: string): Char;
begin
  if s = emptystr then
    result := #0
  else
    result := s[length(s)];
end;

function CatPadRight(const s: string; const c: Char; const len: integer): string;
var
  i: integer;
begin
  result := s;
  i := len - length(s);
  if i < 1 then
    Exit;
  result := s + StringOfChar(c, i);
end;

function CatPadLeft(const s: string; const c: Char; const len: integer): string;
var
  i: integer;
begin
  result := s;
  i := len - length(s);
  if i < 1 then
    Exit;
  result := StringOfChar(c, i) + s;
end;

function LeftStr(s: string; c: longword): string;
begin
  result := Copy(s, 1, c);
end;

function MatchIntInArray(const i: integer; aArray: array of integer): Boolean;
var
  b: integer;
begin
  result := false;
  for b := Low(aArray) to High(aArray) do
  begin
    if i = aArray[b] then begin
      result := true;
      break;
    end;
  end;
end;

function MatchStrInArray(s: string; aArray: array of string;
  IgnoreCase: Boolean = false): Boolean;
var
  b: integer;
begin
  result := false;
  if IgnoreCase then
  begin
    s := lowercase(s);
    for b := Low(aArray) to High(aArray) do
      aArray[b] := lowercase(aArray[b]);
  end;
  for b := Low(aArray) to High(aArray) do
  begin
    if s = aArray[b] then begin
      result := true;
      break;
    end;
  end;
end;

// Works like MatchStrInArray, but instead of boolean, returns 0 if not matched
// or the non-zero based index of the matched string
function MatchStrInArrayIdx(s: string; aArray: array of string;
  IgnoreCase: Boolean = false): integer;
var
  b: integer;
begin
  result := 0;
  if IgnoreCase then
  begin
    s := lowercase(s);
    for b := Low(aArray) to High(aArray) do
      aArray[b] := lowercase(aArray[b]);
  end;
  for b := Low(aArray) to High(aArray) do
  begin
    if s = aArray[b] then begin
      result := b+1;
      break;
    end;
  end;
end;

procedure MergeStrings(const dest, source: TStrings);
var
  i: integer;
begin
  for i := 0 to -1 + source.count do
    if dest.IndexOf(source[i]) = -1 then
      dest.Add(source[i]);
end;

function Occurs(substr, s: string): integer;
var
  i: integer;
begin
  result := 0;
  for i := 1 to length(s) do
    if Copy(s, i, length(substr)) = substr then
      inc(result);
end;

function RandomCase(const s: string;
  const ToUpperSet: TSysCharSet = ['a' .. 'z'];
  const ToLowerSet: TSysCharSet = ['A' .. 'Z']): string;
var
  i: integer;
begin
  Randomize();
  result := s;
  for i := 1 to length(result) do
    if Random(2) = 1 then
      if CharInSet(result[i], ToLowerSet) then
        inc(result[i], 32)
      else if CharInSet(result[i], ToUpperSet) then
        Dec(result[i], 32);
end;

function RandomString(const len: integer;
  const chars: string = 'abcdefghijklmnopqrstuvwxyz'): string;
begin
  Randomize;
  result := emptystr;
  repeat
    result := result + chars[Random(length(chars)) + 1];
  until (length(result) = len);
end;

function RemoveSurroundingChar(const s: string;const aSet:TSysCharSet): string;
var
  i: integer;
begin
  result := s;
  i := length(s);
  if i = 0 then
    Exit;
  if (CharInSet(s[1], aSet) = true) and (s[1] = LastChar(s)) then
  begin
    Delete(result, 1, 1);
    SetLength(result, length(result) - 1);
  end;
end;

// Strips a quote pair off a string if it exists
// The leading and trailing quotes will only be removed if both exist
// Otherwise, the string is left unchanged
function RemoveQuotes(const s: string): string;
begin
  result := RemoveSurroundingChar(s, ['"', '''']);
end;

// Removes the last character from a string
function RemoveLastChar(const s: string): string;
var
  len: integer;
  astr: string;
begin
  astr := s;
  len := length(astr);
  if len > 0 then
    Delete(astr, len, 1);
  result := astr;
end;

function RemoveNumbers(const s: string): string;
var
  i, l: integer;
begin
  SetLength(result, length(s));
  l := 0;
  for i := 1 to length(s) do
    if not(CharInSet(s[i], ['0' .. '9'])) then
    begin
      inc(l);
      result[l] := s[i];
    end;
  SetLength(result, l);
end;

function RemoveShortcuts(const s: string): string;
begin
  result := ReplaceStr(s, '&', emptystr);
end;

function RepeatString(const s: string; count: cardinal): string;
var
  i: Integer;
begin
  Result := ''; // <-- important initialization
  for i := 1 to count do
    Result := Result + s;
end;

function ReplaceStr(const s, substr, repstr: string): string;
begin
  result := stringreplace(s, substr, repstr, [rfReplaceAll]);
end;

function StrIncrease(const s: string; const step: integer = 1): string;
var
  i, c: integer;
  tmpstr: WideString;
begin
  tmpstr := '';
  for i := 1 to length(s) do
  begin
    c := ord(s[i]);
    inc(c, step);
    tmpstr := tmpstr + widechar(c);
  end;
  result := tmpstr;
end;

function StrDecrease(const s: string; const step: integer = 1): string;
var
  i, c: integer;
  tmpstr: WideString;
begin
  tmpstr := '';
  for i := 1 to length(s) do
  begin
    c := ord(s[i]);
    Dec(c, step);
    tmpstr := tmpstr + widechar(c);
  end;
  result := tmpstr;
end;

function RestStr(const s: string; const index: longword): string;
var
  l: integer;
begin
  l := length(s);
  if l > 0 then
    result := Copy(s, index, l)
  else
    result := emptystr;
end;

procedure StripBlankLines(const sl: TStringList);
var
  i: integer;
begin
  for i := (sl.count - 1) downto 0 do
  begin
    if (Trim(sl[i]) = emptystr) then
      sl.Delete(i);
  end;
end;

function StrToCommaText(const s: string): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.text := s;
  result := sl.CommaText;
  sl.free;
end;

function StripChars(const s: string; const aSet: TSysCharSet): string;
var
  i, P: integer;
begin
  P := 0;
  SetLength(result, length(s));
  for i := 1 to length(s) do
  begin
    if not(CharInSet(s[i], aSet)) then
    begin
      inc(P);
      result[P] := s[i];
    end;
  end;
  SetLength(result, P);
end;

// Gets a token-separated string and changes its order randomly
// Eg: 1,2,3 becomes 2,1,3 etc
function RandomizeTokenizedStr(const input: string;aseparator:char=','): string;
var
  list: TStringList;
  i: Integer;
begin
  // Create a TStringList to hold the individual values
  list := TStringList.Create;
  try
    // Set the delimiter to comma
    list.StrictDelimiter := true;
    list.Delimiter := aseparator;
    // Load the input string into the TStringList
    list.DelimitedText := input;

    // Randomize the order of the elements
    for i := 0 to list.Count - 1 do
      list.Exchange(i, Random(list.Count));

    // Concatenate the elements back into a string with comma separation
    result := list.DelimitedText;
  finally
    // Free the TStringList
    list.Free;
  end;
end;

function ReplaceChars(const s: string; const aSet: TSysCharSet;
  const repwith: Char = '_'): string;
var
  i, P: integer;
begin
  P := 0;
  SetLength(result, length(s));
  for i := 1 to length(s) do
  begin
    inc(P);
    result[P] := s[i];
    if (CharInSet(s[i], aSet)) then
      result[P] := repwith;
  end;
  SetLength(result, P);
end;

function CommaTextToStr(const s: string): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.CommaText := s;
  result := sl.GetText;
  sl.free;
end;

function StrMaxLen(const s: string; const MaxLen: integer;
  const AddEllipsis: Boolean = false): string;
var
  i: integer;
begin
  result := s;
  if length(result) <= MaxLen then
    Exit;
  SetLength(result, MaxLen);
  if AddEllipsis = true then begin
      for i := MaxLen downto MaxLen - 2 do
      result[i] := '.';
  end;
end;

function StrToAlphaNum(const s: string): string;
var
  i: integer;
  tmpstr: string;
begin
  tmpstr := emptystr;
  for i := 1 to length(s) do
  begin
    if (CharInSet(s[i], ['0' .. '9', 'A' .. 'Z', 'a' .. 'z'])) then
      tmpstr := tmpstr + Copy(s, i, 1);
  end;
  result := tmpstr;
end;

function StrArrayToCommaText(aArray: array of string):string;
var
  b: integer;
  sl: TStringList;
begin
  sl := TStringList.Create;
  for b := Low(aArray) to High(aArray) do
    sl.add(aArray[b]);
  result := sl.CommaText;
  sl.Free;
end;

function StrArrayToText(aArray: array of string):string;
var
  b: integer;
  sl: TStringList;
begin
  sl := TStringList.Create;
  for b := Low(aArray) to High(aArray) do
    sl.add(aArray[b]);
  result := sl.Text;
  sl.Free;
end;

function StrArrayToSeparatedStr(aArray: array of string;aSepStr:string=' '):string;
var
  b: integer;
begin
  result := emptystr;
  for b := Low(aArray) to High(aArray) do
    result := result+aSepStr+aArray[b];
end;

function StrToBool(const s: string): Boolean;
begin
  if MatchStrInArray(Trim(lowercase(s)), ['true', '1', 'yes', 't', 'y', 'on']) then
    result := true
  else
    result := false;
end;

function StrDef(const S, Default: string): string;
begin
  if S <> emptystr then
    Result := S
  else
    Result := Default;
end;

function StrToCharSet(const s: string): TSysCharSet;
var
  P: PAnsiChar;
begin
  result := [];
  if s = emptystr then
    Exit;
  P := PAnsiChar(AnsiString(s));
  while P^ <> #0 do
  begin
    Include(result, P^);
    inc(P);
  end;
end;

function CharSetToStr(const c: TSysCharSet): string;
var
  i: integer;
begin
  result := emptystr;
  for i := 0 to 255 do
    if CharInSet(Chr(i),c) then
      result := result + Chr(i);
end;

function StrToHex(const s: string): string;
var
  i: integer;
begin
  result := emptystr;
  for i := 1 to length(s) do
    result := result + IntToHex(ord(Copy(s, i, 1)[1]), 2);
end;

function StrToHex16(const s: string): string;
var
  i: integer;
  str: string;
begin
  str := emptystr;
  for i := 1 to length(s) do
    str := str + IntToHex(integer(s[i]), 4);
  result := str;
end;

function HexToStr(const s: string): string;
var
  i: integer;
  h: string;
begin
  result := emptystr;
  try
    for i := 1 to length(s) div 2 do
    begin
      h := Copy(s, (i - 1) * 2 + 1, 2);
      result := result + Char(StrToInt('$' + h));
    end;
  except
    result := emptystr;
  end;
end;

function Hex16ToStr(const s: string): string;
var
  i: integer;
  c: string;
begin
  result := emptystr;
  i := 1;
  while i < length(s) do
  begin
    c := Copy(s, i, 4);
    result := result + Chr(StrToInt('$' + c));
    inc(i, 4);
  end;
end;

// Strips all text enclosed between beginChar and endChar, preserving the number of lines
function StripEnclosedPreserve(const s: string; const beginChar, endChar: char): string;
var
  i: integer;
  strip: boolean;
  resultBuilder: TStringBuilder;
begin
  resultBuilder := TStringBuilder.Create;
  try
    strip := false;
    for i := 1 to length(s) do
    begin
      if s[i] = beginChar then
        strip := true;

      if strip then
      begin
        if s[i] = endChar then
        begin
          strip := false;
          Continue;
        end;
        // Preserve newlines during stripping
        if s[i] = #10 then
          resultBuilder.Append(s[i]); // Append newline character
      end
      else
        resultBuilder.Append(s[i]);
    end;
    Result := resultBuilder.ToString;
  finally
    resultBuilder.Free;
  end;
end;


function StripEnclosed(const s: string;const beginChar,endChar:char): string;
var
  i: integer;
  strip: boolean;
begin
  result := emptystr;
  strip := false;
  for i := 1 to length(s) do
  begin
    if s[i] = beginChar then
      strip := true;
    if strip then
    begin
      if s[i] = endChar then
      begin
        strip := false;
        Continue;
      end;
    end
    else
      result := result + s[i];
  end;
end;

function SwapCase(const s: string): string;
const ToUpperSet: TSysCharSet = ['a' .. 'z'];
const ToLowerSet: TSysCharSet = ['A' .. 'Z'];
var
  i: integer;
begin
  result := s;
  for i := 1 to length(result) do begin
      if CharInSet(result[i], ToLowerSet) then
        Inc(result[i], 32)
      else if CharInSet(result[i], ToUpperSet) then
        Dec(result[i], 32);
  end;
end;

function TitleCase(const s: string): string;
var
  i: integer;
begin
  if s = emptystr then
    result := emptystr
  else
  begin
    result := Uppercase(s[1]);
    for i := 2 to length(s) do
      if s[i - 1] = ' ' then
        result := result + Uppercase(s[i])
      else
        result := result + lowercase(s[i]);
  end;
end;

function StrToNameValue(const s: string; const aSeparator:string='='): TCatNameValue;
begin
  result.name := Before(s, aSeparator);
  result.value := After(s, aSeparator);
end;

// CONTRIBUTED ------------------------------------------------------------//

// Peter Below, 11.27.1996
function IScan(ch: Char; const s: string; fromPos: integer): integer;
var
  i: integer;
begin
  result := 0;
  for i := fromPos to length(s) do
  begin
    if s[i] = ch then
    begin
      result := i;
      break;
    end;
  end;
end;

// PB, 08.07.1997
procedure SplitString(const s: string; separator: Char;
  substrings: TStringList);
var
  i, n: integer;
begin
  if Assigned(substrings) and (length(s) > 0) then
  begin
    i := 1;
    repeat
      n := IScan(separator, s, i);
      if n = 0 then
        n := length(s) + 1;
      substrings.Add(Copy(s, i, n - i));
      i := n + 1;
    until i > length(s);
  end;
end;

// Based on an example by PB
procedure GetTextBetweenTags(const s, tag1, tag2: string; const list: TStrings;
  const includetags: Boolean = false);
var
  pScan, pEnd, pTag1, pTag2: PAnsiChar;
  foundText, searchtext: string;
begin
  searchtext := Uppercase(s);
  pTag1 := PAnsiChar(AnsiString(Uppercase(tag1)));
  pTag2 := PAnsiChar(AnsiString(Uppercase(tag2)));
  pScan := PAnsiChar(AnsiString(searchtext));
  repeat

{$IFDEF DXE2_OR_UP}
    {$IFDEF MSWINDOWS}
    pScan := System.AnsiStrings.StrPos(pScan, pTag1);
    {$ELSE}
    // FIXME
    {$ENDIF}
{$ELSE}
    pScan := StrPos(pScan, pTag1);
{$ENDIF}
    if pScan <> nil then
    begin
      inc(pScan, length(tag1));

{$IFDEF DXE2_OR_UP}
    {$IFDEF MSWINDOWS}
      pEnd := System.AnsiStrings.StrPos(pScan, pTag2);
    {$ELSE}
    // FIXME!!!
    {$ENDIF}
{$ELSE}
      pEnd := StrPos(pScan, pTag2);
{$ENDIF}
      if pEnd <> nil then
      begin
        SetString(foundText, PAnsiChar(AnsiString(s)) +
          (pScan - PAnsiChar(AnsiString(searchtext))), pEnd - pScan);
        if includetags then
          list.Add(Uppercase(tag1) + foundText + Uppercase(tag2))
        else
          list.Add(foundText);
        list.text := list.GetText;
        pScan := pEnd + length(tag2);
      end
      else
        pScan := nil;
    end;
  until pScan = nil;
end;

// Extract a list of strings enclosed within specific characters
//  Eg: ExtractEnclosed('user is: {user}','{','}') -> 'user'
function ExtractEnclosed(const s: string;const beginChar,endChar:char): string;
var
  i: integer;
  extract: boolean;
  sl:TStringList;
  exstr:string;
begin
  exstr := emptystr;
  extract := false;
  sl := TStringList.Create;
  for i := 1 to length(s) do
  begin
    if s[i] = beginChar then
      extract := true;
    if extract then
    begin
      if s[i] = endChar then
      begin
        extract := false;
        Delete(exstr, 1, 1);
        sl.Add(exstr);
        exstr := emptystr;
        Continue;
      end else
      exstr := exstr + s[i];
    end;
  end;
  result := sl.Text;
  sl.Free;
end;

// ExtractEnclosedSpecial('somestring {$name} another','{','$','}') will extract: name
function ExtractEnclosedSpecial(const s: string;const beginChar,beginChar2,endChar:char): string;
var
  i: integer;
  extract: boolean;
  sl:TStringList;
  exstr:string;
begin
  exstr := emptystr;
  extract := false;
  sl := TStringList.Create;
  for i := 1 to length(s) do
  begin
    if (s[i] = beginChar2) and (i-1 >= 1) and (s[i-1] = beginChar) then
      extract := true;
    if extract then
    begin
      if s[i] = endChar then
      begin
        extract := false;
        Delete(exstr, 1, 1);
        sl.Add(exstr);
        exstr := emptystr;
        Continue;
      end else
      exstr := exstr + s[i];
    end;
  end;
  result := sl.Text;
  sl.Free;
end;

// Based on an example by Mike Orriss
function ExtractFromString(const s, startstr, endstr: string): string;
var
  ps, pe: integer;
begin
  ps := pos(startstr, s);
  pe := pos(endstr, s);
  if (pe <= ps) or (ps = 0) then
    result := emptystr
  else
  begin
    inc(ps, length(startstr));
    result := Copy(s, ps, pe - ps);
  end;
end;

function ExtractChars(const s:string;const aSet: TSysCharSet):string;
var
  i, l: integer;
begin
  SetLength(result, length(s));
  l := 0;
  for i := 1 to length(s) do
    if (CharInSet(s[i], aSet)) then
    begin
      inc(l);
      result[l] := s[i];
    end;
  SetLength(result, l);
end;

function ExtractNumbers(const s: string): string;
begin
  result := ExtractChars(s, ['0' .. '9']);
end;

// Gets a string part of a separated string starting from left
// Based on an example from Thomas Scheffczyk
function GetToken(const aString, SepChar: String; const TokenNum: Integer): String;
var
  Token, tmpstr: String;
  StrLen, num, EndofToken: integer;
begin
  tmpstr := aString;
  StrLen := length(tmpstr);
  num := 1;
  EndofToken := StrLen;
  while ((num <= TokenNum) and (EndofToken <> 0)) do
  begin
    EndofToken := pos(SepChar, tmpstr);
    if EndofToken <> 0 then
    begin
      Token := Copy(tmpstr, 1, EndofToken - 1);
      Delete(tmpstr, 1, EndofToken);
      inc(num);
    end
    else
      Token := tmpstr;
  end;
  if num >= TokenNum then
    result := Token
  else
    result := emptystr;
end;

// Gets a string part of a separated string starting from right
// Works like GetToken() above, but this one starts from right to left
function GetTokenFromRight(const aString, SepChar: string; const TokenNum: Integer): string;
var
 revstring, revsep:string;
begin
 // Quick hack to reverse the search...
 revstring := ReverseString(astring);
 revsep := ReverseString(sepchar);
 Result := GetToken(revstring,revsep,tokennum);
 Result := ReverseString(result);
end;

function MemStreamToStr(m: TMemoryStream): String;
begin
  SetString(Result, PAnsiChar(AnsiString(m.Memory)), M.Size);
end;

function NumToWordsShort(dblValue : Double) : string;
  procedure Shorten(const s:string);
  begin
    if pos(s,result) <> 0 then
      result := before(result,s)+' '+s;
  end;
begin
  result := NumToWords(dblvalue);
  Shorten('trillion');
  Shorten('billion');
  Shorten('million');
  Shorten('thousand');
end;

// Thanks to rahsoft (http://rahsoft.blogspot.com)
function NumToWords(dblValue : Double) : String;
var
  ones : array[0..9] of String;
  teens : array[0..9] of String;
  tens : array[0..9] of String;
  thousands : array[0..4] of String;
  i, nPosition, nDigit, bAllZeros : Integer;
  strResult, strTemp, tmpBuff : String;

begin

  ones[0] := 'ZERO';
  ones[1] := 'ONE';
  ones[2] := 'TWO';
  ones[3] := 'THREE';
  ones[4] := 'FOUR';
  ones[5] := 'FIVE';
  ones[6] := 'SIX';
  ones[7] := 'SEVEN';
  ones[8] := 'EIGHT';
  ones[9] := 'NINE';

  teens[0] := 'TEN';
  teens[1] := 'ELEVEN';
  teens[2] := 'TWELVE';
  teens[3] := 'THIRTEEN';
  teens[4] := 'FOURTEEN';
  teens[5] := 'FIFTEEN';
  teens[6] := 'SIXTEEN';
  teens[7] := 'SEVENTEEN';
  teens[8] := 'EIGHTEEN';
  teens[9] := 'NINETEEN';

  tens[0] := '';
  tens[1] := ''; //TEN1

  tens[2] := 'TWENTY';
  tens[3] := 'THIRTY';
  tens[4] := 'FORTY';
  tens[5] := 'FIFTY';
  tens[6] := 'SIXTY';
  tens[7] := 'SEVENTY';
  tens[8] := 'EIGHTY';
  tens[9] := 'NINETY';

  thousands[0] := '';
  thousands[1] := 'THOUSAND';
  thousands[2] := 'MILLION';
  thousands[3] := 'BILLION';
  thousands[4] := 'TRILLION';

 try

  strResult := '';
  strTemp := IntToStr(round(dblValue));
  //Iterate through string
  For i := Length(strTemp) downTo 1 do
  begin

    //Get value of this digit
    nDigit := StrToInt(MidStr(strTemp, i, 1));
    //Get column position
    nPosition := (Length(strTemp) - i) + 1;
    //Action depends on 1's, 10's or 100's column
    //Select Case (nPosition Mod 3)
    case (nPosition Mod 3) of

    1 :  //Case 1  //'1's position
    begin
        bAllZeros := 0;
        if i = 1 Then

          tmpBuff := ones[nDigit] + ' '
        else
        if MidStr(strTemp, i - 1, 1) = '1' Then

        begin
          tmpBuff := teens[nDigit] + ' ';
        end
        else
        if nDigit > 0 Then

          tmpBuff := ones[nDigit] + ' '
        else
        begin
          //If next 10s & 100s columns are also
          //zero, then don't show 'thousands'
          bAllZeros := 1;
          if i > 1 Then

          begin
            If MidStr(strTemp, i - 1, 1) <> '0' Then
              bAllZeros := 0;
          end;
          If i > 2 Then

          begin
            If MidStr(strTemp, i - 2, 1) <> '0' Then
              bAllZeros := 0;

          End;
          tmpBuff := '';
        end;
        If (bAllZeros = 0) and (nPosition > 1) Then

          tmpBuff := tmpBuff + thousands[nPosition div 3] + ' ';

        strResult := tmpBuff + strResult;

    end;
    2 :  //Tens position
    begin

        if nDigit > 0 Then
          //if MidStr(strTemp, i - 1, 1) <> '1' Then
          strResult := tens[nDigit] +  ' ' + strResult;

    end;
    0 :  //Hundreds position

    begin

        if nDigit > 0 Then
          strResult := ones[nDigit] + ' HUNDRED ' + strResult;

    end;
    end;
  end;

  Result := lowercase(strResult);

 except
  Result := emptystr;
 end;

end;

// ------------------------------------------------------------------------//
end.
