unit CatMatch;
{
  Catarinka - Regular Expression and various other useful matching functions

  Copyright (c) 2003-2025 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  Uses the RegExpr library by Andrey V. Sorokin.
  CompareVersionNumber() function by Martin Prikryl (@martinprikryl) with small
  changes and suggestions by Tithen-Firion
  CompareVersionString() function is my extension to Martin's function

  I extended the MatchWildcard function by Arsne von Wyss and added
  a few additional functions based on it:
    MatchWildcardX(), MatchWildcardXL() and ExtractMatchX()
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils, RegExpr;
{$ELSE}
  Classes, SysUtils, RegExpr;
{$ENDIF}

type
  TCatRegExpr = TRegExpr;

const
 cWCXLVariable = '%WCXL%';
 VERSTG_RELEASE = 5;
 VERSTG_RELEASECANDIDATE = 4;
 VERSTG_BETA = 3;
 VERSTG_ALPHA = 2;
 VERSTG_PREALPHA = 1;

type
 TVersionParts = record
   originalstring: string;
   version: string;
   ext:string;
   extnum: string;
   hasletter: boolean;
   stage:integer;
 end;

function RegExpFind(const s, re: string): string;
function RegExpReplace(const s, re, sreplacement: string): string; overload;
function RegExpReplace(const s, re: string; refunc: TRegExprReplaceFunction): string; overload;

function AnyEmptyStr(const Values: array of string): Boolean;
function AnyTrue(const Values: array of Boolean): Boolean;
function AnyFalse(const Values: array of Boolean): Boolean;
function CatCaseWildOf(const s: string; labels: array of string;
  const casesensitive: Boolean = true): integer;
function CatCaseWildXOf(const s: string; labels: array of string;
  const casesensitive: Boolean = true): integer;  
function CatMatchSignature(const sigpattern, s: string): boolean;
function CountMatchingWords(const str1, str2: string): Integer;
function CompareVersionNumber(const version1, version2: string;
  const Silent:boolean=true): Integer;
function CompareVersionString(const version1, version2: string;
  const Silent:boolean=true): Integer;
function CrackVersionString(const ver: string): TVersionParts;
function ExtractMatchX(const s,mask:string;const sep:string=','):string;
function IsWildCardString(const s:string):boolean;
function IsValidEmail(email: string): boolean;
function ExtractVersionFromString(s:string):string;
function MatchStrInSepStr(const s,tags:string;separator:string=','):boolean;
function MatchStrInSepStrAlt(const s,tags:string;separator:string=','):boolean;
function MatchStrEndInSepStr(const s,tags:string;separator:string=','):boolean;
function MatchStringLUT(const sub, s:string):boolean;
function MatchVersion(curver, vercheck:string):boolean;
function MatchVersionRange(curver, vercheck:string):boolean;
function MatchVersionEx(curver, vercheck:string):boolean;
function MatchWildcard(s, Mask: string; IgnoreCase: Boolean = false): Boolean;
function MatchWildcardX(s, Mask: string; IgnoreCase: Boolean = false): Boolean;
function MatchWildcardXL(s, Mask: string; sarr:array of string;
  IgnoreCase: Boolean = false): Boolean;

implementation

uses CatStrings, CatStringLoop;

// Takes an array of string values and returns True if any one of them is empty
function AnyEmptyStr(const Values: array of string): Boolean;
var
  i: Integer;
begin
  Result := False; // Default to False
  for i := Low(Values) to High(Values) do
  begin
    if Values[i] = emptystr then
    begin
      Result := True;
      Exit; // Exit early if a True value is found
    end;
  end;
end;

// Takes an array of boolean values and returns True if any one of them is True
function AnyTrue(const Values: array of Boolean): Boolean;
var
  i: Integer;
begin
  Result := False; // Default to False
  for i := Low(Values) to High(Values) do
  begin
    if Values[i] = True then
    begin
      Result := True;
      Exit; // Exit early if a True value is found
    end;
  end;
end;

// Takes an array of boolean values and returns True if any one of them is False
function AnyFalse(const Values: array of Boolean): Boolean;
var
  i: Integer;
begin
  Result := False; // Default to False
  for i := Low(Values) to High(Values) do
  begin
    if Values[i] = False then
    begin
      Result := True;
      Exit; // Exit early if a True value is found
    end;
  end;
end;

{ 
  Case Statement with wildcard matching
  Usage examples:
  case CatCaseWildOf(inputstring, ['1:do?', '2:c*', '3:b??d']) of
   1: result := ...
   2: result := ...
   3: result := ...
  else
   result := 'nomatch';  
  end;
  case CatCaseWildOf(a, ['1:http://*', '2:https://*']) of
   1: result := 'http URL';
   2: result := 'https URL';
  else
   result := 'nomatch';
  end;  
  
  
  Check CatStrings.pas for non-wildcard version of this function
}
function CatCaseWildOf(const s: string; labels: array of string;
  const casesensitive: Boolean = true): integer;
var
  i: integer;
begin
  result := -1; // label not found
  for i := low(labels) to high(labels) do
  begin
    if MatchWildcard(s, after(labels[i],':'), not casesensitive) = true then
      result := StrToIntDef(before(labels[i],':'), -1);
    if result <> -1 then
      break;
  end;
end;

function CatCaseWildXOf(const s: string; labels: array of string;
  const casesensitive: Boolean = true): integer;
var
  i: integer;
begin
  result := -1; // label not found
  for i := low(labels) to high(labels) do
  begin
    if MatchWildcardX(s, after(labels[i],':'), not casesensitive) = true then
      result := StrToIntDef(before(labels[i],':'), -1);
    if result <> -1 then
      break;
  end;
end;

// Compare each word from the first string with words from the second string and
// returns the matching count
function CountMatchingWords(const str1, str2: string): Integer;
var
  words1, words2: TStringList;
  i, j: Integer;
begin
  // Split the first string into words
  words1 := TStringList.Create;
  try
    words1.Delimiter := ' ';
    words1.DelimitedText := str1;

    // Split the second string into words
    words2 := TStringList.Create;
    try
      words2.Delimiter := ' ';
      words2.DelimitedText := str2;

      Result := 0;

      // Compare each word from the first string with words from the second string
      for i := 0 to words1.Count - 1 do
      begin
        for j := 0 to words2.Count - 1 do
        begin
          if SameText(words1[i], words2[j]) then
          begin
            Inc(Result);
            Break; // Move to the next word from the first string
          end;
        end;
      end;
    finally
      words2.Free;
    end;
  finally
    words1.Free;
  end;
end;

function RegExpReplace(const s, re, sreplacement: string): string;
var
  r: TRegExpr;
begin
  r := TRegExpr.Create;
  try
    r.Expression := re;
    result := r.Replace(s, sreplacement, true);
  finally
    r.Free;
  end;
end;

function RegExpReplace(const s, re: string; refunc: TRegExprReplaceFunction): string;
var
  r: TRegExpr;
begin
  r := TRegExpr.Create;
  try
    r.Expression := re;
    result := r.ReplaceEx(s, refunc);
  finally
    r.Free;
  end;
end;

function RegExpFind(const s, re: string): string;
var
  r: TRegExpr;
begin
  result := emptystr;
  r := TRegExpr.Create;
  try
    r.Expression := re;
    if r.Exec(s) then
      repeat
        result := result + r.Match[0] + ', ';
      until not r.ExecNext;
  finally
    r.Free;
  end;
end;

// Allows to match a string using different methods:
// [somestring] -- case sensitive contains
// wild:[somestring] -- case sensitive wild match
// wildx:[somestring] -- case sensitive wild match extended
// regexp:[expression] -- regular expression
// icase:[somestring] -- case insensitive contains
// icase:wild:[somestring] -- case insensitive wild match (* and ?)
function CatMatchSignature(const sigpattern, s: string): boolean;
const
  cIgnoreCase = 'icase:';
  cReEx = 'regexp:';
  cWild = 'wild:';
  cWildX = 'wildx';
var
  sig, content, pat: string;
begin
  sig := sigpattern;
  content := s;
  if BeginsWith(sig,cIgnoreCase) then
  begin
    sig := after(sig, cIgnoreCase);
    sig := lowercase(sig);
    content := lowercase(content);
  end;
  if BeginsWith(sig,cReEx) then
  begin
    pat := after(sig, cReEx);
    result := (RegExpFind(content, pat) <> emptystr);
  end
  else
  if BeginsWith(sig,cWild) then
  begin
    pat := after(sig, cWild);
    result := MatchWildcard(content, pat);
  end
  else
  if BeginsWith(sig,cWildx) then
  begin
    pat := after(sig, cWildx);
    result := MatchWildcardX(content, pat);
  end
  else
  begin
    result := (pos(sig, content) <> 0);
  end;
end;

// Thanks ffert2907
function IsValidEmail(email: string): boolean;
const
  charslist = ['_', '-', '.', '0'..'9', 'A'..'Z', 'a'..'z'];
var
  Arobasc, lastpoint : boolean;
  i, n : integer;
  c : char;
begin
  n := Length(email);
  i := 1;
  Arobasc := false;
  lastpoint := false;
  result := true;
  while (i <= n) do begin
    c := email[i];
    if c = '@' then
    begin
      if Arobasc then  // Only 1 Arobasc
      begin
        result := false;
        exit;
      end;
      Arobasc := true;
    end
    else if (c = '.') and Arobasc then  // at least 1 . after arobasc
    begin
      lastpoint := true;
    end
    else if not(c in charslist) then  // valid chars
    begin
      result := false;
      exit;
    end;
    inc(i);
  end;
  if not(lastpoint) or (email[n] = '.')then  // not finish by . and have a . after arobasc
    result := false;
end;

function ExtractVersionFromString(s:string):string;
begin
  result := RegExpFind(s, '\d+(\.\d+)+');
  if pos(',', result) <> 0 then
  result := before(result, ',');
end;

function NextIntFromVersion(var S: string;const Silent:boolean=true): Integer;
var
  P: integer;
begin
    P := Pos('.', S);
    if P > 0 then
    begin
      if silent = true then
        result := StrToIntDef(Copy(S, 1, P - 1), 0)
      else
        result := StrToInt(Copy(S, 1, P - 1));
      Delete(S, 1, P);
    end
      else
    if S <> emptystr then
    begin
      if silent = true then
      result := StrToIntDef(S, 0) else
      result := StrToInt(S);
      S := emptystr;
    end
      else
    begin
      result := 0;
    end;
end;

{
Compares version numbers
 Returns 0, if the versions are equal.
 Returns -1, if the V1 is older than the V2.
 Returns 1, if the V1 is newer than the V2.
 1.12 is considered newer than 1.1.
 1.1 is considered the same as 1.1.0.
 If silent is false, throws an exception, when a version is syntactically invalid (only digits and dots are allowed).
}
// Thanks Martin Prikryl (@martinprikryl) and Tithen-Firion
function CompareVersionNumber(const version1, version2: string;
  const Silent:boolean=true): Integer;
var
  v1, v2:string;
  N1, N2: Integer;
begin
  Result := 0;
  v1 := version1;
  v2 := version2;
  while (Result = 0) and ((V1 <> EmptyStr) or (V2 <> EmptyStr)) do
  begin
    N1 := NextIntFromVersion(V1);
    N2 := NextIntFromVersion(V2);
    if N1 < N2 then Result := -1
      else
    if N1 > N2 then Result := 1;
  end;
end;

function GetVersionStage(const ver:string):integer;
var lv:string;
begin
  lv := lowercase(ver);
  result := VERSTG_RELEASE;
  if pos('rc', lv) <> 0 then
    result := VERSTG_RELEASECANDIDATE;
  if pos('pre', lv) <> 0 then // pre-release
    result := VERSTG_RELEASECANDIDATE;
  if pos('beta', lv) <> 0 then
    result := VERSTG_BETA;
  if pos('preview', lv) <> 0 then
    result := VERSTG_BETA;
  if pos('alpha', lv) <> 0 then
    result := VERSTG_ALPHA;
  // pre-alpha
  if (pos('pre', lv) <> 0) and (pos('alpha', lv) <> 0) then
    result := VERSTG_PREALPHA;
end;

function CrackVersionString(const ver: string): TVersionParts;
begin
  result.ext := emptystr;
  result.extnum := emptystr;
  result.originalstring := ver;
  result.version := ver;
  result.hasletter := false;
  result.stage := VERSTG_RELEASE;
  if IsAlphaNumeric(ver) then begin
    result.version := ExtractVersionFromString(ver);
    result.ext := After(ver, result.version);
    // handles versions like: 1.0.0b-somename and 1.0-beta1
    if pos('-', result.ext) <> 0 then begin
      if beginswith(result.ext,'-') = true then
      result.ext := After(result.ext, '-')
      else
      result.ext := Before(result.ext, '-');
    end;
    result.extnum := ExtractNumbers(result.ext);
    result.hasletter := (length(result.ext) = 1) and (result.ext[1] in ['a'..'z','A'..'Z']);
    result.stage := GetVersionStage(ver);
  end;
end;


// This function I wrote extends the CompareVersion function by martinprikryl (above)
// to compare versions that contain alphanumeric extensions like:
// v1.0 versus V1.1
// 1.0 RC1 versus 1.0 RC2
// 1.0a versus 1.0B or 1.0b
// 1.5.0-rc1 versus 1.5.0-beta1 and similar
function CompareVersionString(const version1, version2: string;
  const Silent:boolean=true): Integer;
var
 v1,v2: TVersionParts;
 function CompareVersionAlphaExtension(curver, vercheck:string):integer;
 begin
   result := 0;
    if curver < vercheck  then
        result := -1
    else if curver > vercheck  then
        result := 1;
 end;
begin
  v1 := CrackVersionString(version1);
  v2 := CrackVersionString(version2);
  result := CompareVersionNumber(v1.version, v2.version, silent);

  // If versions are identical and extended version information is available
  // conclude by comparing numbers from extension part
  if result = 0 then begin
    // Compares case like 1.5.0-beta.10 versus 1.5.0-beta.11
    if (v1.ext <> emptystr) and (v2.ext <> emptystr) then
    result := CompareVersionNumber(v1.extnum, v2.extnum, silent);

    // Assign letter if required and compare cases like
    //1.0 versus 1.0a
    //1.0.1a versus 1.0.1b
    if (v1.hasletter = true) and (v2.ext = emptystr) then
       result := 1;
    if (v2.hasletter = true) and (v1.ext = emptystr) then
       result := -1;
    if (length(v1.ext) =1) and (length(v2.ext) = 1) then
      result := CompareVersionAlphaExtension(lowercase(v1.ext), lowercase(v2.ext));
    // if the development stage is different, the result must be the stage comparison
    if (v1.stage <> v2.stage) then
      result := CompareVersionNumber(IntToStr(v1.stage), IntToStr(v2.stage));
  end;
end;

// Matches the end of a string against a separated string (comma-based if separator is ommited)
function MatchStrEndInSepStr(const s,tags:string;separator:string=','):boolean;
var
  i,c:integer;
  tag:string;
begin
  c := occurs(separator, tags);
  result := false;
  if c = 0 then begin
    if endswith(s, tags) = true then
      result := true;
  end else begin
    for i := 0 to c do begin
      tag := gettoken(tags, separator, i+1);
      if endswith(s, tag) = true then begin
        result := true;
        break;
      end;
    end;
  end;
end;

// Matches a string against a separated string (comma-based if separator is ommited)
function MatchStrInSepStr(const s,tags:string;separator:string=','):boolean;
var
  i,c:integer;
  tag:string;
begin
  c := occurs(separator, tags);
  result := false;
  if c = 0 then begin
    if s = tags then
      result := true;
  end else begin
    for i := 0 to c do begin
      tag := gettoken(tags, separator, i+1);
      if s = tag then begin
        result := true;
        break;
      end;
    end;
  end;
end;

// Matches a string against a separated string (comma-based if separator is ommited)
// this one has been implemented using TSepStringLoop
function MatchStrInSepStrAlt(const s,tags:string;separator:string=','):boolean;
var Tag: TSepStringLoop;
begin
  result := false;
  if tags <> emptystr then
  begin
      Tag := TSepStringLoop.Create(tags, separator);
      while Tag.Found do
      begin
        if Tag.Current = s then
          result := true;
      end;
      Tag.Free;
  end;
end;

// Matches a string in lower, upper and title case formats
function MatchStringLUT(const sub, s:string):boolean;
begin
  result := false;
  if pos(lowercase(sub), s) <> 0 then
    result := true
  else if pos(uppercase(sub), s) <> 0 then
    result := true
  else if pos(titlecase(sub), s) <> 0 then
    result := true;
end;

function IsWildCardString(const s:string):boolean;
begin
  result := ContainsAnyOfChars(s, ['*','?']);
end;

// expects expression like:
//  <3.4.0
//  <=3.4.0
// when using just the equal sign, can be followed by a wildcard, ex:
//  =* (matches any version)
//  =3.*
//  =3.1.??
function MatchVersion(curver, vercheck:string):boolean;
var
  outver: string;
  compres: integer;
begin
  result := false;
  compres := -1; // avoid compiler message
  outver := vercheck; //ExtractVersionFromString(vercheck);
    //if beginswith(vercheck, '=') = false then
    //  compres := CompareVersionNumber(curver, outver);
    if beginswith(vercheck, '>=') then begin
        outver := after(vercheck, '>=');
        compres := CompareVersionString(curver, outver);
        if (compres = 1) or (compres = 0) then
        result := true;
    end else
    if beginswith(vercheck, '<=') then begin
        outver := after(vercheck, '<=');
        compres := CompareVersionString(curver, outver);
        if (compres = -1) or (compres = 0) then
        result := true;
    end else
    if beginswith(vercheck, '<') then begin
        outver := after(vercheck, '<');
        compres := CompareVersionString(curver, outver);
        if (compres = -1) then
        result := true;
    end else
    if beginswith(vercheck, '>') then begin
        outver := after(vercheck, '>');
        compres := CompareVersionString(curver, outver);
        if (compres = 1) then
        result := true;
    end else
    if beginswith(vercheck, '=') then begin
        outver := after(vercheck, '=');
        if IsWildCardString(outver) = false then begin
          compres := CompareVersionString(curver, outver);
          if (compres = 0) then
          result := true;
        end else begin
          if matchwildcard(curver, outver) = true  then
          result := true;
        end;
    end;
end;

// expects multiple version checks separated by spaces like:
// >=1.4.2 <1.6.2
// will return true if all are matched, false otherwise
function MatchVersionRange(curver, vercheck:string):boolean;
var
  s: TSepStringLoop;
begin
  result := true;
  s := TSepStringLoop.Create(vercheck,' ');
  while s.Found do begin
    if MatchVersion(curver,s.Current) = false then
        result := false;
  end;
  s.Free;
end;

// expects version check list separated by commas like:
// <1.12.2,>=1.12.3 <2.2.2,>=2.2.3 <3.0.0
// must match at least one of the checks to return true
// wildcards can be used when using just equal sign, ex:
// usage examples:
// MatchVersionEx('2.2.3','<1.12.2,>=1.12.3 <2.2.2,>=2.2.3 <3.0.0') -> true
// MatchVersionEx('3.0.1','<1.12.2,>=1.12.3 <2.2.2,>=2.2.3 <3.0.0') -> false
// MatchVersionEx('2.1.11','=2.1.??,=3.*') -> true
function MatchVersionEx(curver, vercheck:string):boolean;
var
  s: TSepStringLoop;
begin
  result := false;
  s := TSepStringLoop.Create(vercheck,',');
  while s.Found do begin
    if MatchVersionRange(curver,s.Current) = true then begin
        result := true;
        s.Stop;
    end;
  end;
  s.Free;
end;

function MatchWildcard_Pos(const substr, s: string; Start: integer): integer;
var
  i, J, len: integer;
begin
  len := length(substr);
  if len = 0 then
  begin
    result := 1;
    Exit;
  end;
  for i := Start to Succ(length(s) - len) do
  begin
    J := 1;
    while J <= len do
    begin
      if not((substr[J] = '?') or (substr[J] = s[Pred(i + J)])) then
        break;
      inc(J);
    end;
    if J > len then
    begin
      result := i;
      Exit;
    end;
  end;
  result := 0;
end;

{
  This function takes two strings and compares them. The first string
  can be anything, but should not contain pattern characters (* or ?).
  The pattern string can have as many of these pattern characters as you want.
  For example: MatchStrings('Pascal','*as*') would return True.

  Contributed by Arsne von Wyss (1999)
}
function MatchWildcard(s, Mask: string; IgnoreCase: Boolean = false): Boolean;
const
  WildSize = 0; { minimal number of characters representing a "*" }
var
  Min, Max, At, MaskSTart, MaskEnd: integer;
  T: string;
begin
  if IgnoreCase then
  begin
    s := uppercase(s);
    mask := uppercase(mask);
  end;
  s := s + #0;
  Mask := Mask + #0;
  Min := 1;
  Max := 1;
  MaskEnd := 0;
  while length(Mask) >= MaskEnd do
  begin
    MaskSTart := MaskEnd + 1;
    repeat
      inc(MaskEnd);
    until (MaskEnd > length(Mask)) or (Mask[MaskEnd] = '*');
    T := Copy(Mask, MaskSTart, MaskEnd - MaskSTart);
    At := MatchWildcard_Pos(T, s, Min);
    if (At = 0) or (At > Max) then
    begin
      result := false;
      Exit;
    end;
    Min := At + length(T) + WildSize;
    Max := length(s);
  end;
  result := true;
end;

function MatchWildcardX_Pos(const substr, s: string; Start: integer): integer;
var
  i, J, len: integer;
begin
  len := length(substr);
  if len = 0 then
  begin
    result := 1;
    Exit;
  end;
  for i := Start to Succ(length(s) - len) do
  begin
    J := 1;
    while J <= len do
    begin
      if (substr[J] = '#') and (IsInteger(s[Pred(i + J)]) <> true) then
        break;
      if (substr[J] = '¿') and (IsAlpha(s[Pred(i + J)]) <> true) then
        break;
      if (substr[J] = '¡') and (IsAlphaOrNumeric(s[Pred(i + J)]) <> true) then
        break;
      if not(ContainsAnyOfChars(substr[J], ['?','#','¿','¡']) or (substr[J] = s[Pred(i + J)])) then
        break;
      inc(J);
    end;
    if J > len then
    begin
      result := i;
      Exit;
    end;
  end;
  result := 0;
end;

// Extended version of MatchWildcard() function that allows the following
// additional chars to be used as part of the mask:
// # (hash) to match a number
// ¿ (inverted interrogation) to match a letter (A..Z, a..z)
// ¡ (inverted exclamation) to match alpha or numeric character (A..Z, a..z, 0..9)
// todo: implement cedilla (¸) and macron (¯) as well as ™ © ® ¤ × ÷ ° § ¶ † ‡ ‰ ƒ ª ¬ ½ ¼ « » ¦ ß ± ²
// by Felipe Daragon
function MatchWildcardX(s, Mask: string; IgnoreCase: Boolean = false): Boolean;
const
  WildSize = 0; { minimal number of characters representing a "*" }
var
  Min, Max, At, MaskSTart, MaskEnd: integer;
  T: string;
begin
  if IgnoreCase then
  begin
    s := uppercase(s);
    mask := uppercase(mask);
  end;
  s := s + #0;
  Mask := Mask + #0;
  Min := 1;
  Max := 1;
  MaskEnd := 0;
  while length(Mask) >= MaskEnd do
  begin
    MaskSTart := MaskEnd + 1;
    repeat
      inc(MaskEnd);
    until (MaskEnd > length(Mask)) or (Mask[MaskEnd] = '*');
    T := Copy(Mask, MaskSTart, MaskEnd - MaskSTart);
    At := MatchWildcardX_Pos(T, s, Min);
    if (At = 0) or (At > Max) then
    begin
      result := false;
      Exit;
    end;
    Min := At + length(T) + WildSize;
    Max := length(s);
  end;
  result := true;
end;

function MatchWildcardXL(s, Mask: string; sarr:array of string;
  IgnoreCase: Boolean = false): Boolean;
var
  i: integer;
  maskmut:string;
begin
  result := false;
  for i := low(sarr) to high(sarr) do
  begin
    maskmut := replacestr(mask,cWCXLVariable,sarr[i]);
    if MatchWildcardX(s, maskmut, ignorecase) = true then begin
      result := true;
      break;
    end;
  end;
end;

// Loops through a comma-separated string and returns the first match
// or an empty string if there is no match
// Example:
// ExtractMatchX('test,11/11/2020,123','##/##/##')
//  will return 11/11/2020
function ExtractMatchX(const s,mask:string;const sep:string=','):string;
var slp:TSepStringLoop;
begin
  result := emptystr;
  slp := TSepstringLoop.Create(s,sep);
  while slp.Found do begin
    if MatchWildcardX(slp.current,mask) = true then begin
      result := slp.Current;
      break;
    end;
  end;
  slp.Free;
end;

// ------------------------------------------------------------------------//
end.
