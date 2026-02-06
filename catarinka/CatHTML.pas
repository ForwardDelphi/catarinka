unit CatHTML;
{
  Catarinka - HTML & URL related functions
  Copyright (c) 2003-2025 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  ColorToHTMLColor function by Ralf Mimoun
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils, Vcl.Graphics, CatStringLoop;
{$ELSE}
  Classes, SysUtils, Graphics, CatStringLoop;
{$ENDIF}

type
  TNamedWebColor = record
    Name: string;
    Color: TColor;
  end;

const
  NamedWebColors: array[0..139] of TNamedWebColor = (
    (Name: 'Fuchsia'; Color: $00ff00ff),
    (Name: 'Aqua'; Color: $00ffff00),
    (Name: 'Snow'; Color: $FAFAFF),
    (Name: 'FloralWhite'; Color: $F0FAFF),
    (Name: 'LavenderBlush'; Color: $F5F0FF),
    (Name: 'OldLace'; Color: $E6F5FD),
    (Name: 'Ivory'; Color: $F0FFFF),
    (Name: 'CornSilk'; Color: $DCF8FF),
    (Name: 'Beige'; Color: $DCF5F5),
    (Name: 'AntiqueWhite'; Color: $D7EBFA),
    (Name: 'Wheat'; Color: $B3DEF5),
    (Name: 'AliceBlue'; Color: $FFF8F0),
    (Name: 'GhostWhite'; Color: $FFF8F8),
    (Name: 'Lavender'; Color: $FAE6E6),
    (Name: 'Seashell'; Color: $EEF5FF),
    (Name: 'LightYellow'; Color: $E0FFFF),
    (Name: 'PapayaWhip'; Color: $D5EFFF),
    (Name: 'NavajoWhite'; Color: $ADDEFF),
    (Name: 'Moccasin'; Color: $B5E4FF),
    (Name: 'Burlywood'; Color: $87B8DE),
    (Name: 'Azure'; Color: $FFFFF0),
    (Name: 'Mintcream'; Color: $FAFFF5),
    (Name: 'Honeydew'; Color: $F0FFF0),
    (Name: 'Linen'; Color: $FAF0E6),
    (Name: 'LemonChiffon'; Color: $CDFAFF),
    (Name: 'BlanchedAlmond'; Color: $CDEBFF),
    (Name: 'Bisque'; Color: $C4E4FF),
    (Name: 'PeachPuff'; Color: $B9DAFF),
    (Name: 'Tan'; Color: $8CB4D2),
    (Name: 'Yellow'; Color: $00FFFF),
    (Name: 'DarkOrange'; Color: $008CFF),
    (Name: 'Red'; Color: $0000FF),
    (Name: 'DarkRed'; Color: $00008B),
    (Name: 'Maroon'; Color: $000080),
    (Name: 'IndianRed'; Color: $5C5CCD),
    (Name: 'Salmon'; Color: $7280FA),
    (Name: 'Coral'; Color: $507FFF),
    (Name: 'Gold'; Color: $00D7FF),
    (Name: 'Tomato'; Color: $4763FF),
    (Name: 'Crimson'; Color: $3C14DC),
    (Name: 'Brown'; Color: $2A2AA5),
    (Name: 'Chocolate'; Color: $1E69D2),
    (Name: 'SandyBrown'; Color: $60A4F4),
    (Name: 'LightSalmon'; Color: $7AA0FF),
    (Name: 'LightCoral'; Color: $8080F0),
    (Name: 'Orange'; Color: $00A5FF),
    (Name: 'OrangeRed'; Color: $0045FF),
    (Name: 'Firebrick'; Color: $2222B2),
    (Name: 'SaddleBrown'; Color: $13458B),
    (Name: 'Sienna'; Color: $2D52A0),
    (Name: 'Peru'; Color: $3F85CD),
    (Name: 'DarkSalmon'; Color: $7A96E9),
    (Name: 'RosyBrown'; Color: $8F8FBC),
    (Name: 'PaleGoldenrod'; Color: $AAE8EE),
    (Name: 'LightGoldenrodYellow'; Color: $D2FAFA),
    (Name: 'Olive'; Color: $008080),
    (Name: 'ForestGreen'; Color: $228B22),
    (Name: 'GreenYellow'; Color: $2FFFAD),
    (Name: 'Chartreuse'; Color: $00FF7F),
    (Name: 'LightGreen'; Color: $90EE90),
    (Name: 'Aquamarine'; Color: $D4FF7F),
    (Name: 'SeaGreen'; Color: $578B2E),
    (Name: 'GoldenRod'; Color: $20A5DA),
    (Name: 'Khaki'; Color: $8CE6F0),
    (Name: 'OliveDrab'; Color: $238E6B),
    (Name: 'Green'; Color: $008000),
    (Name: 'YellowGreen'; Color: $32CD9A),
    (Name: 'LawnGreen'; Color: $00FC7C),
    (Name: 'PaleGreen'; Color: $98FB98),
    (Name: 'MediumAquamarine'; Color: $AACD66),
    (Name: 'MediumSeaGreen'; Color: $71B33C),
    (Name: 'DarkGoldenRod'; Color: $0B86B8),
    (Name: 'DarkKhaki'; Color: $6BB7BD),
    (Name: 'DarkOliveGreen'; Color: $2F6B55),
    (Name: 'Darkgreen'; Color: $006400),
    (Name: 'LimeGreen'; Color: $32CD32),
    (Name: 'Lime'; Color: $00FF00),
    (Name: 'SpringGreen'; Color: $7FFF00),
    (Name: 'MediumSpringGreen'; Color: $9AFA00),
    (Name: 'DarkSeaGreen'; Color: $8FBC8F),
    (Name: 'LightSeaGreen'; Color: $AAB220),
    (Name: 'PaleTurquoise'; Color: $EEEEAF),
    (Name: 'LightCyan'; Color: $FFFFE0),
    (Name: 'LightBlue'; Color: $E6D8AD),
    (Name: 'LightSkyBlue'; Color: $FACE87),
    (Name: 'CornFlowerBlue'; Color: $ED9564),
    (Name: 'DarkBlue'; Color: $8B0000),
    (Name: 'Indigo'; Color: $82004B),
    (Name: 'MediumTurquoise'; Color: $CCD148),
    (Name: 'Turquoise'; Color: $D0E040),
    (Name: 'Cyan'; Color: $FFFF00),
    (Name: 'PowderBlue'; Color: $E6E0B0),
    (Name: 'SkyBlue'; Color: $EBCE87),
    (Name: 'RoyalBlue'; Color: $E16941),
    (Name: 'MediumBlue'; Color: $CD0000),
    (Name: 'MidnightBlue'; Color: $701919),
    (Name: 'DarkTurquoise'; Color: $D1CE00),
    (Name: 'CadetBlue'; Color: $A09E5F),
    (Name: 'DarkCyan'; Color: $8B8B00),
    (Name: 'Teal'; Color: $808000),
    (Name: 'DeepskyBlue'; Color: $FFBF00),
    (Name: 'DodgerBlue'; Color: $FF901E),
    (Name: 'Blue'; Color: $FF0000),
    (Name: 'Navy'; Color: $800000),
    (Name: 'DarkViolet'; Color: $D30094),
    (Name: 'DarkOrchid'; Color: $CC3299),
    (Name: 'Magenta'; Color: $FF00FF),
    (Name: 'DarkMagenta'; Color: $8B008B),
    (Name: 'MediumVioletRed'; Color: $8515C7),
    (Name: 'PaleVioletRed'; Color: $9370DB),
    (Name: 'BlueViolet'; Color: $E22B8A),
    (Name: 'MediumOrchid'; Color: $D355BA),
    (Name: 'MediumPurple'; Color: $DB7093),
    (Name: 'Purple'; Color: $800080),
    (Name: 'DeepPink'; Color: $9314FF),
    (Name: 'LightPink'; Color: $C1B6FF),
    (Name: 'Violet'; Color: $EE82EE),
    (Name: 'Orchid'; Color: $D670DA),
    (Name: 'Plum'; Color: $DDA0DD),
    (Name: 'Thistle'; Color: $D8BFD8),
    (Name: 'HotPink'; Color: $B469FF),
    (Name: 'Pink'; Color: $CBC0FF),
    (Name: 'LightSteelBlue'; Color: $DEC4B0),
    (Name: 'MediumSlateBlue'; Color: $EE687B),
    (Name: 'LightSlateGray'; Color: $998877),
    (Name: 'White'; Color: $FFFFFF),
    (Name: 'Lightgrey'; Color: $D3D3D3),
    (Name: 'Gray'; Color: $808080),
    (Name: 'SteelBlue'; Color: $B48246),
    (Name: 'SlateBlue'; Color: $CD5A6A),
    (Name: 'SlateGray'; Color: $908070),
    (Name: 'WhiteSmoke'; Color: $F5F5F5),
    (Name: 'Silver'; Color: $C0C0C0),
    (Name: 'DimGray'; Color: $696969),
    (Name: 'MistyRose'; Color: $E1E4FF),
    (Name: 'DarkSlateBlue'; Color: $8B3D48),
    (Name: 'DarkSlategray'; Color: $4F4F2F),
    (Name: 'Gainsboro'; Color: $DCDCDC),
    (Name: 'DarkGray'; Color: $A9A9A9),
    (Name: 'Black'; Color: $000000)
  );

type
  TURLParts = record
    Filename: string;
    Fileext: string;
    Host: string;
    Path: string;
    Port: integer;
    Protocol: string;
    ProtocolHTTP: boolean;
    URL: string;
  end;


{
  TURLParamsLoop - Loops through URL params
  Expects a URL to be loaded with LoadFromURL()
  Example:
  obj.LoadFromURL('http://somehost/index.php?url=1&id=2');
}

type
  TURLParamsLoop = class(TStringLoop)
  private
    fParamName:string;
    fParamNameLower:string;
    fParamValue:string;
  public
    function Found: boolean;
    procedure LoadFromURL(const aURL:string);
    procedure LoadFromPOSTData(const PostData:string);
    property ParamName:string read fParamName;
    property ParamNameLower:string read fParamNameLower;
    property ParamValue:string read fParamValue;
  end;

// HTML functions
function BeginsWithHTTPProto(const url: string): boolean;
function BoolToDisplayState(const b: boolean): string;
function ColorToHTMLColor(const Color: TColor): string;
function DequoteHTMLAttribValue(const s: string): string;
function ExtractFromTag(const s, tag: string): string;
function HtmlColorToColor(const Color: string): TColor;
function HtmlEntityEncode(const s: string): string;
function HtmlEntityDecode(const s: string): string;
function HtmlEscape(const s: string): string;
function HtmlUnescape(const s: string): string;
function StripHTML(const s: string;PreserveLinesNumber:boolean=false): string;
function StripPHPCode(const s: string): string;

// JSON functions
function JSONStringEscape(const s: string): string;

// URL functions
function CrackURL(const url: string): TURLParts;
function ChangeUrlPath(const url, newpath: string): string;
function RemoveUrlPath(const url: string): string;
function RemoveUrlFilename(const URL:string;ExtensionRequired:boolean=false):string;
function RemoveUrlFileExt(const url: string; KeepParams: Boolean = False): string;
function ExtractUrlFileExt(const url: string): string;
function ExtractUrlFileName(const url: string): string;
function ExtractUrlHost(const url: string): string;
function ExtractUrlPath(const url: string;
  const includeparams: boolean = true): string;
function ExtractUrlPort(const url: string): integer;
function FileUrlToFilename(const url: string): string;
function GenerateURL(const Host: string; const Port: integer): string;
function URLDecode(const s: string): string;
function URLEncode(const s: string; plus: boolean = false;
  const preserve: TSysCharSet = ['0' .. '9', 'A' .. 'Z', 'a' .. 'z',
  ' ','.']): string;
function URLEncode_U(const s: string; plus: boolean = false;
  const preserve: TSysCharSet = ['0' .. '9', 'A' .. 'Z', 'a' .. 'z',
  ' ','.']): string;
function URLEncodeFull(const s: string): string;
function URLPathTitleCase(const s: string): string;

implementation

uses
  CatStrings, CatJSON, CatMatch, CatEntities;
  
function BeginsWithHTTPProto(const url: string): boolean;
begin
  result := false;
  if BeginsWith(url, ['http:', 'https:'], true) then
    result := true;
end;  

function BoolToDisplayState(const b: boolean): string;
begin
  if b then
    result := 'block'
  else
    result := 'none';
end;

// By Ralf Mimoun
function ColorToHTMLColor(const Color: TColor): string;
var
  cl: LongInt;
begin
  cl := ColorToRGB(Color);
  result := format('#%6.6x', [((cl and $FF0000) shr 16) + ((cl and $00FF00)) +
    ((cl and $0000FF) shl 16)]);
end;

function ChangeURLPath(const url, newpath: string): string;
var
  oldpath, reppath, aurl: string;
begin
  aurl := url;
  if lastchar(aurl) <> '/' then
  aurl := aurl+'/';
  oldpath := '/' + ExtractUrlPath(aurl);
  reppath := newpath;
  result := replacestr(aurl + ' ', oldpath + ' ', reppath);
end;

function CrackURL(const url: string): TURLParts;
begin
  result.URL := trim(url);
  result.Fileext := ExtractUrlFileExt(result.URL);
  result.Filename := ExtractUrlFileName(result.URL);
  result.Host := ExtractUrlHost(result.URL);
  result.Path := ExtractUrlPath(result.URL);
  result.Port := ExtractUrlPort(result.URL);
  result.Protocol := before(result.URL, ':');
  result.ProtocolHTTP := BeginsWithHttpProto(result.URL);
end;

// A special dequote for HTML attribute values
// "value" -> value, 'value' -> value
function DequoteHTMLAttribValue(const s: string): string;
const
  cQuotes = ['"', ''''];
var
  i: integer;
  last: char;
  firstisquote: boolean;
begin
  result := s;
  i := length(s);
  if i = 0 then
    Exit;
  last := LastChar(s);
  firstisquote := CharInSet(s[1], cQuotes);
  if (firstisquote = true) and (s[1] = last) then
  begin
    Delete(result, 1, 1);
    SetLength(result, length(result) - 1);
  end
  else if (firstisquote = false) and (ContainsAnyOfChars(s, cQuotes) = true) then
  begin
    // uncommon, but handling something like: value" target="new"
    if pos(' ', s) <> 0 then
      result := before(s, ' ');
  end;
end;

function ExtractFromTag(const s, tag: string): string;
begin
  result := ExtractFromString(s, '<' + tag + '>', '</' + tag + '>');
end;

function FileUrlToFilename(const url: string): string;
var
  f: string;
begin
  f := url;
  f := after(f, 'file://');
  f := replacestr(f, '/', '\\');
  result := f;
end;

// Generates an URL from a hostname
function GenerateURL(const Host: string; const Port: integer): string;
var
  proto, sport: string;
begin
  if Port = 443 then
    proto := 'https://'
  else
    proto := 'http://';
  if (Port <> 80) and (Port <> 443) then
    sport := ':' + inttostr(Port);
  result := proto + Host + sport;
  if host = emptystr then
    result := emptystr;
end;

function StripHTML(const s: string;PreserveLinesNumber:boolean=false): string;
begin
  if PreserveLinesNumber = true then
  result := StripEnclosedPreserve(s,'<','>')
  else
  result := StripEnclosed(s,'<', '>');
end;

function StripPHPCode(const s: string): string;
var
  i: integer;
  strip: boolean;
begin
  result := emptystr;
  strip := false;
  for i := 1 to length(s) do
  begin
    if (s[i] = '<') and (s[i + 1] = '?') then
      strip := true;
    if strip then
    begin
      if (s[i] = '>') and (s[i - 1] = '?') then
      begin
        strip := false;
        Continue;
      end;
    end
    else
      result := result + s[i];
  end;
end;

// Converts special characters to their HTML entity encoded counterparts
function HtmlEscape(const s: string): string;
begin
  result := replacestr(s, '&', '&amp;');
  result := replacestr(result, '<', '&lt;');
  result := replacestr(result, '>', '&gt;');
  result := replacestr(result, '"', '&quot;');
  result := replacestr(result, '''', '&#x27;'); // same as &apos;
  // The symbolic entity &apos; was originally not included in the HTML spec and
  // might therefore not be supported by all browsers
end;

// Reverts HTML escape of special characters
function HtmlUnescape(const s: string): string;
begin
  result := replacestr(s, '&lt;', '<');
  result := replacestr(result, '&gt;', '>');
  result := replacestr(result, '&quot;', '"');
  result := replacestr(result, '&apos;', '''');
  result := replacestr(result, '&#x27;', '''');
  result := replacestr(result, '&#39;', '''');
  result := replacestr(result, '&amp;', '&');
end;

function HtmlEntityDecode(const s: string): string;
var
  d:THTMLEntities;
begin
  d := THTMLEntities.Create;
  result := d.Decode(s);
  d.Free;
end;

function HtmlEntityEncode(const s: string): string;
var
  d:THTMLEntities;
begin
  d := THTMLEntities.Create;
  result := d.Encode(s);
  d.Free;
end;

function HtmlColorToColor(const Color: string): TColor;
var
  cl: string;
begin
  cl := Color;
  Delete(cl, 1, 1);
  result := StrToIntDef('$' + Copy(cl, 5, 2) + Copy(cl, 3, 2) + Copy(cl, 1, 2),
    $00FFFFFF);
end;

function ExtractUrlFileName(const url: string): string;
var
  i: integer;
begin
  result := url;
  if pos('?', result) <> 0 then
    result := before(result, '?');
  i := LastDelimiter('/', result);
  result := Copy(result, i + 1, length(result) - (i));
end;

function ExtractUrlFileExt(const url: string): string;
begin
  result := ExtractUrlFileName(url);
  if pos('?', result) <> 0 then
    result := before(result, '?');
  result := extractfileext(result);
end;

function ExtractUrlHost(const url: string): string;
begin
  result := after(url, '://');
  result := before(result, '/');
  if beginswith(result, '[') then
  begin // ipv6 format
    result := after(result, '[');
    result := before(result, ']');
    result := '[' + result + ']';
  end
  else
  begin // ipv4 format
    if pos(':', result) <> 0 then
      result := before(result, ':');
  end;
end;

function ExtractUrlPort(const url: string): integer;
var
  temp: string;
begin
  result := 80; // default
  if beginswith(lowercase(url), 'https://') then
    result := 443;
  temp := after(url, '://');
  temp := before(temp, '/');
  if pos(':', temp) <> 0 then
  begin // port provided via format [proto]://[host]:[port]/
    if beginswith(temp, '[') then // ipv6 format
      temp := after(temp, ']:')
    else // ipv4 format
      temp := after(temp, ':');
    if isinteger(temp) then
      result := StrToInt(temp);
  end;
end;

function ExtractUrlPath(const url: string;
  const includeparams: boolean = true): string;
begin
  result := after(url, '://');
  result := after(result, '/');
  if includeparams = false then
  begin
    if pos('?', result) <> 0 then
      result := before(result, '?');
  end;
end;

// Converts JSON special characters to their escaped counterparts
function JSONStringEscape(const s: string): string;
const
  cEscapeChars: TSysCharSet = ['"', '/', '\', #8, #9, #10, #12, #13];
var
  i, p, len: integer;
begin
  Result := EmptyStr;
  p := 1;
  len := Length(s) + 1;
  for i := 1 to len do
  begin
    if CharInSet(s[i], cEscapeChars) then
    begin
      Result := Result+Copy(s, p, i - p);
      case s[i] of
        '\': Result := Result+ '\\';
        '/': Result := Result+ '\/';
        '"': Result := Result+ '\"';
        #8: Result := Result+ '\b';
        #9: Result := Result+ '\t';
        #10: Result := Result+ '\n';
        #12: Result := Result+ '\f';
        #13: Result := Result+ '\r';
      end;
      p := i + 1;
    end;
  end;
  Result := Result+ Copy(s, p, i - 1);
end;

// Removes the extension part of an URL
// RemoveUrlFileExt('http://site.com/image.png?x=1'); -> http://site.com/image
// RemoveUrlFileExt('http://site.com/image.png?x=1', True); -> http://site.com/image?x=1
// RemoveUrlFileExt('http://site.com/archive.zip'); -> http://site.com/archive
// RemoveUrlFileExt('http://site.com/path/'); -> http://site.com/path/
function RemoveUrlFileExt(const url: string; KeepParams: Boolean = False): string;
var
  BaseUrl, Ext, Params: string;
  QPos: Integer;
begin
  // Find query string position
  QPos := Pos('?', url);
  if QPos > 0 then
  begin
    BaseUrl := Copy(url, 1, QPos - 1);
    Params := Copy(url, QPos, MaxInt);
  end
  else
  begin
    BaseUrl := url;
    Params := '';
  end;

  // Remove extension if present
  Ext := ExtractFileExt(BaseUrl);
  if Ext <> '' then
    Delete(BaseUrl, Length(BaseUrl) - Length(Ext) + 1, Length(Ext));

  // Reattach query string only if requested
  if KeepParams then
    Result := BaseUrl + Params
  else
    Result := BaseUrl;
end;

// Removes the filename part of a URL, example:
// http://www.somehost.com/index.php -> http://www.somehost.com/
// If second parameter is true,
// filename is only removed if filename with extension is found in URL
function RemoveURLFilename(const URL:string;ExtensionRequired:boolean=false):string;
var
  fn:string;
  len:integer;
begin
 result := URL;
 fn := ExtractURLFileName(Url);
 if fn = emptystr then
  exit;
 if (ExtensionRequired = true) and (MatchWildcard(fn, '*.??*') = false) then
  exit;
 len := Length(Url)-Length(fn);
 result := Copy(URL, 1, len);
end;

// Removes the path from a URL, example:
// http://www.somehost.com/somepath -> http://www.somehost.com
function RemoveUrlPath(const url: string): string;
var proto:string;
begin
  if pos('://', url )<> 0 then begin
    proto := before(url, '://');
    result := after(url, '://');
  end;
  if proto = emptystr then
    proto := 'http';
  result := proto+ '://' +before(result, '/');
end;

function URLDecode(const s: string): string;
var
  i: integer;
begin
  result := emptystr;
  if length(s) = 0 then
    result := emptystr
  else
  begin
    i := 1;
    while i <= length(s) do
    begin
      if s[i] = '%' then
      begin
        result := result + Chr(HexToInt(s[i + 1] + s[i + 2]));
        Inc(i, 2);
      end
      else if s[i] = '+' then
        result := result + ' '
      else
        result := result + s[i];

      Inc(i);
    end;
  end;
end;

function URLEncode(const s: string; plus: boolean = false;
  const preserve: TSysCharSet = ['0' .. '9', 'A' .. 'Z', 'a' .. 'z',
  ' ', '.']): string;
var
  i: integer;
  sp: string;
begin
  if length(s) = 0 then
    result := emptystr
  else
  begin
    if plus then
      sp := '+'
    else
      sp := '%20';
    for i := 1 to length(s) do
    begin
      if not(CharInSet(s[i], preserve)) then
        result := result + '%' + IntToHex(ord(s[i]), 2)
      else if (s[i] = ' ') then
        result := result + sp
      else
        result := result + s[i];
    end;
  end;
end;

function URLEncode_U(const s: string; plus: boolean = false;
  const preserve: TSysCharSet = ['0' .. '9', 'A' .. 'Z', 'a' .. 'z',
  ' ','.']): string;
var
  i: integer;
  sp: string;
begin
  if length(s) = 0 then
    result := emptystr
  else
  begin
    if plus then
      sp := '+'
    else
      sp := '%u0020';
    for i := 1 to length(s) do
    begin
      if not(CharInSet(s[i], preserve)) then
        result := result + '%u' + IntToHex(Integer(s[i]), 4)
      else if (s[i] = ' ') then
        result := result + sp
      else
        result := result + s[i];
    end;
  end;
end;

function URLEncodeFull(const s: string): string;
begin
  result := URLEncode(s, false, []);
end;

// TitleCase function adapted to work with URL paths
function URLPathTitleCase(const s: string): string;
var
  i: integer;
begin
  result := s;
  for i := 1 to length(result) - 1 do
    if CharInSet(result[i], (['~', '/'] - ['.', '-', 'A' .. 'Z', 'a' .. 'z']))
    then
      if CharInSet(result[i + 1], ['a' .. 'z']) then
        result[i + 1] := char(ord(result[i + 1]) and not $20);
end;

{------------------------------------------------------------------------------}

procedure TURLParamsLoop.LoadFromURL(const aURL:string);
var params,url:string;
begin
  url := trim(aURL);
  url := replacestr(url, ' ','%20');
  params := emptystr;
  if occurs('?', url) <> 0 then begin
    params := gettoken(url, '?', 2);
    params := replacestr(params,'&',crlf);
  end;
  LoadFromString(params);
  Reset;
end;

procedure TURLParamsLoop.LoadFromPOSTData(const POSTData:string);
var params:string;
begin
  params := trim(POSTData);
  params := replacestr(params, ' ','%20');
  if BeginsWith(params, '&') then
    params := After(params, '&');
  if occurs('&', params) <> 0 then begin
    params := replacestr(params,'&',crlf);
  end;
  LoadFromString(params);
  Reset;
end;

function TURLParamsLoop.Found: boolean;
const
 cSep = '=';
begin
 result := inherited found;
 fParamName:=emptystr;
 fParamValue:=emptystr;
 fParamNameLower:=emptystr;
 if pos(cSep,Current) <> 0 then begin
   fParamName:=trim(before(current,cSep));
   fParamValue:=trim(after(current,cSep));
   fParamNameLower:=lowercase(fParamName);
 end;
end;

// ------------------------------------------------------------------------//
end.
