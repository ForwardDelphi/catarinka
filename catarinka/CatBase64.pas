unit CatBase64;
{
  Catarinka - Base64 encode/decode functions

  Copyright (c) 2003-2025 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  Base64 encode and decode functions by Lukas Gebauer (BSD license, included below)
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  {$IFDEF USECROSSVCL}
  WinAPI.Windows,
  {$ENDIF}
  System.Classes, System.SysUtils, System.NetEncoding;
{$ELSE}
  Classes, SysUtils, NetEncoding;
{$ENDIF}

function IsBase64String(const S: string): Boolean;
function Base64Encode(const s: string): string;
function Base64Decode(const s: string): string;
function ContainsValidBase64Symbols(const S: string): Boolean;
function FileToB64(const filename:string):string;
procedure B64ToFile(const Base64, outfilename:string);
function FileToDataURL(const filename:string; mimetype:string=''):string;
procedure MemoryStreamToBase64(const Stream: TMemoryStream; var Base64: string);
procedure Base64ToMemoryStream(const Base64: string; var Stream: TMemoryStream);

implementation

uses
  CatStrings, CatFiles;

function RemoveWhitespace(const S: string): string;
var
  i, p: Integer;
  ch: Char;
begin
  SetLength(Result, Length(S));
  p := 0;
  for i := 1 to Length(S) do
  begin
    ch := S[i];
    if not (ch in [#9, #10, #13, ' ']) then
    begin
      Inc(p);
      Result[p] := ch;
    end;
  end;
  SetLength(Result, p);
end;

function NormalizeBase64ForDecode(const S: string; out Normalized: string): Boolean;
var
  i, len, eqPos, eqCount: Integer;
  ch: Char;
  hasPlusSlash, hasUrlSafe: Boolean;
begin
  Result := False;

  Normalized := RemoveWhitespace(S);
  if Normalized = '' then
    Exit(False);

  // Detect mixed alphabets
  hasPlusSlash := (Pos('+', Normalized) > 0) or (Pos('/', Normalized) > 0);
  hasUrlSafe   := (Pos('-', Normalized) > 0) or (Pos('_', Normalized) > 0);
  if hasPlusSlash and hasUrlSafe then
    Exit(False);

  // Translate URL-safe to standard
  if hasUrlSafe then
  begin
    Normalized := StringReplace(Normalized, '-', '+', [rfReplaceAll]);
    Normalized := StringReplace(Normalized, '_', '/', [rfReplaceAll]);
  end;

  // Validate charset
  for i := 1 to Length(Normalized) do
  begin
    ch := Normalized[i];
    if not (ch in ['A'..'Z','a'..'z','0'..'9','+','/','=']) then
      Exit(False);
  end;

  // '=' padding must be only at the end, 1 or 2 max
  eqPos := Pos('=', Normalized);
  if eqPos > 0 then
  begin
    eqCount := Length(Normalized) - eqPos + 1;
    if (eqCount > 2) then
      Exit(False);
    for i := eqPos to Length(Normalized) do
      if Normalized[i] <> '=' then
        Exit(False);
  end;

  // Ensure length is multiple of 4 (pad if needed; remainder 1 is invalid)
  len := Length(Normalized);
  case (len mod 4) of
    0: ; // ok
    2: Normalized := Normalized + '==';
    3: Normalized := Normalized + '=';
    1: Exit(False);
  end;

  Result := True;
end;

function IsBase64String(const S: string): Boolean;
var
  N: string;
begin
  Result := False;
  if not NormalizeBase64ForDecode(S, N) then
    Exit;
  try
    // If decoding succeeds, it's valid Base64
    TNetEncoding.Base64.DecodeStringToBytes(N);
    Result := True;
  except
    Result := False;
  end;
end;

function ContainsValidBase64Symbols(const S: string): Boolean;
var
  I: Integer;
  C: Char;
begin
  Result := False;

  // Base64 strings should not be empty
  if S = '' then
    Exit;

  for I := 1 to Length(S) do
  begin
    C := S[I];

    // Check if the character is valid in Base64 encoding
    if not (C in ['A'..'Z', 'a'..'z', '0'..'9', '+', '/', '=']) then
      Exit(False);
  end;

  Result := True;
end;


function FileToDataURL(const filename:string; mimetype:string=''):string;
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(filename);
  ms.Position := 0;
  MemoryStreamToBase64(ms, result);
  ms.Free;
  if mimetype = '' then
  mimetype := FilenameToMimeType(filename);
  result := 'data:'+mimetype+';base64,'+result;
  result := replacestr(result, crlf, emptystr);
end;

function FileToB64(const filename:string):string;
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(filename);
  ms.Position := 0;
  MemoryStreamToBase64(ms, result);
  ms.Free;
end;

procedure B64ToFile(const Base64, outfilename:string);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  Base64ToMemoryStream(Base64, ms);
  ms.SaveToFile(outfilename);
  ms.Free;
end;

procedure MemoryStreamToBase64(const Stream: TMemoryStream; var Base64: string);
var
  Encoder: TBase64Encoding;
begin
  Encoder := TBase64Encoding.Create;
  try
    Base64 := Encoder.EncodeBytesToString(Stream.Memory, Stream.Size);
  finally
    Encoder.Free;
  end;
end;

procedure Base64ToMemoryStream(const Base64: string; var Stream: TMemoryStream);
var
  Bytes: TBytes;
begin
  Bytes := TNetEncoding.Base64.DecodeStringToBytes(Base64);
  Stream.Clear;
  Stream.WriteBuffer(Bytes[0], Length(Bytes));
  Stream.Position := 0;
end;

{function MemStreamToB64(m: TMemoryStream): String;
begin
  Result := Base64Encode(MemStreamToStr(m));
end; }

// CONTRIBUTED ------------------------------------------------------------//
// Base64 encoder and decoder taken from Ararat Synapse's synacode.pas
{
  | Copyright (c)1999-2007, Lukas Gebauer                                        |
  | All rights reserved.                                                         |
  |                                                                              |
  | Redistribution and use in source and binary forms, with or without           |
  | modification, are permitted provided that the following conditions are met:  |
  |                                                                              |
  | Redistributions of source code must retain the above copyright notice, this  |
  | list of conditions and the following disclaimer.                             |
  |                                                                              |
  | Redistributions in binary form must reproduce the above copyright notice,    |
  | this list of conditions and the following disclaimer in the documentation    |
  | and/or other materials provided with the distribution.                       |
  |                                                                              |
  | Neither the name of Lukas Gebauer nor the names of its contributors may      |
  | be used to endorse or promote products derived from this software without    |
  | specific prior written permission.                                           |
  |                                                                              |
  | THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
  | AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
  | IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
  | ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
  | ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
  | DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
  | SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
  | CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
  | LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
  | OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
  | DAMAGE.                                                                      |
}
function Encode3to4(const Value, Table: AnsiString): AnsiString;
var
  c: Byte;
  n, l: integer;
  count: integer;
  DOut: array [0 .. 3] of Byte;
begin
  SetLength(result, ((length(Value) + 2) div 3) * 4);
  l := 1;
  count := 1;
  while count <= length(Value) do
  begin
    c := ord(Value[count]);
    inc(count);
    DOut[0] := (c and $FC) shr 2;
    DOut[1] := (c and $03) shl 4;
    if count <= length(Value) then
    begin
      c := ord(Value[count]);
      inc(count);
      DOut[1] := DOut[1] + (c and $F0) shr 4;
      DOut[2] := (c and $0F) shl 2;
      if count <= length(Value) then
      begin
        c := ord(Value[count]);
        inc(count);
        DOut[2] := DOut[2] + (c and $C0) shr 6;
        DOut[3] := (c and $3F);
      end
      else
      begin
        DOut[3] := $40;
      end;
    end
    else
    begin
      DOut[2] := $40;
      DOut[3] := $40;
    end;
    for n := 0 to 3 do
    begin
      if (DOut[n] + 1) <= length(Table) then
      begin
        result[l] := Table[DOut[n] + 1];
        inc(l);
      end;
    end;
  end;
  SetLength(result, l - 1);
end;

function Decode4to3Ex(const Value, Table: AnsiString): AnsiString;
var
  x, y, lv: integer;
  d: integer;
  dl: integer;
  c: Byte;
  P: integer;
begin
  lv := length(Value);
  SetLength(result, lv);
  x := 1;
  dl := 4;
  d := 0;
  P := 1;
  while x <= lv do
  begin
    y := ord(Value[x]);
    if y in [33 .. 127] then
      c := ord(Table[y - 32])
    else
      c := 64;
    inc(x);
    if c > 63 then
      continue;
    d := (d shl 6) or c;
    Dec(dl);
    if dl <> 0 then
      continue;
    result[P] := AnsiChar((d shr 16) and $FF);
    inc(P);
    result[P] := AnsiChar((d shr 8) and $FF);
    inc(P);
    result[P] := AnsiChar(d and $FF);
    inc(P);
    d := 0;
    dl := 4;
  end;
  case dl of
    1:
      begin
        d := d shr 2;
        result[P] := AnsiChar((d shr 8) and $FF);
        inc(P);
        result[P] := AnsiChar(d and $FF);
        inc(P);
      end;
    2:
      begin
        d := d shr 4;
        result[P] := AnsiChar(d and $FF);
        inc(P);
      end;
  end;
  SetLength(result, P - 1);
end;

function Base64Encode(const s: string): string;
const
  TableBase64 =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';
begin
  result := string(Encode3to4(AnsiString(s), TableBase64));
end;

function Base64Decode(const s: string): string;
const
  ReTablebase64 = #$40 + #$40 + #$40 + #$40 + #$40 + #$40 + #$40 + #$40 + #$40 +
    #$40 + #$3E + #$40 + #$40 + #$40 + #$3F + #$34 + #$35 + #$36 + #$37 + #$38 +
    #$39 + #$3A + #$3B + #$3C + #$3D + #$40 + #$40 + #$40 + #$40 + #$40 + #$40 +
    #$40 + #$00 + #$01 + #$02 + #$03 + #$04 + #$05 + #$06 + #$07 + #$08 + #$09 +
    #$0A + #$0B + #$0C + #$0D + #$0E + #$0F + #$10 + #$11 + #$12 + #$13 + #$14 +
    #$15 + #$16 + #$17 + #$18 + #$19 + #$40 + #$40 + #$40 + #$40 + #$40 + #$40 +
    #$1A + #$1B + #$1C + #$1D + #$1E + #$1F + #$20 + #$21 + #$22 + #$23 + #$24 +
    #$25 + #$26 + #$27 + #$28 + #$29 + #$2A + #$2B + #$2C + #$2D + #$2E + #$2F +
    #$30 + #$31 + #$32 + #$33 + #$40 + #$40 + #$40 + #$40 + #$40 + #$40;
begin
  result := string(Decode4to3Ex(AnsiString(s), ReTablebase64));
end;

// ------------------------------------------------------------------------//
end.


