unit CatMediaImages;

{
  Catarinka - Media Images Library
  Useful functions for manipulating images

  Copyright (c) 2003-2025 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls,
  vcl.imaging.Jpeg, Vcl.Imaging.pngimage;
{$ELSE}
  SysUtils, Windows, Classes, Graphics, Controls, Jpeg, pngimage;
{$ENDIF}
function IsImageFileName(const AFileName: string): Boolean;
function IsPlainTextExt(const AFileName: string): Boolean;
function IsVideoExt(const AFileName: string): Boolean;
function CreateTinyBitmap(Color: TColor): TBitmap;
function CreateTextPreviewBitmap(const AText, AExt: string;
  AWidth, AHeight: Integer): TBitmap;
procedure Bmp2Jpeg(const BmpFileName, JpgFileName: string);

implementation

function IsVideoExt(const AFileName: string): Boolean;
var
  ext: string;
begin
  ext := LowerCase(ExtractFileExt(AFileName));
  Result :=
    (ext = '.mp4')  or
    (ext = '.mkv')  or
    (ext = '.avi')  or
    (ext = '.mov')  or
    (ext = '.wmv')  or
    (ext = '.webm') or
    (ext = '.m4v');
end;

function IsImageFileName(const AFileName: string): Boolean;
var
  ext: string;
begin
  ext := LowerCase(ExtractFileExt(AFileName));
  Result :=
    (ext = '.jpg') or (ext = '.jpeg') or
    (ext = '.png') or (ext = '.bmp') or
    (ext = '.gif') or (ext = '.tif') or
    (ext = '.tiff') or (ext = '.webp')or (ext = '.ico');
end;

function IsPlainTextCodeExt(const AFileName: string): Boolean;
var
  ext: string;
begin
  ext := LowerCase(ExtractFileExt(AFileName));
  Result :=
    (ext = '.dpr') or
    (ext = '.pas');
end;

function IsPlainTextExt(const AFileName: string): Boolean;
var
  ext: string;
begin
  ext := LowerCase(ExtractFileExt(AFileName));
  Result :=
    (ext = '.txt') or
    (ext = '.log') or
    (ext = '.ini') or
    (ext = '.cfg') or
    (ext = '.json') or
    (ext = '.xml') or
    (ext = '.md') or
    (ext = '.csv') or (IsPlainTextCodeExt(AFilename) = true);
end;

function CreateTextPreviewBitmap(const AText, AExt: string;
  AWidth, AHeight: Integer): TBitmap;
var
  R: TRect;
  S: string;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.SetSize(AWidth, AHeight);

  // Background
  Result.Canvas.Brush.Color := clWhite;
  Result.Canvas.FillRect(Rect(0, 0, AWidth, AHeight));

  // Border
  Result.Canvas.Pen.Color := clSilver;
  Result.Canvas.Brush.Style := bsClear;
  Result.Canvas.Rectangle(0, 0, AWidth - 1, AHeight - 1);

  // Header bar (extension)
  Result.Canvas.Brush.Style := bsSolid;
  Result.Canvas.Brush.Color := clNavy;
  Result.Canvas.Pen.Color := clNavy;
  Result.Canvas.Rectangle(0, 0, AWidth - 1, 18);
  Result.Canvas.Font.Color := clWhite;
  Result.Canvas.Font.Name := 'Segoe UI';
  Result.Canvas.Font.Size := 8;
  Result.Canvas.TextOut(4, 2, UpperCase(Copy(AExt, 2, MaxInt))); // "TXT", "JSON"

  // Text area
  Result.Canvas.Brush.Style := bsClear;
  Result.Canvas.Font.Color := clBlack;
  Result.Canvas.Font.Name := 'Consolas';      // nice monospaced preview
  Result.Canvas.Font.Size := 8;

  // Truncate text to something sane
  S := AText;
  if Length(S) > 800 then
  begin
    SetLength(S, 800);
    S := S + '...';
  end;

  R := Rect(4, 20, AWidth - 4, AHeight - 4);
  DrawText(Result.Canvas.Handle, PChar(S), Length(S), R,
    DT_LEFT or DT_TOP or DT_WORDBREAK or DT_END_ELLIPSIS);
end;

procedure Bmp2Jpeg(const BmpFileName, JpgFileName: string);
var
  Bmp: TBitmap;
  Jpg: TJPEGImage;
begin
  Bmp := TBitmap.Create;
  Jpg := TJPEGImage.Create;
  try
    Bmp.LoadFromFile(BmpFileName);
    Jpg.Assign(Bmp);
    Jpg.SaveToFile(JpgFileName);
  finally
    Jpg.Free;
    Bmp.Free;
  end;
end;

function CreateTinyBitmap(Color: TColor): TBitmap;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.SetSize(1, 1);
  Result.Canvas.Brush.Color := Color;
  Result.Canvas.FillRect(Rect(0, 0, 1, 1));
end;

// ------------------------------------------------------------------------//
end.
