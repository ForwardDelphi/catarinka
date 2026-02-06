unit CatFiles;
{
  Catarinka - File System functions

  Copyright (c) 2003-2025 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  CopyAfterFirstLine and WipeFile functions by Peter Below
  FileCopy function by David Stidolph
}

interface

{$I Catarinka.inc}

uses
{$IF CompilerVersion > 22}
  Winapi.Windows, System.Classes, System.SysUtils, Winapi.ShellAPI,
  System.IOUtils;
{$ELSE}
  Windows, Classes, SysUtils, ShellAPI;
{$IFEND}
function CleanFilename(const filename: string;
  const invCharRep: Char = '_'): string;
function DeleteFolder(const dir: string): boolean;
function DirExists(const dir: string): boolean;
function DirIsEmpty(const dir: string): boolean;
function FileCanBeOpened(const filename: String): boolean;
function FileCopy(const source, dest: string): boolean;
function FilenameToMimeType(const filename: string): string;
function FindDiskDriveByVolumeName(const AVolumeName: String;
  IncludeRemovables:boolean=true): String;
function ForceDir(const dir: string): boolean;
function GetAbsoluteDir(const BaseDir, Reference: string; IgnoreFirstTraversal: Boolean = False): string;
function GetDirAge(const Dir: string): TDateTime;
function GetDiskSerialNumber(const drive: string): string;
function GetVolumeName(const ADriveLetter: Char): string;
function GetDllFilename:string;
function GetFileSize(const filename: string): Int64;
function GetFileToStr(const filename: string): string;
function GetFileVersion(const filename: string;
  const ResFormat: string = '%d.%d.%d.%d'): string;
function GetFileVersionValue(fileName: string; PropertyName: string): string;
function GetSizeDescription(const bytes: int64): string;
function GetSizeDescriptionBytes(const desc: string): int64;
function GetTextFileLinesCount(const filename: string): integer;
function GetTempFile(const ext: string): string;
function GetWindowsTempDir: string;
function SendToLog(const filename: TFilename; const s: string): boolean;
function SL_LoadFromFile(const SL: TStrings; const filename: string)
  : boolean;
function SL_SaveToFile(const SL: TStrings; const filename: string): boolean;
procedure CatReadLn(const f: Text; var s: string);
procedure CopyAfterFirstLine(const sourcefile, targetfile: string;
  appendln: boolean = false; lnstr: string = '');
procedure GetDirs(const dir: string; const Result: TStrings;
  SortResult: boolean = true);
procedure GetDirsByCreationTime(const dir: string; const Result: TStrings;
  Ascending: Boolean = True);
function GetDirectorySize(const APath: string): Int64;
procedure GetFiles(const dir: string; const Result: TStrings;
  const IncludeDir: boolean = false; const IncludeExt: boolean = true);
procedure GetFilesRecursive(const Result: TStrings; Dir, Mask: string);
procedure WipeFile(const filename: string);
function GetFreeSpaceOnDrive(const Drive: string): Int64;
procedure GetDiskDrives(var ADriveList: TStrings; IncludeRemovables:boolean=true);

// File operations involving date/time
procedure GetFilesRecursiveByCreationTime(const Result: TStrings;
  Dir, Mask: string; Ascending: Boolean = True);
procedure GetFilesByCreationTimeAsc(const dir: string; const Result: TStrings;
  const IncludeDir: Boolean = False; const IncludeExt: Boolean = True);
procedure GetFilesByCreationTimeDesc(const dir: string; const Result: TStrings;
  const IncludeDir: Boolean = False; const IncludeExt: Boolean = True);
function SortFoldersByCreationDate(const BasePath, FolderNames: string): string;
function GetFileDateTimes(const FileName: string;
  out Created, Modified: TDateTime): Boolean;
procedure ApplyFileDateTimes(const FileName: string;
  const Created, Modified: TDateTime);


implementation

uses
  CatStrings;

function GetFileDateTimes(const FileName: string;
  out Created, Modified: TDateTime): Boolean;
var
  h: THandle;
  ftCreate, ftAccess, ftWrite: TFileTime;
  st: TSystemTime;
begin
  Result := False;
  Created := 0;
  Modified := 0;

  h := CreateFile(PChar(FileName),
                  GENERIC_READ,
                  FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                  nil,
                  OPEN_EXISTING,
                  FILE_ATTRIBUTE_NORMAL,
                  0);
  if h = INVALID_HANDLE_VALUE then
    Exit;
  try
    if GetFileTime(h, @ftCreate, @ftAccess, @ftWrite) then
    begin
      if FileTimeToSystemTime(ftCreate, st) then
        Created := SystemTimeToDateTime(st);
      if FileTimeToSystemTime(ftWrite, st) then
        Modified := SystemTimeToDateTime(st);
      Result := True;
    end;
  finally
    CloseHandle(h);
  end;
end;

procedure ApplyFileDateTimes(const FileName: string;
  const Created, Modified: TDateTime);
var
  h: THandle;
  st: TSystemTime;
  ftLocal, ftUtcCreate, ftUtcWrite: TFileTime;
begin
  if (Created = 0) and (Modified = 0) then
    Exit;

  h := CreateFile(PChar(FileName),
                  GENERIC_WRITE,
                  FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                  nil,
                  OPEN_EXISTING,
                  FILE_ATTRIBUTE_NORMAL,
                  0);
  if h = INVALID_HANDLE_VALUE then
    Exit;
  try
    // Creation time
    if Created <> 0 then
    begin
      DateTimeToSystemTime(Created, st);
      SystemTimeToFileTime(st, ftLocal);
      LocalFileTimeToFileTime(ftLocal, ftUtcCreate);
    end
    else
      ZeroMemory(@ftUtcCreate, SizeOf(ftUtcCreate));

    // Modified time
    if Modified <> 0 then
    begin
      DateTimeToSystemTime(Modified, st);
      SystemTimeToFileTime(st, ftLocal);
      LocalFileTimeToFileTime(ftLocal, ftUtcWrite);
    end
    else
      ZeroMemory(@ftUtcWrite, SizeOf(ftUtcWrite));

    // Access time left unchanged (nil)
SetFileTime(h,
  PFileTime(Ord(Created <> 0) * NativeInt(@ftUtcCreate)),
  nil,
  PFileTime(Ord(Modified <> 0) * NativeInt(@ftUtcWrite))
);
  finally
    CloseHandle(h);
  end;
end;

function GetAbsoluteDir(const BaseDir, Reference: string; IgnoreFirstTraversal: Boolean = False): string;
  function NormalizeRef(const S: string): string;
  begin
    Result := S.Replace('/', PathDelim).Replace('\', PathDelim);
    // Strip redundant leading path delimiters (so "/../" works relative to BaseDir)
    while (Length(Result) > 0) and (Result[1] = PathDelim) do
      Delete(Result, 1, 1);
  end;

  function JoinParts(const Parts: TArray<string>): string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to High(Parts) do
    begin
      if Result = '' then
        Result := Parts[I]
      else
        Result := TPath.Combine(Result, Parts[I]);
    end;
  end;

var
  BaseAbs, RefNorm, Combined, RelPath: string;
  Parts, NewParts: TArray<string>;
  I: Integer;
  SkipFirst: Boolean;
begin
  BaseAbs := ExpandFileName(TPath.GetFullPath(BaseDir));
  RefNorm := NormalizeRef(Reference);

  if TPath.IsPathRooted(RefNorm) then
    Exit(ExpandFileName(TPath.GetFullPath(RefNorm)));

  Parts := RefNorm.Split([PathDelim], TStringSplitOptions.ExcludeEmpty);
  SetLength(NewParts, 0);

  SkipFirst := IgnoreFirstTraversal;

  for I := 0 to High(Parts) do
  begin
    if Parts[I] = '..' then
    begin
      if SkipFirst then
      begin
        SkipFirst := False; // only ignore one
        Continue;
      end;
      if Length(NewParts) > 0 then
        SetLength(NewParts, Length(NewParts)-1);
    end
    else if Parts[I] <> '.' then
    begin
      NewParts := NewParts + [Parts[I]];
    end;
  end;

  RelPath := JoinParts(NewParts);

  if RelPath <> '' then
    Combined := TPath.Combine(BaseAbs, RelPath)
  else
    Combined := BaseAbs;

  Result := ExpandFileName(TPath.GetFullPath(Combined));

  if (Result <> TPath.GetPathRoot(Result)) then
    Result := ExcludeTrailingPathDelimiter(Result);
end;

function GetFreeSpaceOnDrive(const Drive: string): Int64;
var
  FreeBytesAvailable, TotalBytes, TotalFreeBytes: Int64;
begin
  // Call the GetDiskFreeSpaceEx function
  if GetDiskFreeSpaceEx(PChar(Drive), FreeBytesAvailable, TotalBytes, @TotalFreeBytes) then
  begin
    Result := FreeBytesAvailable;
  end
  else
  begin
    // Return -1 in case of an error
    Result := -1;
  end;
end;

function GetDirectorySize(const APath: string): Int64;
var
  SearchRec: TSearchRec;
begin
  Result := 0;

  if FindFirst(APath + '\*.*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          if (SearchRec.Attr and faDirectory) = faDirectory then
            Result := Result + GetDirectorySize(APath + '\' + SearchRec.Name)
          else
            Result := Result + SearchRec.Size;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

procedure CatReadLn(const f: Text; var s: string);
var
  c: Char;
  tempStr: string;
begin
  tempStr := emptystr;
  while not Eof(f) do
  begin
    read(f, c);
    case Ord(c) of
      10:
        Break;
      13:
        begin
          read(f, c);
          if Ord(c) = 13 then
          begin // hides H2077 compiler warning
          end;
          Break;
        end;
    else
      tempStr := tempStr + c;
    end;
  end;
  s := tempStr;
end;

// Deletes a directory and its sub directories
function DeleteFolder(const dir: string): boolean;
var
  sdir: string;
  st: TSHFileOpStruct;
begin
  sdir := dir;
  if LastChar(sdir) = '\' then
    sdir := copy(sdir, 1, Length(sdir) - 1);
  try
    FillChar(st, SizeOf(st), #0);
    sdir := sdir + #0#0;
    with st do
    begin
      Wnd := 0;
      wFunc := FO_DELETE;
      pFrom := PChar(sdir);
      fFlags := FOF_SILENT or FOF_NOCONFIRMATION;
    end;
    Result := (SHFileOperation(st) = 0);
  except
    Result := false;
  end;
end;

function DirExists(const dir: string): boolean;
begin
  Result := DirectoryExists(dir);
end;

// Returns true if a directory is empty, false otherwise. If the directory does
// not exists, also returns true
function DirIsEmpty(const dir: string): boolean;
var
  i: Integer;
  sr: TSearchRec;
begin
  if DirExists(dir) = true then begin
    Result := false;
    FindFirst(IncludeTrailingPathDelimiter(dir) + '*', faAnyFile, sr);
    for i := 1 to 2 do
      if (sr.Name = '.') or (sr.Name = '..') then
        Result := FindNext(sr) <> 0;
    FindClose(sr);
  end else
  Result := true;
end;

function FileCanBeOpened(const filename: string): boolean;
var
  h: integer;
begin
  try
    h := FileOpen(filename, fmOpenRead or fmShareDenyNone);
    if h > 0 then
    begin
      Result := true;
      FileClose(h);
    end
    else
      Result := false;
  except
    Result := false
  end;
end;

function FilenameToMimeType(const filename: string): string;
var
  ext: string;
begin
  ext := LowerCase(ExtractFileExt(filename));
  if Length(ext) > 1 then
    ext := copy(ext, 2, Length(ext));
  if ext = 'css' then
    Result := 'text/css'
  else if (ext = 'htm') or (ext = 'html') then
    Result := 'text/html'
  else if ext = 'bmp' then
    Result := 'image/bmp'
  else if ext = 'gif' then
    Result := 'image/gif'
  else if (ext = 'jpg') or (ext = 'jpeg') then
    Result := 'image/jpeg'
  else if ext = 'js' then
    Result := 'text/javascript'
  else if (ext = 'png') then
    Result := 'image/png'
  else if ext = 'txt' then
    Result := 'text/plain'
  else
    Result := 'application/octet-stream'; // Unknown Type
end;

function CleanFilename(const filename: string;
  const invCharRep: Char = '_'): string;
const
  invChars = ['\', ':', '*', '?', '"', '<', '>', '|', '/'];
begin
  Result := ReplaceChars(filename, invChars, invCharRep);
end;

function ForceDir(const dir: string): boolean;
var
  d: string;
begin
  d := ReplaceStr(dir, '\\', '\');
  d := ReplaceStr(d, '//', '/');
  if d <> emptystr then
  Result := ForceDirectories(d) else
  Result := false;
end;

function GetDirAge(const Dir: string): TDateTime;
var
  FileSpecs: TGetFileExInfoLevels;
  DirData: TWin32FileAttributeData;
  FileTime: TSystemTime;
begin
  Result := 0;
  fillchar(FileSpecs, sizeof(FileSpecs), 0);
  FileSpecs := GetFileExInfoStandard;
  fillchar(DirData, sizeof(DirData), 0);
  if GetFileAttributesEx({$IFDEF UNICODE}pwidechar{$ELSE}pchar{$ENDIF}(string(Dir)), FileSpecs, @DirData) then
  begin
    FileTimeToSystemTime(DirData.ftCreationTime, FileTime);
    Result := SystemTimeToDateTime(FileTime);
  end;
end;

procedure GetDirs(const dir: string; const Result: TStrings;
  SortResult: boolean = true);
var
  SL: TStringList;
  sr: TSearchRec;
begin
  SL := TStringList.Create;
  try
    if FindFirst(dir + '*.*', faDirectory, sr) = 0 then
    begin
      repeat
        if ((sr.Attr and faDirectory) = faDirectory) and (sr.name <> '.') and
          (sr.name <> '..') then
          SL.Add(sr.name);
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
    if SortResult = true then
      SL.sort;
    Result.Text := SL.Text;
  finally
    SL.Free;
  end;
end;

function GetDiskSerialNumber(const drive: string): string;
var
  sn, len, flags: DWORD;
begin
  GetVolumeInformation(PChar(drive), nil, 0, @sn, len, flags, nil, 0);
  Result := IntToHex(HiWord(sn), 4) + '-' + IntToHex(LoWord(sn), 4);
end;

// Returns the DLL filename (like Application.Exename would for the EXE)
function GetDllFilename:string;
var FileName : array[0..MAX_PATH] of char;
begin
 FillChar(FileName, sizeof(FileName), #0);
 GetModuleFileName(hInstance, filename, sizeof(filename));
 result:=filename;
end;

procedure GetFiles(const dir: string; const Result: TStrings;
  const IncludeDir: boolean = false; const IncludeExt: boolean = true);
var
  rc: integer;
  tmpPath, ffound: string;
  sr: TSearchRec;
begin
  if Result = nil then
    exit;
  tmpPath := IncludeTrailingPathDelimiter(ExtractFilePath(dir));
  rc := FindFirst(dir, faAnyFile, sr);
  while rc = 0 do
  begin
    ffound := sr.name;
    if IncludeExt = false then
      ffound := changefileext(ffound, emptystr);
    if IncludeDir then
      Result.Add(tmpPath + ffound)
    else
      Result.Add(ffound);
    rc := FindNext(sr);
  end;
  FindClose(sr);
end;

procedure GetFilesRecursive(const Result: TStrings; Dir, Mask: string);
var
  dirs: TStrings;
  SR: TSearchRec;
  f: boolean;
  i: integer;
begin
  if LastChar(Dir) <> PathDelim then
    Dir := Dir + PathDelim;

  f := FindFirst(Dir + Mask, faAnyFile - faDirectory, SR) = 0;
  while f = true do
  begin
    Result.Add(Dir + SR.Name);
    f := FindNext(SR) = 0;
  end;
  FindClose(SR);

  // Makes a list of the subdirectories
  dirs := TStringList.Create;
  f := FindFirst(Dir + '*.*', faAnyFile, SR) = 0;
  while f do
  begin
    if ((SR.Attr and faDirectory) <> 0) and (SR.Name[1] <> '.') then
      dirs.Add(Dir + SR.Name);
    f := FindNext(SR) = 0;
  end;
  FindClose(SR);

  // Gets the list of files in each subdirectory
  for i := 0 to dirs.Count - 1 do
    GetFilesRecursive(Result, dirs[i], Mask);

  dirs.Free;
end;

function GetFileSize(const filename: string): Int64;
var
  f: TWin32FindData;
  h: THandle;
begin
  Result := -1;
  try
    if not FileExists(filename) then
      exit;
    h := FindFirstFile({$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(filename), f);
    if h = INVALID_HANDLE_VALUE then
      RaiseLastOSError;
    try
      Result := f.nFileSizeHigh shl 32 + f.nFileSizeLow;
    finally
{$IF CompilerVersion > 22}Winapi.{$ENDIF}Windows.FindClose(h);
    end;
  except
  end;
end;

function GetFileToStr(const filename: string): string;
var
  SL: TStringList;
  f: TFileStream;
begin
  SL := TStringList.Create;
  try
    f := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
    try
      SL.LoadFromStream(f);
      Result := SL.Text;
    finally
      f.Free;
    end;
  except
    Result := '';
  end;
  SL.Free;
end;

// Returns the file version of a binary file (DLL, EXE, etc)
function GetFileVersion(const filename: string;
  const ResFormat: string = '%d.%d.%d.%d'): string;
var
  p, pi: Pointer;
  infosz, plen: DWORD;
  verinfo: VS_FIXEDFILEINFO;
begin
  Result := emptystr;
{$IFDEF MSWINDOWS}
  infosz := GetFileVersionInfoSize({$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(filename), plen);
  FillChar(verinfo, SizeOf(verinfo), 0);
  if infosz > 0 then
  begin
    GetMem(p, infosz);
    GetFileVersionInfo({$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(filename),
      0, infosz, p);
    VerQueryValue(p, '\', pi, plen);
    move(pi^, verinfo, SizeOf(verinfo));
    Result := Format(ResFormat, [verinfo.dwFileVersionMS shr 16,
      verinfo.dwFileVersionMS and 65535, verinfo.dwFileVersionLS shr 16,
      verinfo.dwFileVersionLS and 65535]);
    FreeMem(p);
  end;

{$ENDIF}
end;

// Gets the value of a version property information of a binary file
// Ex: GetFileVersionValue(filename, 'ProductVersion') will return the product version
function GetFileVersionValue(fileName: string; PropertyName: string): string;
var
  infosz, plen: DWORD;
  len: UINT;
  buf, value: {$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF};
  transno: PLongInt;
  sfinfo: string;
begin
  Result := emptystr;
  infosz := GetFileVersionInfoSize({$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(FileName), plen);
  if infosz > 0 then begin
    Buf := AllocMem(infosz);
    try
      GetFileVersionInfo({$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(FileName), 0, infosz, Buf);
      VerQueryValue(Buf, {$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}('VarFileInfo\Translation'), Pointer(TransNo), Len);
      SFInfo := Format('%s%.4x%.4x%s%s%', ['StringFileInfo\', LoWord(TransNo^), HiWord(Transno^), '\', PropertyName]);
      if VerQueryValue(Buf, {$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(SFInfo), Pointer(Value), Len) then
        Result := Value;
    finally
      if Assigned(Buf) then
        FreeMem(Buf, infosz);
    end;
  end;
end;

function GetSizeDescription(const bytes: int64): string;
const
  cFF = '0.0';
begin
  if bytes < 1 then
    Result := '0 bytes'
  else
    case bytes of
      1 .. 1023:
        Result := InttoStr(bytes) + ' bytes';
      1024 .. 1048575:
        Result := FormatFloat(cFF, bytes / 1024) + ' KB';
      1048576 .. 1073741823:
        Result := FormatFloat(cFF, bytes / 1048576) + ' MB';
    else
      Result := FormatFloat(cFF, bytes / 1073741824) + ' GB';
    end;
end;

// Reverts a size description like 1mb or "1 megabyte" to the equivalent
// number of bytes
function GetSizeDescriptionBytes(const desc: string): int64;
var
  sz: double;
  szstr, szlet, adesc: string;
begin
  result := 0;
  adesc := replacechars(desc, ['(', ')'], ' ');
  adesc := replacestr(adesc, ' ', emptystr);
  adesc := replacestr(adesc, ',', '.');
  adesc := lowercase(adesc);
  if lastchar(adesc) = 's' then
    adesc := removelastchar(adesc);
  szstr := ExtractChars(adesc, ['0' .. '9', '.']);
  szlet := ExtractChars(adesc, ['A' .. 'Z', 'a'..'z']);
  try
    sz := StrToFloat(szstr);
    if szlet = 'b' then
      result := Trunc(sz)
    else if szlet = 'byte' then
      result := Trunc(sz)
    else if MatchStrInArray(szlet, ['kilobyte', 'kbyte', 'kb']) then
      result := Trunc(sz * 1024)
    else if MatchStrInArray(szlet, ['megabyte', 'mb', 'm']) then
      result := Trunc(sz * 1048576)
    else if MatchStrInArray(szlet, ['gigabyte', 'gb']) then
      result := Trunc(sz * 1073741824)
    else if MatchStrInArray(szlet, ['terabyte', 'tb']) then
      result := Trunc(sz * 1099511627776)
    else if MatchStrInArray(adesc, ['petabyte', 'pb']) then
      result := Trunc(sz * 1125899906842624);
  except
  end;
end;

// Returns a temporary filename (located in the Windows Temporary directory)
// This function will not create the temporary file, just return a filename suggestion
// Usage Example: ShowMessage(GetTempFile('.tmp'))
function GetTempFile(const ext: string): string;
var
  buf: array [0 .. MAX_PATH] of {$IFDEF UNICODE}WideChar{$ELSE}Char{$ENDIF};
begin
  GetTempPath({$IFDEF UNICODE}Length{$ELSE}SizeOf{$ENDIF}(buf) - 1, buf);
  GetTempFileName(buf, '~', 0, buf);
  Result := StrPas(buf);
  DeleteFile(result);
  if ext <> emptystr then // if the extension is empty will return a .tmp
    Result := changefileext(Result, ext);
end;

function GetTextFileLinesCount(const filename: string): integer;
var
  f: Textfile;
  s: string;
begin
  AssignFile(f, filename);
  Reset(f);
  Result := 0;
  while not seekeof(f) do
  begin
    Result := Result + 1;
    CatReadLn(f, s);
  end;
  Closefile(f);
end;

function GetWindowsTempDir: String;
var
  bufFolder: array [0 .. MAX_PATH] of
{$IFDEF UNICODE}WideChar{$ELSE}Char{$ENDIF};
begin
  GetTempPath({$IFDEF UNICODE}Length{$ELSE}SizeOf{$ENDIF}(bufFolder),
    bufFolder);
  Result := IncludeTrailingPathDelimiter(String(bufFolder));
end;

function SendToLog(const filename: TFilename; const s: String): boolean;
var
  f: Textfile;
begin
  try
    AssignFile(f, filename);
    if FileExists(filename) = false then
      ReWrite(f)
    else
    begin
      Reset(f);
      Append(f);
    end;
    WriteLn(f, s);
    Closefile(f);
    Result := true;
  except
    Result := false;
  end;
end;

function SL_SaveToFile(const SL: TStrings; const filename: string): boolean;
var
  fs: TStream;
begin
  Result := false;
  if filename = emptystr then
    exit;
  if FileExists(filename) = false then
  begin
    fs := TFileStream.Create(filename, fmCreate or fmOpenWrite or
      fmShareDenyWrite);
    fs.Free;
  end;

  fs := TFileStream.Create(filename, fmOpenWrite or fmShareDenyWrite);
  fs.Size := 0;
  try
    SL.SaveToStream(fs);
    Result := true;
  except
  end;
  fs.Free;
end;

function SL_LoadFromFile(const SL: TStrings; const filename: string)
  : boolean;
var
  fs: TStream;
begin
  Result := false;
  if filename = emptystr then
    exit;
  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
  try
    SL.LoadFromStream(fs);
    Result := true;
  except
  end;
  fs.Free;
end;

// Copies a file, based on a source and destination filename, and returns true
// if successful or false if the operation failed
function FileCopy(const source, dest: string): boolean;
{$IF CompilerVersion < 22}
var
  fSrc, fDst, len: integer;
  Size: LongInt;
  buffer: packed array [0 .. 2047] of Byte;
{$IFEND}
begin
{$IF CompilerVersion > 22}
  Result := true;
  if FileExists(source) and FileExists(dest) then
    DeleteFile(dest);
  try
    TFile.copy(source, dest);
  except
    Result := false;
  end;
{$ELSE}
  // By David Stidolph, 21 Jun 1995
  Result := false;
  if source <> dest then
  begin
    fSrc := FileOpen(source, fmOpenRead);
    if fSrc >= 0 then
    begin
      Size := FileSeek(fSrc, 0, 2);
      FileSeek(fSrc, 0, 0);
      fDst := FileCreate(dest);
      if fDst >= 0 then
      begin
        while Size > 0 do
        begin
          len := FileRead(fSrc, buffer, SizeOf(buffer));
          FileWrite(fDst, buffer, len);
          Size := Size - len;
        end;

        FileSetDate(fDst, FileGetDate(fSrc));
        FileClose(fDst);
        FileSetAttr(dest, FileGetAttr(source));

        Result := true;
      end;
      FileClose(fSrc);
    end;
  end;
{$IFEND}
end;

// Peter Below ------------------------------------------------------------//

// Based on an example from PB (4/5/1998)
procedure CopyAfterFirstLine(const sourcefile, targetfile: string;
  appendln: boolean = false; lnstr: string = '');
var
  s: string;
  source, Target: Textfile;
begin
  AssignFile(source, sourcefile);
  AssignFile(Target, targetfile);
  Reset(source);
  try
    ReWrite(Target);
    try
      CatReadLn(source, s);
      while not Eof(source) do
      begin
        CatReadLn(source, s);
        WriteLn(Target, s);
      end;
      if appendln then
        WriteLn(lnstr);
    finally
      Closefile(Target);
    end;
  finally
    Closefile(source);
  end;
end;

{
  If you want to get rid of a file normally you just delete it.
  But someone else can undelete it if the file hasn't been wiped correctly.
  For security purposes, to insure that certain files are permanently
  gone, the WipeFile procedure writes over the data in the file with
  random characters and then erases it.
}
procedure WipeFile(const filename: string); // PB
var
  buffer: array [0 .. 4095] of Byte;
  max, n: LongInt;
  i: integer;
  fs: TFileStream;

  procedure RandomizeBuffer;
  var
    i: integer;
  begin
    for i := Low(buffer) to High(buffer) do
      buffer[i] := Random(256);
  end;

begin
  fs := TFileStream.Create(filename, fmOpenReadWrite or fmShareExclusive);
  try
    for i := 1 to 3 do
    begin
      RandomizeBuffer;
      max := fs.Size;
      fs.Position := 0;
      while max > 0 do
      begin
        if max > SizeOf(buffer) then
          n := SizeOf(buffer)
        else
          n := max;
        fs.Write(buffer, n);
        max := max - n;
      end;
      FlushFileBuffers(fs.Handle);
    end;
  finally
    fs.Free;
  end;
  DeleteFile(filename);
end;

{
 GetDiskDrives, GetVolumeName, FindDiskDriveByVolumeName functions:
 Adapted from Steve F. and Gianluca Colombo's
 https://stackoverflow.com/questions/26303575/how-to-get-drive-letter-of-a-usb-memory-stick-drive-given-its-volume-label
}

procedure GetDiskDrives(var ADriveList: TStrings; IncludeRemovables:boolean=true);
var
  r: LongWord;
  Drives: array [0 .. 128] of Char;
  pDrive: pchar;
begin

  ADriveList.Clear;

  r := GetLogicalDriveStrings(sizeof(Drives), Drives);
  if r = 0 then
    exit;
  if r > sizeof(Drives) then
    raise Exception.Create(SysErrorMessage(ERROR_OUTOFMEMORY));
  pDrive := Drives; // Point to the first drive
  while pDrive^ <> #0 do
  begin
    if IncludeRemovables = false then begin
        if GetDriveType(pDrive) <> DRIVE_REMOVABLE then
          ADriveList.Add(pDrive);
    end else
    ADriveList.Add(pDrive);
    inc(pDrive, 4); // Point to the next drive
  end;
end;

function GetVolumeName(const ADriveLetter: Char): string;
var
  dummy: DWORD;
  buffer: array [0 .. MAX_PATH] of Char;
  oldmode: LongInt;
begin
  oldmode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    GetVolumeInformation(pchar(ADriveLetter + ':\'), buffer, sizeof(buffer), nil, dummy, dummy, nil, 0);
    Result := StrPas(buffer);
  finally
    SetErrorMode(oldmode);
  end;
end;

function FindDiskDriveByVolumeName(const AVolumeName: String;
  IncludeRemovables:boolean=true): String;
var
  dl: TStringList;
  c: Integer;
begin
  Result := emptystr;

  dl := TStringList.Create;
  try
    GetDiskDrives(TStrings(dl), IncludeRemovables);
    for c := 0 to dl.Count - 1 do
      if (AVolumeName = GetVolumeName(dl[c][1])) then
        Result := dl[c][1];

  finally
    dl.Free;
  end;

end;

{ **************************************************************************** }

type
  TDirInfo = record
    Name: string;
    CreationTime: TDateTime;
  end;

  TDirInfoArray = array of TDirInfo;

procedure QuickSortDirs(var A: TDirInfoArray; L, R: Integer; Ascending: Boolean);
var
  I, J: Integer;
  P, T: TDirInfo;
begin
  I := L;
  J := R;
  P := A[(L + R) div 2];

  repeat
    if Ascending then
    begin
      // Oldest -> Newest
      while A[I].CreationTime < P.CreationTime do Inc(I);
      while A[J].CreationTime > P.CreationTime do Dec(J);
    end
    else
    begin
      // Newest -> Oldest
      while A[I].CreationTime > P.CreationTime do Inc(I);
      while A[J].CreationTime < P.CreationTime do Dec(J);
    end;

    if I <= J then
    begin
      T := A[I];
      A[I] := A[J];
      A[J] := T;
      Inc(I);
      Dec(J);
    end;
  until I > J;

  if L < J then QuickSortDirs(A, L, J, Ascending);
  if I < R then QuickSortDirs(A, I, R, Ascending);
end;

procedure GetDirsByCreationTime(const dir: string; const Result: TStrings;
  Ascending: Boolean = True);
var
  sr: TSearchRec;
  Path, FullDir: string;
  Info: TDirInfoArray;
  Count, rc: Integer;
  fad: WIN32_FILE_ATTRIBUTE_DATA;
  ftLocal: TFileTime;
  st: TSystemTime;
  dt: TDateTime;
  i: Integer;
begin
  if Result = nil then Exit;
  Result.Clear;

  Path := IncludeTrailingPathDelimiter(dir);

  Count := 0;
  SetLength(Info, 0);

  rc := FindFirst(Path + '*.*', faDirectory, sr);
  try
    while rc = 0 do
    begin
      if ((sr.Attr and faDirectory) = faDirectory) and
         (sr.Name <> '.') and (sr.Name <> '..') then
      begin
        FullDir := Path + sr.Name;

        dt := 0;
        if GetFileAttributesEx(PChar(FullDir), GetFileExInfoStandard, @fad) then
          if FileTimeToLocalFileTime(fad.ftCreationTime, ftLocal) and
             FileTimeToSystemTime(ftLocal, st) then
            dt := SystemTimeToDateTime(st);

        Inc(Count);
        SetLength(Info, Count);
        Info[Count - 1].Name := sr.Name;
        Info[Count - 1].CreationTime := dt;
      end;

      rc := FindNext(sr);
    end;
  finally
    FindClose(sr);
  end;

  if Count = 0 then Exit;

  //Sort according to Ascending/Descending flag
  QuickSortDirs(Info, 0, Count - 1, Ascending);

  // Output names
  for i := 0 to Count - 1 do
    Result.Add(Info[i].Name);
end;

type
  TFileInfo = record
    Name: string;
    CreationTime: TDateTime;
  end;

  TFileInfoArray = array of TFileInfo;

procedure QuickSortFilesAsc(var A: TFileInfoArray; L, R: Integer);
var
  I, J: Integer;
  P, T: TFileInfo;
begin
  I := L;
  J := R;
  P := A[(L + R) div 2];

  repeat
    while A[I].CreationTime < P.CreationTime do Inc(I);
    while A[J].CreationTime > P.CreationTime do Dec(J);

    if I <= J then
    begin
      T := A[I];
      A[I] := A[J];
      A[J] := T;
      Inc(I);
      Dec(J);
    end;
  until I > J;

  if L < J then QuickSortFilesAsc(A, L, J);
  if I < R then QuickSortFilesAsc(A, I, R);
end;

procedure GetFilesByCreationTimeAsc(const dir: string; const Result: TStrings;
  const IncludeDir: Boolean = False; const IncludeExt: Boolean = True);
var
  rc: Integer;
  tmpPath, ffound, fullName: string;
  sr: TSearchRec;
  FileData: TFileInfoArray;
  Count: Integer;
  fad: WIN32_FILE_ATTRIBUTE_DATA;
  ftLocal: TFileTime;
  st: TSystemTime;
  dt: TDateTime;
  i: Integer;
begin
  if Result = nil then Exit;

  Result.Clear;
  tmpPath := IncludeTrailingPathDelimiter(ExtractFilePath(dir));
  Count := 0;
  SetLength(FileData, 0);

  rc := FindFirst(dir, faAnyFile, sr);
  try
    while rc = 0 do
    begin
      if (sr.Name <> '.') and (sr.Name <> '..') then
      begin
        ffound := sr.Name;
        fullName := tmpPath + sr.Name;

        dt := 0;
        if GetFileAttributesEx(PChar(fullName), GetFileExInfoStandard, @fad) then
          if FileTimeToLocalFileTime(fad.ftCreationTime, ftLocal) and
             FileTimeToSystemTime(ftLocal, st) then
            dt := SystemTimeToDateTime(st);

        if not IncludeExt then
          ffound := ChangeFileExt(ffound, '');

        Inc(Count);
        SetLength(FileData, Count);
        FileData[Count - 1].Name := ffound;
        FileData[Count - 1].CreationTime := dt;
      end;

      rc := FindNext(sr);
    end;
  finally
    FindClose(sr);
  end;

  if Count = 0 then Exit;

  QuickSortFilesAsc(FileData, 0, Count - 1);

  for i := 0 to Count - 1 do
    if IncludeDir then
      Result.Add(tmpPath + FileData[i].Name)
    else
      Result.Add(FileData[i].Name);
end;

procedure QuickSortFilesDesc(var A: TFileInfoArray; L, R: Integer);
var
  I, J: Integer;
  P, T: TFileInfo;
begin
  I := L;
  J := R;
  P := A[(L + R) div 2];

  repeat
    while A[I].CreationTime > P.CreationTime do Inc(I);
    while A[J].CreationTime < P.CreationTime do Dec(J);

    if I <= J then
    begin
      T := A[I];
      A[I] := A[J];
      A[J] := T;
      Inc(I);
      Dec(J);
    end;
  until I > J;

  if L < J then QuickSortFilesDesc(A, L, J);
  if I < R then QuickSortFilesDesc(A, I, R);
end;

procedure GetFilesByCreationTimeDesc(const dir: string; const Result: TStrings;
  const IncludeDir: Boolean = False; const IncludeExt: Boolean = True);
var
  rc: Integer;
  tmpPath, ffound, fullName: string;
  sr: TSearchRec;
  FileData: TFileInfoArray;
  Count: Integer;
  fad: WIN32_FILE_ATTRIBUTE_DATA;
  ftLocal: TFileTime;
  st: TSystemTime;
  dt: TDateTime;
  i: Integer;
begin
  if Result = nil then Exit;

  Result.Clear;
  tmpPath := IncludeTrailingPathDelimiter(ExtractFilePath(dir));
  Count := 0;
  SetLength(FileData, 0);

  rc := FindFirst(dir, faAnyFile, sr);
  try
    while rc = 0 do
    begin
      if (sr.Name <> '.') and (sr.Name <> '..') then
      begin
        ffound := sr.Name;
        fullName := tmpPath + sr.Name;

        dt := 0;
        if GetFileAttributesEx(PChar(fullName), GetFileExInfoStandard, @fad) then
          if FileTimeToLocalFileTime(fad.ftCreationTime, ftLocal) and
             FileTimeToSystemTime(ftLocal, st) then
            dt := SystemTimeToDateTime(st);

        if not IncludeExt then
          ffound := ChangeFileExt(ffound, '');

        Inc(Count);
        SetLength(FileData, Count);
        FileData[Count - 1].Name := ffound;
        FileData[Count - 1].CreationTime := dt;
      end;

      rc := FindNext(sr);
    end;
  finally
    FindClose(sr);
  end;

  if Count = 0 then Exit;

  QuickSortFilesDesc(FileData, 0, Count - 1);

  for i := 0 to Count - 1 do
    if IncludeDir then
      Result.Add(tmpPath + FileData[i].Name)
    else
      Result.Add(FileData[i].Name);
end;

procedure CollectFilesRecursive(const Buffer: TStrings; Dir, Mask: string);
var
  dirs: TStrings;
  SR: TSearchRec;
  f: Boolean;
  i: Integer;
begin
  if Dir = '' then Exit;
  if LastChar(Dir) <> PathDelim then
    Dir := Dir + PathDelim;

  // collect files in this dir
  f := FindFirst(Dir + Mask, faAnyFile - faDirectory, SR) = 0;
  try
    while f do
    begin
      Buffer.Add(Dir + SR.Name);
      f := FindNext(SR) = 0;
    end;
  finally
    FindClose(SR);
  end;

  // collect subdirs
  dirs := TStringList.Create;
  try
    f := FindFirst(Dir + '*.*', faAnyFile, SR) = 0;
    while f do
    begin
      if ((SR.Attr and faDirectory) <> 0) and (SR.Name <> '.') and (SR.Name <> '..') then
        dirs.Add(Dir + SR.Name);
      f := FindNext(SR) = 0;
    end;
    FindClose(SR);

    // recurse
    for i := 0 to dirs.Count - 1 do
      CollectFilesRecursive(Buffer, dirs[i], Mask);

  finally
    dirs.Free;
  end;
end;

procedure QuickSortFiles(var A: TFileInfoArray; L, R: Integer; Ascending: Boolean);
var
  I, J: Integer;
  P, T: TFileInfo;
begin
  I := L;
  J := R;
  P := A[(L + R) div 2];

  repeat
    if Ascending then
    begin
      // Oldest -> Newest
      while A[I].CreationTime < P.CreationTime do Inc(I);
      while A[J].CreationTime > P.CreationTime do Dec(J);
    end
    else
    begin
      // Newest -> Oldest
      while A[I].CreationTime > P.CreationTime do Inc(I);
      while A[J].CreationTime < P.CreationTime do Dec(J);
    end;

    if I <= J then
    begin
      T := A[I];
      A[I] := A[J];
      A[J] := T;
      Inc(I);
      Dec(J);
    end;
  until I > J;

  if L < J then QuickSortFiles(A, L, J, Ascending);
  if I < R then QuickSortFiles(A, I, R, Ascending);
end;

procedure GetFilesRecursiveByCreationTime(const Result: TStrings;
  Dir, Mask: string; Ascending: Boolean = True);
var
  Buffer: TStringList;
  FileData: TFileInfoArray;
  fad: WIN32_FILE_ATTRIBUTE_DATA;
  ftLocal: TFileTime;
  st: TSystemTime;
  dt: TDateTime;
  i, Count: Integer;
begin
  if Result = nil then Exit;

  // 1) Collect files unsorted
  Buffer := TStringList.Create;
  try
    CollectFilesRecursive(Buffer, Dir, Mask);

    Count := Buffer.Count;
    if Count = 0 then Exit;

    SetLength(FileData, Count);

    // 2) Read creation timestamps
    for i := 0 to Count - 1 do
    begin
      FileData[i].Name := Buffer[i];

      dt := 0;
      if GetFileAttributesEx(PChar(Buffer[i]), GetFileExInfoStandard, @fad) then
        if FileTimeToLocalFileTime(fad.ftCreationTime, ftLocal) and
           FileTimeToSystemTime(ftLocal, st) then
          dt := SystemTimeToDateTime(st);

      FileData[i].CreationTime := dt;
    end;

  finally
    Buffer.Free;
  end;

  // 3) Sort the file list by creation time
  QuickSortFiles(FileData, 0, Count - 1, Ascending);

  // 4) Output sorted result
  Result.Clear;
  for i := 0 to Count - 1 do
    Result.Add(FileData[i].Name);
end;

type
  TFolderInfo = record
    Name: string;
    CreationTime: TDateTime;
  end;

function SortFoldersByCreationDate(const BasePath, FolderNames: string): string;
var
  InfoList: array of TFolderInfo;
  SL: TStringList;
  I: Integer;

  function GetCreationTime(const Folder: string): TDateTime;
  var
    SR: TWin32FindData;
    H: THandle;
    FullPath: string;
    LFT: TFileTime;
    ST: TSystemTime;
  begin
    Result := 0;
    FullPath := IncludeTrailingPathDelimiter(BasePath) + Folder;

    H := FindFirstFile(PChar(FullPath), SR);
    if H <> INVALID_HANDLE_VALUE then
    try
      if (SR.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
        if FileTimeToLocalFileTime(SR.ftCreationTime, LFT) and
           FileTimeToSystemTime(LFT, ST) then
          Result := SystemTimeToDateTime(ST);
    finally
      {$IF CompilerVersion > 22}Winapi.{$IFEND}Windows.FindClose(H);
    end;
  end;

  procedure QuickSort(L, R: Integer);
  var
    I, J: Integer;
    Pivot: TDateTime;
    Temp: TFolderInfo;
  begin
    I := L;
    J := R;
    Pivot := InfoList[(L + R) div 2].CreationTime;

    repeat
      // NEWEST first
      while InfoList[I].CreationTime > Pivot do Inc(I);
      while InfoList[J].CreationTime < Pivot do Dec(J);

      if I <= J then
      begin
        Temp := InfoList[I];
        InfoList[I] := InfoList[J];
        InfoList[J] := Temp;
        Inc(I);
        Dec(J);
      end;
    until I > J;

    if L < J then QuickSort(L, J);
    if I < R then QuickSort(I, R);
  end;

begin
  Result := '';

  SL := TStringList.Create;
  try
    SL.Text := Trim(FolderNames);

    if SL.Count = 0 then
      Exit;

    SetLength(InfoList, SL.Count);

    // collect info
    for I := 0 to SL.Count - 1 do
    begin
      InfoList[I].Name := SL[I];
      InfoList[I].CreationTime := GetCreationTime(SL[I]);
    end;

    if Length(InfoList) > 1 then
      QuickSort(0, High(InfoList));

    // rebuild list in sorted order
    SL.Clear;
    for I := 0 to High(InfoList) do
      SL.Add(InfoList[I].Name);

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

// ------------------------------------------------------------------------//
end.
