{
  Catarinka TStringListCache
  Caches multiple string lists, allowing them to be loaded or saved together
  to an external JSON file

  Copyright (c) 2003-2025 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

unit CatStringListCache;

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils, unitObjectCache, CatJSON;
{$ELSE}
  Classes, SysUtils, unitObjectCache, CatJSON;
{$ENDIF}

type
  TStringListCache = class
  protected
    fCache: TObjectCache;
    fNameList: TStringList;
  public
    function GetList(const AName: string): TStringList;
    procedure ClearLists; overload;
    procedure ClearLists(const Key: array of string); overload;
    procedure CopyListsFrom(var Source: TStringListCache; const Key: array of string);
    procedure LoadFromFile(const AFilename: string);
    procedure LoadFromString(const JSON: string);
    procedure SaveToFile(const AFilename: string);
    constructor Create(ACapacity: Integer = 1000; OwnsObjects: boolean = true);
    destructor Destroy; override;
    property Cache: TObjectCache read fCache;
    property Lists[const AName: string]: TStringList read GetList; default;
  end;

type
  TCachedStringList = class(TStringList)
  private
    fID:string;
  public
    property ID: string read fID write fID;
  end;

implementation

uses CatStringLoop;

const
  cCacheKeyPrefix = 'cache.';
  cIDListKey = 'idlist';

// Copies the values of specific lists from a TStringListCache
procedure TStringListCache.CopyListsFrom(var Source: TStringListCache;
  const Key: array of string);
var
  i: integer;
begin
  for i := Low(Key) to High(Key) do
    if not(Key[i] = emptystr) then
      Lists[Key[i]].Text := Source[Key[i]].Text;
end;

procedure TStringListCache.ClearLists;
var
  obj: TObject;
  csl: TCachedStringList;
  m, c: Integer;
begin
  m := fCache.Count;
  for c := m - 1 downto 0 do
  begin
    obj := fCache.ObjectAt(c);
    if obj is TCachedStringList then
    begin
      csl := TCachedStringList(obj);
      csl.Clear;
    end;
  end;
end;

procedure TStringListCache.ClearLists(const Key: array of string);
var
  X: Integer;
begin
  for X := Low(Key) to High(Key) do
    if not(Key[X] = '') then
      GetList(Key[X]).Clear;
end;

procedure TStringListCache.LoadFromFile(const AFilename: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.LoadFromFile(AFilename);
  LoadFromString(sl.Text);
  sl.Free;
end;

procedure TStringListCache.LoadFromString(const JSON: string);
var
  slp: TStringLoop;
  sl: TStringList;
  j: TCatJSON;
begin
  fCache.Clear;
  fNameList.Clear;
  j := TCatJSON.Create(JSON);
  slp := TStringLoop.Create(j[cIDListKey]);
  while slp.Found do
  begin
    sl := GetList(slp.Current);
    sl.Text := j[cCacheKeyPrefix + slp.Current];
  end;
  slp.Free;
  j.Free;
end;

procedure TStringListCache.SaveToFile(const AFilename: string);
var
  j: TCatJSON;
  obj: TObject;
  csl: TCachedStringList;
  m, c: Integer;
begin
  j := TCatJSON.Create;
  try
    j.SetValue(cIDListKey, fNameList.Text);
    m := fCache.Count;
    for c := m - 1 downto 0 do
    begin
      obj := fCache.ObjectAt(c);
      if obj is TCachedStringList then
      begin
        csl := TCachedStringList(obj);
        j.SetValue(cCacheKeyPrefix + csl.ID, csl.Text);
      end;
    end;
    j.SaveToFile(AFilename);
  finally
    j.Free;
  end;
end;

function TStringListCache.GetList(const AName: string): TStringList;
var
  obj: TObject;
  csl: TCachedStringList;
  m, c: Integer;
begin
  m := fCache.Count;
  for c := m - 1 downto 0 do
  begin
    obj := fCache.ObjectAt(c);
    if obj is TCachedStringList then
    begin
      csl := TCachedStringList(obj);
      if csl.ID = AName then
        Exit(csl);
    end;
  end;

  // Not found -> create and register
  csl := TCachedStringList.Create;
  csl.ID := AName;
  fCache.Add(csl);
  fNameList.Add(AName);
  Result := csl;
end;

constructor TStringListCache.Create(ACapacity: Integer = 1000;
  OwnsObjects: boolean = true);
begin
  fCache := TObjectCache.Create(ACapacity, OwnsObjects);
  fNameList := TStringList.Create;
end;

destructor TStringListCache.Destroy;
begin
  fNameList.Free;
  fCache.Free;
  inherited;
end;

end.
