// This is a fork of TInfinityObjectCache from unitObjectCache,
// Originally by Colin Wilson
// It adds the following modifications: Unlimited objects
(*======================================================================*
 |                                                                      |
 | TInfinityObjectCache         Implements an unlimited cache of        |
 |                              objects                                 |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2003  All Rights Reserved                   |
 |                                                                      |
 *======================================================================*)

unit CatInfinityObjCache;

interface

uses
  Windows, Classes, SysUtils, ConTnrs, SyncObjs, Types;

type
  TInfinityObjectCacheProc = procedure (obj : TObject; idx, param : Integer; var continue : boolean) of object;

//---------------------------------------------------------------
  TInfinityObjectCache = class
  private
    fObjects : TObjectList;
    function GetOwnsObjects: boolean;
    procedure SetOwnsObjects(const Value: boolean);
    function GetCount: Integer;
  protected
    function CanRemove (AObject : TObject) : boolean; virtual;
    function Matches (ObjA, ObjB : TObject) : boolean; virtual;
  public
    constructor Create (OwnsObjects : boolean);
    destructor Destroy; override;

    function IndexOfObject (AObject : TObject) : Integer;
    procedure Add (AObject : TObject); virtual;
    procedure Clear;

    function ForEach (proc : TInfinityObjectCacheProc; param : Integer) : TObject;
    function ForEachIdx (proc : TInfinityObjectCacheProc; param : Integer) : Integer;

    procedure BringToFrontObject (idx : Integer);
    function ObjectAt (idx : Integer) : TObject;

    procedure Remove (AObject : TObject);
    function Extract (AObject : TObject) : TObject;

    procedure Push (AObject : TObject);
    function Pop : TObject;

    // NEW: delete by index
    procedure Delete(Index: Integer);

    property OwnsObjects : boolean read GetOwnsObjects write SetOwnsObjects;
    property Count : Integer read GetCount;
  end;

implementation

{ TInfinityObjectCache }

{
procedure TInfinityObjectCache.Add(AObject: TObject);
var
  idx : Integer;
begin
  idx := IndexOfObject (AObject);
  if idx = 0 then
  begin
    if OwnsObjects then
      AObject.Free;
    Exit
  end;

  if idx = -1 then
  begin
    fObjects.Insert (0, AObject)
  end
  else
  begin
    BringToFrontObject (idx);
    if OwnsObjects then
      AObject.Free
  end
end; }

procedure TInfinityObjectCache.Add(AObject: TObject);
var
  idx: Integer;
begin
  idx := IndexOfObject(AObject);
  if idx <> -1 then
  begin
    // already in list – either do nothing,
    // or BringToFrontObject(idx) if you still want MRU *without* changing
    // the index semantics. For the .zcc editor I'd just "Exit".
    Exit;
  end;

  fObjects.Add(AObject);
end;

procedure TInfinityObjectCache.BringToFrontObject(idx: Integer);
var
  b : boolean;
  obj : TObject;
begin
  if (idx > 0) then
  begin
    obj := fObjects [idx];
    b := OwnsObjects;
    OwnsObjects := False;
    try
      fObjects.Delete (idx);
      fObjects.Insert (0, obj)
    finally
      OwnsObjects := b
    end
  end
end;

function TInfinityObjectCache.CanRemove(AObject: TObject): boolean;
begin
  result := True;
end;

procedure TInfinityObjectCache.Clear;
var
  i : Integer;
begin
  i := 0;
  while i < fObjects.Count do
    if CanRemove (fObjects [i]) then
      fObjects.Delete(i)
    else
      Inc (i)
end;

constructor TInfinityObjectCache.Create(OwnsObjects: boolean);
begin
  fObjects := TObjectList.Create (OwnsObjects);
end;

destructor TInfinityObjectCache.Destroy;
begin
  fObjects.Free;
  inherited;
end;

function TInfinityObjectCache.Extract(AObject: TObject): TObject;
begin
  result := fObjects.Extract(AObject);
end;

function TInfinityObjectCache.ForEach(proc: TInfinityObjectCacheProc;
  param: Integer): TObject;
var
  i : Integer;
  continue : boolean;
begin
  i := 0;
  continue := True;

  while continue and (i < fObjects.Count) do
  begin
    proc (fObjects [i], i, param, continue);
    if continue then
      Inc (i)
  end;

  if not continue then
    result := fObjects [i]
  else
    result := Nil
end;

function TInfinityObjectCache.ForEachIdx(proc: TInfinityObjectCacheProc;
  param: Integer): Integer;
var
  i : Integer;
  continue : boolean;
begin
  i := 0;
  continue := True;

  while continue and (i < fObjects.Count) do
  begin
    proc (fObjects [i], i, param, continue);
    if continue then
      Inc (i)
  end;

  if not continue then
    result := i
  else
    result := -1
end;

function TInfinityObjectCache.GetCount: Integer;
begin
  result := fObjects.Count;
end;

function TInfinityObjectCache.GetOwnsObjects: boolean;
begin
  result := fObjects.OwnsObjects;
end;

function TInfinityObjectCache.IndexOfObject(AObject: TObject): Integer;
var
  i, c : Integer;
begin
  result := -1;
  c := fObjects.Count;
  for i := 0 to c - 1 do
    if Matches (fObjects [i], AObject) then
    begin
      result := i;
      break
    end;
end;

function TInfinityObjectCache.Matches(ObjA, ObjB: TObject): boolean;
begin
  result := ObjA = ObjB;
end;

function TInfinityObjectCache.ObjectAt(idx: Integer): TObject;
begin
  result := fObjects [idx]
end;

function TInfinityObjectCache.Pop: TObject;
begin
  if Count > 0 then
  begin
    result := fObjects [0];
    Extract (result)
  end
  else
    result := Nil
end;

procedure TInfinityObjectCache.Push(AObject: TObject);
begin
  Add (AObject);
end;

procedure TInfinityObjectCache.Remove(AObject: TObject);
begin
  if (fObjects.Count > 0) and CanRemove (AObject) then
    fObjects.Remove(AObject)
end;

procedure TInfinityObjectCache.SetOwnsObjects(const Value: boolean);
begin
  fObjects.OwnsObjects := Value;
end;

procedure TInfinityObjectCache.Delete(Index: Integer);
begin
  if (Index >= 0) and (Index < fObjects.Count) then
    if CanRemove(fObjects[Index]) then
      fObjects.Delete(Index);
end;

end.
