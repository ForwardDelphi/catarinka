{
  Catarinka INI-Like class to work with YAML files
  Copyright (c) 2003-2023 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

unit CatYINI;

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils,
{$ELSE}
  Classes, SysUtils,
{$ENDIF}
  CatStrings, CatStringList, CatStringLoop, Neslib.LibYaml, Neslib.Yaml;

type
  TYIniList = class
  protected
  private
    fDoc:IYamlDocument;
    fParsingErrors:string;
    fSingleDocument:boolean;
    function GetText:string;
    function FindNode(const keyname:string):TYamlNode;
  public
    function LoadFromFile(const filename:string):boolean;
    function LoadFromString(const yml:string):boolean;
    procedure SaveToFile(const filename:string);
    procedure ReadSectionKeys(const Section:string; outsl:TStrings);
    procedure ReadSectionValues(const Section:string; outsl:TStrings);
    function ReadSectionValuesAsString(const Section:string):string;
    function ReadBool(const Section,Key:string;const DefaultValue:boolean):boolean; virtual;
    function ReadString(const Section,Key:string;const DefaultValue:string=''):string; virtual;
    function ReadStringEx(const Section,Key:string;const DefaultValue:string=''):TCatFuncResult;
    procedure WriteString(const Section,Key,Value:string);
    function ValueExists(const Section,Key:string):boolean;
    constructor Create; virtual;
    destructor Destroy; override;
    property Document: IYamlDocument read fDoc;
    property ParsingErrors:string read fParsingErrors;
    property SingleDocument: boolean read fSingleDocument write fSingleDocument;
    property Text: string read GetText;
  end;

implementation

{ TYIniList }

function TYIniList.GetText:string;
begin
  result := fDoc.ToYaml;
  if fSingleDocument = true then begin
    if beginswith(result,'---') then begin
    result := after(result, '---');
    result := trim(result);
    end;
    if endswith(result,'...') then begin
    result := Copy(result, 1, Length(result) - 3);
    end;
  end;
end;

function TYIniList.LoadFromString(const yml:string):boolean;
begin
  result := true;
  try
    if trim(yml) = emptystr then begin
    fDoc := TYamlDocument.Parse(emptystr);
    fDoc := TYamlDocument.CreateMapping;
    end else
    fDoc := TYamlDocument.Parse(trim(yml));
    fDoc.Root.MappingStyle := TYamlMappingStyle.Any;
  except
    on E : Exception do begin
    fParsingErrors := e.message;
    result := false;
    end;
  end;

end;

function TYIniList.LoadFromFile(const filename:string):boolean;
var sl:TCatStringList;
begin
  sl := TCatStringList.Create;
  sl.LoadFromFile(filename);
  result := LoadFromString(sl.text);
  sl.Free;
end;

procedure TYIniList.SaveToFile(const filename:string);
begin
  fDoc.Save(filename);
end;

procedure EnumerateNodeKeys(Node: TYamlNode; outsl:TStrings);
var
  i:integer;
begin
  if Node.IsNil = false then
  begin
    for i := 0 to Node.Count -1 do
      outsl.Add(node.Elements[i].Key.ToString);
  end;
end;

procedure GetNodeKeyValues(Node: TYamlNode; outsl:TStrings);
var
  i:integer;
begin
  if Node.IsNil = false then
  begin
    if Node.Count = 0 then
      outsl.Text := Node else begin
       for i := 0 to Node.Count -1 do
        outsl.values[node.Elements[i].Key.ToString]:=node.Elements[i].Value.ToString;
      end;
  end;
end;

// allows to use @key.subkey to indicate subkey
function TYIniList.FindNode(const keyname:string):TYamlNode;
var
 slp:TSepStringLoop;
 akeyname:string;
begin
  result := fDoc.Root;
  if beginswith(keyname,'@') = true then begin
    akeyname := after(keyname,'@');
    slp := TSepStringLoop.Create(akeyname,'.');
    while slp.Found do
      result := result.Values[slp.Current];
    slp.Free;
  end;
end;

procedure TYIniList.ReadSectionKeys(const Section:string;outsl:TStrings);
begin
  outsl.Clear;
  EnumerateNodeKeys(FindNode(section),outsl);
  outsl.Text := trim(outsl.text);
end;

procedure TYIniList.ReadSectionValues(const Section:string;outsl:TStrings);
begin
  outsl.Clear;
  GetNodeKeyValues(FindNode(section),outsl);
  outsl.Text := trim(outsl.Text);
end;

function TYIniList.ReadSectionValuesAsString(const Section:string):string;
var sl:TStringList;
begin
  sl := TCatStringList.create;
  ReadSectionValues(Section, sl);
  result := sl.text;
  sl.Free;
end;

function TYIniList.ReadBool(const Section,Key:string;const DefaultValue:boolean):boolean;
var
  n:TYamlNode;
begin
  n := fDoc.Root.Values[section].Values[key];
  if n.IsNil then
    result := defaultvalue else
    result := StrToBool(n);
end;

function TYIniList.ReadString(const Section,Key:string;const DefaultValue:string=''):string;
var
  n:TYamlNode;
begin
  n := fDoc.Root.Values[section].Values[key];
  if n.IsNil then
    result := defaultvalue else
    result := n;
end;

function TYIniList.ReadStringEx(const Section,Key:string;const DefaultValue:string=''):TCatFuncResult;
var
  n:TYamlNode;
begin
  result.b := false;
  n := fDoc.Root.Values[section].Values[key];
  if n.IsNil then begin
    result.s := defaultvalue;
    result.b := false;
  end else begin
    result.s := n;
    result.b := true;
  end;
end;

procedure TYIniList.WriteString(const Section,Key,Value:string);
var
  n:TYamlNode;
begin
   n := fDoc.Root.Values[section];
   if n.IsNil then begin
     n := fDoc.Root.AddOrSetMapping(section);
   end;
   n.AddOrSetValue(key,value);
end;

function TYIniList.ValueExists(const Section,Key:string):boolean;
var
  n:TYamlNode;
begin
  n := fDoc.Root.Values[section].Values[key];
  result := not n.IsNil;
end;

constructor TYIniList.Create;
begin
  inherited;
      fDoc := TYamlDocument.Parse(emptystr);
    fDoc := TYamlDocument.CreateMapping;
    fDoc.Root.MappingStyle := TYamlMappingStyle.Any;
  //LoadFromString(emptystr);
  fParsingErrors := emptystr;
  fSingleDocument := true;
end;

destructor TYIniList.Destroy;
begin
  fDoc := nil;
  inherited;
end;

end.
