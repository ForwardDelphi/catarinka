unit CatListEditor;
{
 Catarinka TCatListEditor
 A fork of super.pas (TSuperList 1.7) with enhancements
 Copyright (c) 2003-2025 Felipe Daragon

 License: 4-clause BSD, same license as the original code by Spanware Inc.

 Original Author: David Koretzky
 Copyright (c) 1999 Spanware Inc.
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.ImgList, Vcl.Buttons, Vcl.Menus, Vcl.Clipbrd, System.Types;
{$ELSE}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ImgList, Buttons, Menus, Clipbrd, Types;
{$ENDIF}

type
  TListBoxAlignment = (lbBottom, lbLeft ,lbRight, lbTop);    //added all four alignments
  TEnabledButton = (Plus, Minus);
  FEnabledButtons = set of TEnabledButton;

type TCatListEditorImageIndex = record // FD
    AddImageIndex:integer;
    DelImageIndex:integer;
    MoreImageIndex:integer;
  end;

type
  TCatListEditor = class(TCustomControl)
  private
    //the list box
    FListBox : TListBox;
    FListBoxAlignment : TListBoxAlignment;
    FListBoxChanged : Boolean;

    //panel for holding the buttons
    FPanel : TPanel;

    //the add and delete buttons
    FAddButton : TSpeedButton;
    FDeleteButton : TSpeedButton;
    FMoreButton : TSpeedButton;
    FMorePopupMenu : TPopupMenu;
    FLoadFromFileItem:TMenuItem;
    FSaveToFileItem:TMenuItem;
    FPasteFromClipboardItem:TMenuItem;
    FSortItem:TMenuItem;
    FClearItemSeparator:TMenuItem;
    FPasteItemSeparator:TMenuItem;
    FClearItem:TMenuItem;
    FSaveDialog:TSaveDialog;
    FOpenDialog:TOpenDialog;

    //the events
    FOnAddButtonClick: TNotifyEvent;
    FOnDeleteButtonClick: TNotifyEvent;
    FOnListBoxDoubleClick : TNotifyEvent;
    FOnListBoxKeyDown : TNotifyEvent;
    FOnListBoxChanged : TNotifyEvent; // FD

    FAddString : string;
    FAddCaption : string;
    FEditString : string;
    FEditCaption : string;

    FMaxLength : Integer;  //max length of string in listbox
    FMaxCount : Integer; //max number of entries allowed in listbox
    FAllowBlanks : boolean; //are blanks allowed in the listbox
    FUseKeyMaps : boolean; //use default key strokes for adding, editing and deleting???

    FLabel : TLabel;
    FStartingPoint : TPoint; // FD
    FCustomImageList: TCustomImageList; // FD
    FCustomImageIndex: TCatListEditorImageIndex;// FD

    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure AddBtnClick(Sender : TObject);
    procedure DeleteBtnClick(Sender : TObject);
    procedure ListBoxDoubleClick(Sender : TObject);
    procedure ListBoxKeyDown(Sender : TObject; var Key : word; shift : TShiftState);
    procedure ListBoxDragDrop(Sender, Source: TObject; X, Y: Integer); // FD
    procedure ListBoxDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); // FD
    procedure ListBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); // FD
    procedure SaveToFileItemClick(Sender : TObject);
    procedure LoadFromFileItemClick(Sender : TObject);
    procedure ClearItemClick(Sender : TObject);
    procedure MoreBtnClick(Sender:TObject);
    procedure PasteFromClipboardItemClick(Sender:TObject);
    procedure SortItemClick(Sender:TObject);

    procedure CreateButtons;
    procedure CreatePanel;
    procedure EnableButtons;

    procedure SetAlignment(value : TListBoxAlignment);

    function GetListBoxFont : TFont;
    procedure SetListBoxFont(value : TFont);

    function GetListBoxItems : TStrings;
    procedure SetListBoxItems(value : TStrings);

    function GetListBoxItemIndex : Integer;
    procedure SetListBoxItemIndex(value : integer);

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Delete;
    function GetCurrentString : String;
    property ListBoxItemIndex : Integer read GetListBoxItemIndex write SetListBoxItemIndex;
    property ListBox : TListBox read FListBox write FListBox;
    property ListBoxChanged : boolean read FListBoxChanged write FListBoxChanged default false;
    procedure AddItems(value : TStrings);
    procedure SetCustomImageList(ilist:TCustomImageList); // FD
    procedure SetCustomImageListAltIndex(ilist:TCustomImageList;imgindex:TCatListEditorImageIndex); // FD
  published
    property AddPrompt : string read FAddString write FAddString;
    property AddCaption : string read FAddCaption write FAddCaption;
    property Align;
    property AllowBlanks : boolean read FAllowBlanks write FAllowBlanks default false;
    property CustomImageList: TCustomImageList read FCustomImageList write SetCustomImageList; // FD
    property EditPrompt : string read FEditString write FEditString;
    property EditCaption : string read FEditCaption write FEditCaption;
    property EditLabel : TLabel read FLabel; // FD
    property ListBoxAlignment : TListBoxAlignment read FListBoxAlignment write SetAlignment default lbBottom;
    property ListBoxFont : TFont read GetListBoxFont write SetListBoxFont;
    property ListBoxItems : TStrings read GetListBoxItems write SetListBoxItems;
    property MaxLength : Integer read FMaxLength write FMaxLength default 0;
    property MaxCount : Integer read FMaxCount write FMaxCount default 0;
    // events
    property OnAddClick: TNotifyEvent read FOnAddButtonClick write FOnAddButtonClick;
    property OnDeleteClick: TNotifyEvent read FOnDeleteButtonClick write FOnDeleteButtonClick;
    property OnListBoxDoubleClick : TNotifyEvent read FOnListBoxDoubleClick write FOnListBoxDoubleClick;
    property OnListBoxKeyDown : TNotifyEvent read FOnListBoxKeyDown write FOnListBoxKeyDown;
    property OnListBoxChanged: TNotifyEvent read FOnListBoxChanged write FOnListBoxChanged; // FD
    property UseDefaultKeyMaps : boolean read FUsekeyMaps write FUseKeyMaps default false;
  end;

procedure Register;
function ShowEditListDialog(const PrevText:string;const egtext:string;const TituloJanela: TCaption; const TituloLabel: TCaption; var S: String):boolean;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TCatListEditor]);
end;

function ShowEditListDialog(const PrevText:string;const egtext:string;const TituloJanela: TCaption; const TituloLabel: TCaption; var S: String):boolean;
var
Form: TForm;
Edt: TCatListEditor;
begin
  Result := false;
  Form := TForm.Create(Application);
  try
  Form.BorderStyle := bsDialog;
  Form.Caption := TituloJanela;
  Form.Position := poScreenCenter;
  Form.Width := 350;
  Form.Height := 220;

  with TLabel.Create(Form) do
  begin
    Parent := Form;
    Caption := TituloLabel;
    Left := 10;
    Top := 10;
  end;

  Edt := TCatListEditor.Create(Form);
  with Edt do
  begin
    Parent := Form;
    ListboxItems.Text := PrevText;
    FLabel.Caption:=egtext;
    Left := 10;
    Top := 25;
    height := 119;
    Width := Form.ClientWidth -20;
  end;

  with TButton.Create(Form) do
  begin
    Parent := Form;
    Caption := '&OK';
    Left := trunc((Form.ClientWidth)/2)-100;
    Top := 155;
    default:=true;
    modalresult:=mrOk;
    //Kind := bkOK;
  end;

  with TButton.Create(Form) do
  begin
    Parent := Form;
    Caption := 'Cancel';
    Left := trunc((Form.ClientWidth)/2)+30;
    Top := 155;
    modalresult:=mrCancel;
  end;

  if Form.ShowModal = mrOK then
  begin
    S := Edt.Listboxitems.Text;
    Result := true;
  end;
  finally
  Form.Free;
end;
end;

function NewInputBox(const TituloJanela:TCaption; const TituloLabel:TCaption; const PrevText:string):string;
var
Form: TForm;
Edt: TEdit;
begin
  Result := prevtext;
  Form := TForm.Create(Application);
  try
  Form.BorderStyle := bsDialog;
  Form.Caption := TituloJanela;
  Form.Position := poScreenCenter;
  Form.Width := 350;
  Form.Height := 120;

  with TLabel.Create(Form) do
  begin
    Parent := Form;
    Caption := TituloLabel;
    Left := 10;
    Top := 10;
  end;

  Edt := TEdit.Create(Form);
  with Edt do
  begin
    Parent := Form;
    Text := PrevText;
    Left := 10;
    Top := 25;
    Width := Form.ClientWidth -20;
  end;

  with TButton.Create(Form) do
  begin
    Parent := Form;
    Caption := '&OK';
    Left := trunc((Form.ClientWidth)/2)-100;
    Top := 55;
    default:=true;
    modalresult:=mrOk;
  end;

  with TButton.Create(Form) do
  begin
    Parent := Form;
    Caption := 'Cancel';
    Left := trunc((Form.ClientWidth)/2)+30;
    Top := 55;
    modalresult:=mrCancel;
  end;

  if Form.ShowModal = mrOK then
  result := Edt.Text;

  finally
  Form.Free;
end;
end;

procedure TCatListEditor.SetCustomImageList(ilist:TCustomImageList);
begin
   FCustomImageList := ilist;
   FAddButton.Glyph.Assign(nil);
   FDeleteButton.Glyph.Assign(nil);
   FMoreButton.Glyph.Assign(nil);
   if FCustomImageList = nil then
     exit;
   with FCustomImageList do begin
     GetBitmap(FCustomImageIndex.AddImageIndex, fAddButton.Glyph);
     GetBitmap(FCustomImageIndex.DelImageIndex, fDeleteButton.Glyph);
     GetBitmap(FCustomImageIndex.MoreImageIndex, fMoreButton.Glyph);
   end;
end;

procedure TCatListEditor.SetCustomImageListAltIndex(ilist:TCustomImageList;imgindex:TCatListEditorImageIndex);
begin
   FCustomImageIndex := imgindex;
   SetCustomImageList(ilist);
end;

constructor TCatListEditor.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Width := 250;
  Height := 200;
  ControlStyle := ControlStyle + [csAcceptsControls];

  FListBox := TListBox.Create(Self);
  FListBox.Parent := Self;
  FListbox.ctl3d:=true;
  FListBox.Visible := true;
  FListBox.Width := 250; FListBox.Height := 170;
  FListBox.Top := 20; FListBox.Left := 0;
  FListBox.Align := alClient;
  FListBox.OnDblClick := ListBoxDoubleClick;
  FListBox.OnKeyDown := ListBoxKeyDown;
  FListBox.DragMode := dmAutomatic; // FD
  FListBox.OnDragDrop := ListBoxDragDrop; // FD
  FListBox.OnDragOver := ListBoxDragOver; // FD
  FListBox.OnMouseDown := ListBoxMouseDown; // FD

  FCustomImageIndex.AddImageIndex := 0;
  FCustomImageIndex.DelImageIndex := 1;
  FCustomImageIndex.MoreImageIndex := 2;

  CreatePanel;
  CreateButtons;

  FAddCaption := 'Input';
  FAddString := 'Enter New Value:';
  FEditCaption := 'Edit';
  FEditString := 'Edit Value';
end;


procedure TCatListEditor.CreatePanel;
begin
  FPanel := TPanel.Create(Self);
  FPanel.Parent := Self;
  FPanel.Visible := true;
  FPanel.Align := alTop;
  FPanel.Caption := emptystr;
  FPanel.BevelInner := bvNone;
  FPanel.BevelOuter := bvNone;
  FPanel.Height := 35;

  FLabel := TLabel.Create(Self);
  FLabel.Parent := FPanel;
  Flabel.Font.Style:=[fsBold];
  FLabel.Visible := true;
  Flabel.Caption:=emptystr;
  Flabel.Left:=0;
  Flabel.Top:=8;

end;

procedure TCatListEditor.ListBoxDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  DropPosition, StartPosition: Integer;
  DropPoint: TPoint;
begin
  DropPoint.X := X;
  DropPoint.Y := Y;
  with Source as TListBox do
  begin
    StartPosition := ItemAtPos(fStartingPoint,True) ;
    DropPosition := ItemAtPos(DropPoint,True) ;
    Items.Move(StartPosition, DropPosition) ;
  end;
end;

procedure TCatListEditor.ListBoxDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source = fListBox;
end;

procedure TCatListEditor.ListBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fStartingPoint.X := X;
  fStartingPoint.Y := Y;
end;

procedure TCatListEditor.LoadFromFileItemClick(Sender:TObject);
begin
 if not FOpenDialog.Execute then exit;
 if fileexists(Fopendialog.FileName) then
 Flistbox.Items.LoadFromFile(Fopendialog.FileName);
end;

procedure TCatListEditor.SaveToFileItemClick(Sender:TObject);
begin
 if not FSaveDialog.Execute then exit;
 try Flistbox.Items.SaveToFile(Fsavedialog.FileName); except end;
end;

procedure TCatListEditor.ClearItemClick(Sender:TObject);
begin
 Flistbox.Items.clear;
end;

procedure TCatListEditor.PasteFromClipboardItemClick(Sender:TObject);
begin
 if FListbox.Items.Text = emptystr then
   FListbox.Items.Text := Clipboard.AsText
 else
   Flistbox.Items.Text := FListbox.Items.Text + #13 + #10 + Clipboard.AsText;
end;

procedure TCatListEditor.SortItemClick(Sender:TObject);
begin
  FListbox.Sorted := false; FListbox.Sorted := true;
end;

procedure TCatListEditor.MoreBtnClick(Sender:TObject);
var
  P : TPoint;
begin
  P := FmoreButton.ClientToScreen(Point(0, FmoreButton.Height));
  FMorePopupMenu.Popup(P.x, P.y);
end;                

procedure TCatListEditor.CreateButtons;
const
 cDefFilter='List files (*.lst)|*.lst|Text files (*.txt)|*.txt|All files (*.*)|*.*';
begin
  FMorePopupMenu := TPopupMenu.Create(Self);
  FLoadFromFileItem:=tmenuitem.create(FMorePopupMenu);
  FLoadFromFileItem.Caption:='Load from file...';
  FLoadFromFileItem.OnClick:=LoadFromFileItemClick;
  FSaveToFileItem:=tmenuitem.create(FMorePopupMenu);
  FSaveToFileItem.Caption:='Save to file...';
  FSaveToFileItem.OnClick:=SaveToFileItemClick;
  FPasteFromClipboardItem:=tmenuitem.create(FMorePopupMenu);
  FPasteFromClipboardItem.Caption:='Paste from Clipboard';
  FPasteFromClipboardItem.OnClick:=PasteFromClipboardItemClick;
  FSortItem := tmenuitem.create(FMorePopupMenu);
  FSortItem.Caption := 'Sort';
  FSortItem.OnClick := SortItemClick;
  FClearItem:=tmenuitem.create(FMorePopupMenu);
  FClearItem.Caption:='Clear';
  FClearItem.OnClick:=ClearItemClick;
  FClearItemSeparator:=tmenuitem.create(FMorePopupMenu);
  FClearItemSeparator.Caption:='-';
  FPasteItemSeparator:=tmenuitem.create(FMorePopupMenu);
  FPasteItemSeparator.Caption:='-';
  FMorePopupMenu.Items.Add(FLoadFromFileItem);
  FMorePopupMenu.Items.Add(FSaveToFileItem);
  FMorePopupMenu.items.Add(FPasteItemSeparator);
  FMorePopupMenu.Items.Add(FPasteFromClipboardItem);
  FMorePopupMenu.Items.Add(FSortItem);
  FMorePopupMenu.items.Add(FClearItemSeparator);
  FMorePopupMenu.items.Add(FClearItem);
  FSaveDialog:=TSaveDialog.Create(Self);
  FOpenDialog:=TOpenDialog.Create(Self);
  FOpenDialog.DefaultExt:='lst'; FSaveDialog.DefaultExt:=FOpenDialog.DefaultExt;
  FOpenDialog.Filter:=cDefFilter;
  FSaveDialog.Filter:=cDefFilter;

  FmoreButton := TSpeedButton.Create(Self);
  FmoreButton.Width := 30;
  FmoreButton.Height := 25;
  FmoreButton.Font.Name := 'MS Sans Serif Bold';
  FmoreButton.Font.Size := 8;
  FmoreButton.Caption := 'More';
  FmoreButton.Parent := FPanel;
  FmoreButton.Top := 5; FMoreButton.Left := Self.Width - 95;
  FmoreButton.OnClick:=MoreBtnClick;

  FaddButton := TSpeedButton.Create(Self);
  FAddButton.Width := 25;
  FAddButton.Height := 25;
  FaddButton.Font.Name := 'MS Sans Serif Bold';
  FaddButton.Font.Size := 8;
  FaddButton.Caption := '+';
  FaddButton.Parent := FPanel;
  FaddButton.Top := 5; FaddButton.Left := Self.Width - 60;
  FaddButton.OnClick := AddBtnClick;

  FdeleteButton := TSpeedButton.Create(Self);
  FdeleteButton.Width := 25;
  FDeleteButton.Height := 25;
  FdeleteButton.Font.Name := 'MS Sans Serif Bold';
  FdeleteButton.Font.Size := 8;
  FdeleteButton.Caption := '-';
  FdeleteButton.Parent := FPanel;
  FdeleteButton.Top := 5; FdeleteButton.Left := Self.Width - 30;
  FDeleteButton.OnClick := DeleteBtnClick;
end;

destructor TCatListEditor.Destroy;
begin
  FSaveDialog.free;
  FOpenDialog.free;
  FLoadFromFileItem.free;
  FSaveToFileItem.free;
  FSortItem.Free;
  FPasteFromClipboardItem.free;
  FPasteItemSeparator.free;
  FClearItemSeparator.free;
  FClearItem.free;
  FMorePopupMenu.free;
  FListBox.Free;
  FaddButton.Free;
  FdeleteButton.Free;
  Fmorebutton.free;
  FLabel.free;
  FPanel.Free;
  inherited;
end;

procedure TCatListEditor.WMSize(var Message: TWMSize);
var
  w, h: Integer;
begin
  inherited;

  w := Width;
  h := Height;

  if (w < 75) then Width := 75;
  if (h < 75) then Height := 75;

  case FListBoxAlignment of
    lbBottom, lbTop:
      begin
        FPanel.Height := 35;
        FMoreButton.Left := Self.Width -95;
        FMoreButton.Top := 5;
        FaddButton.Left := Self.Width - 60;
        FaddButton.Top := 5;
        FdeleteButton.Left := Self.Width - 30;
        FdeleteButton.Top := 5;
      end;
    lbLeft, lbRight:
      begin
        FPanel.Width := 35;
        FMoreButton.Left := 5;
        FMoreButton.Top := FPanel.Top +5;
        FAddButton.Left := 5;
        FAddButton.Top := FPanel.Top + 5;
        FDeleteButton.Left := 5;
        FDeleteButton.Top := FPanel.Top + 35;
      end;
  end;

end;

procedure TCatListEditor.AddBtnClick(Sender: TObject);
var s : string;
begin
  s := NewInputBox(FAddCaption, FAddString, '');
    if (Length(s) <= FMaxLength ) or (FMaxLength <= 0) then
      if (FAllowBlanks) or ((not FAllowBlanks) and (Length(s) > 0)) then
      begin
        FListBox.Items.Add(s);
        FListBoxChanged := true;
        if Assigned(FOnListBoxChanged) then
          FOnListBoxChanged(Self);
      end;
  if Assigned(FOnAddButtonClick) then FOnAddButtonClick(Self);
  EnableButtons;
end;

procedure TCatListEditor.DeleteBtnClick(Sender : TObject);
begin
   if (FListBox.ItemIndex > -1) then
   begin
    FListBox.Items.Delete( FListBox.ItemIndex );
    FListBoxChanged := true;
    if Assigned(FOnListBoxChanged) then
      FOnListBoxChanged(Self);
   end;
   if Assigned(FOnDeleteButtonClick) then FOnDeleteButtonClick(Self);
   //EnableButtons;
end;

procedure TCatListEditor.EnableButtons;
begin
   FAddButton.Enabled := (FListBox.Items.Count < FMaxCount) or  (FMaxCount <= 0);
   FDeleteButton.Enabled := (FListBox.Items.Count > 0);
end;

procedure TCatListEditor.ListBoxDoubleClick(Sender : TObject);
var s : string;
begin
  if (FListBox.ItemIndex < 0) then
    exit;

  s := NewInputBox(FEditCaption, FEditString, FListBox.Items[FListBox.ItemIndex]);
  if ( s <> FListBox.Items[FListBox.ItemIndex] ) then
  begin
    if (Length(s) < FMaxLength) or (FMaxLength <= 0) then
      if (FAllowBlanks) or ((not FAllowBlanks) and (Length(s) > 0)) then
      begin
        FListBox.Items [FListBox.ItemIndex] := s;
        FListBoxChanged := true;
        if Assigned(FOnListBoxChanged) then
          FOnListBoxChanged(Self);
      end;
    if Assigned(FOnListBoxDoubleClick) then FOnListBoxDoubleClick(Self);
   end;
end;

procedure TCatListEditor.SetAlignment(value : TListBoxAlignment );
begin
  FListBoxAlignment := value;
  FListBox.Align := alClient;
  case value of
    lbLeft: FPanel.Align := alRight;
    lbTop:  FPanel.Align := alBottom;
    lbRight : FPanel.Align := alLeft;
    lbBottom : FPanel.Align := alTop;
  end;

  SendMessage(Self.Handle, WM_SIZE, 0, 0); //call resize to realign everything
end;

function TCatListEditor.GetListBoxFont : TFont;
begin
  result := FListBox.Font;
end;

procedure TCatListEditor.SetListBoxFont(value : TFont);
begin
  FListBox.Font.Assign(value);
end;

function TCatListEditor.GetListBoxItems : TStrings;
begin
  result := FListBox.Items;
end;

procedure TCatListEditor.SetListBoxItems(value : TStrings);
begin
  FListBox.Items.Assign(value);
end;

procedure TCatListEditor.SetListBoxItemIndex(value : integer);
begin
  FListBox.ItemIndex := value;
end;

function TCatListEditor.GetListBoxItemIndex : integer;
begin
  result := FListBox.ItemIndex;
end;

procedure TCatListEditor.Delete;
begin
  if (FListBox.ItemIndex > -1) then
  begin
    FListBox.Items.Delete(FListBox.ItemIndex);
    FListBoxChanged := true;
    if Assigned(FOnListBoxChanged) then
      FOnListBoxChanged(Self);
  end;
end;

function TCatListEditor.GetCurrentString : String;
begin
  if (FListBox.ItemIndex > -1) then
    result := FListBox.Items[ FListBox.ItemIndex]
  else
    result := '';
end;


procedure TCatListEditor.ListBoxKeyDown(Sender : TObject; var Key : word; shift : TShiftState);
begin
  if (FUseKeyMaps) then
  begin
    if (ssCtrl in shift) then
    case key of
      ord('a'), ord('A') :  AddBtnClick(Sender);
      ord('e'), ord('E') :  ListBoxDoubleClick(Sender);
      ord('d'), ord('D') :  DeleteBtnClick(Sender);
    end;

    if (key = 13) then
      ListBoxDoubleClick(Sender);

  end;

  if Assigned(FOnListBoxKeyDown) then
    FOnListBoxKeyDown(Sender);
end;

procedure TCatListEditor.AddItems(value : TStrings);
begin
  FListBox.Items.AddStrings(value);
end;

end.
