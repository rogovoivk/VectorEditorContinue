unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, ActnList, ColorBox, Spin, ToolsUnit, FiguresUnit, Buttons,
  ExtDlgs, ScaleUnit, HistoryUnit, LCLType, Laz2_DOM;

type

  { TEditor }

  TPBPic = (NONE, PNG, BMP, JPG);

  TEditor = class(TForm)
    ButtonUndo: TButton;
    Redo: TButton;
    ButtonCopy: TButton;
    ButtonPaste: TButton;
    MenuItem1: TMenuItem;
    MenuItemUndo: TMenuItem;
    MenuItemRedo: TMenuItem;
    OpenDialog1: TOpenDialog;
    Save: TMenuItem;
    SaveAs: TMenuItem;
    Open: TMenuItem;
    SaveDialog: TSaveDialog;
    SelectAll: TButton;
    DeleteSelected: TButton;
    SelectedUp: TButton;
    SelectedDown: TButton;
    ShowAllButton: TButton;
    Clear: TButton;
    Back: TButton;
    ZoomLabel: TLabel;
    ScrollBarHorizontal: TScrollBar;
    ScrollBarVertical: TScrollBar;
    ToolPanel: TPanel;
    ButtonPanel: TPanel;
     // Shift: TShiftState; X, Y: Integer);
    MMenu: TMainMenu;
    MFile: TMenuItem;
    MAbout: TMenuItem;
    MExit: TMenuItem;
    SMAbout: TMenuItem;
    PB: TPaintBox;
    ZoomSpinEdit: TSpinEdit;
    procedure BackClick(Sender: TObject);
    procedure ButtonCopyClick(Sender: TObject);
    procedure ButtonPasteClick(Sender: TObject);
    procedure ButtonsDown(Sender: TObject);
    procedure ButtonUndoClick(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure DeleteSelectedClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItemRedoClick(Sender: TObject);
    procedure MenuItemUndoClick(Sender: TObject);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; ACanvas: TCanvas);
    procedure FormPaint(Sender: TObject);
    procedure MExitClick(Sender: TObject);
    procedure OpenClick(Sender: TObject);
    procedure PBClick(Sender: TObject);
    procedure RedoClick(Sender: TObject);
    procedure SaveAsClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
    procedure SelectedDownClick(Sender: TObject);
    procedure SelectedUpClick(Sender: TObject);
    procedure SMAboutClick(Sender: TObject);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ScrollBarScroll(Sender: TObject;
      ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure ShowAllButtonClick(Sender: TObject);
    procedure ToolPanelClick(Sender: TObject);
    procedure ZoomChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);


  private
    { private declarations }
    mCurrentPBPic: TPBPic;
    paintBoxBitmap: TBitmap;
     paintBoxPng: TPortableNetworkGraphic;
    paintBoxJpg: TJPEGImage;
  public
    { public declarations }
  end;

var
  Editor: TEditor;
  isDrawing: boolean;
  CurrentTool: TFigureTool;
  FileName: string;

implementation

{$R *.lfm}

{procedure TMainForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  86: if shift = [ssCtrl] then
        MenuItemPasteClick(Sender);
end;}

procedure TEditor.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case key of
    VK_Z: if CtrlButtonState then
        if ShiftButtonState then
        begin
          if Current < length(History) - 1 then
          begin
            Inc(Current);
            History[Current].LoadFigures();
            Drawing := False;
            //ChoosenTool.CreateParams();
            Invalidate;
            //SelectedNumber := 0;
          end;
        end
        else
        begin
          if Current > 0 then
          begin
            Current := Current - 1;
            History[Current].LoadFigures();
            Drawing := False;
            //ChoosenTool.CreateParams();
            Invalidate;
            //SelectedNumber := 0;
          end;
        end;
  end;
end;

{ TEditor }


procedure TEditor.MExitClick(Sender: TObject);
begin
Close();
end;


procedure TEditor.OpenClick(Sender: TObject);
var
  bmpPic: TBitmap;
  pngPic: TPortableNetworkGraphic;
  jpgPic: TJPEGImage;
  openxml: boolean=True;
begin
if OpenDialog1.Execute then
  if (TFigure.LoadFile(OpenDialog1.FileName)) then begin
     Editor.Caption:= OpenDialog1.FileName + ' - ';
     FileName:= OpenDialog1.FileName;
     IsSaved:= True;
  end;
  Invalidate;
end;

procedure TEditor.PBClick(Sender: TObject);
begin

end;

procedure TEditor.RedoClick(Sender: TObject);
var
  NilXML: array [0..1] of TXMLDocument;
begin
if (HistoryPosition>1) then
    TFigure.OperationRedo
else
  if arrayHistory[Length(arrayHistory)] = arrayHistory[Length(arrayHistory)-1] then begin
    HistoryPosition:=0;
    SetLength(Figures,0);
  end;
Invalidate;
end;

procedure TEditor.ButtonUndoClick(Sender: TObject);
begin
  TFigure.OperationUndo;
  Invalidate;
end;

procedure TEditor.SaveAsClick(Sender: TObject);
var
  Save2XML: boolean;
  TestFormat:string;
  BitMap: TBitmap;
  Dest: TRect;
  Source: TRect;
  PNG: TPortableNetworkGraphic;
  JPEG: TJPEGImage;
  s: string;
  NRec: pointer;
begin
if SaveDialog.Execute then
begin
  case copy(SaveDialog.FileName , Length(SaveDialog.FileName) - 2, 3) of
    'bmp':
      begin
        Save2XML:=True;
        Bitmap := TBitmap.Create;
        try
          with Bitmap do
          begin
            Width := PB.Width;
            Height := PB.Height;
            Dest := Rect(0, 0, Width, Height);
          end;
          with PB do
            Source := Rect(0, 0, Width, Height);
            Bitmap.Canvas.CopyRect(Dest, PB.Canvas, Source);
            Bitmap.SaveToFile(SaveDialog.FileName);
        finally
          Bitmap.Free;
        end;
      end;
     'png':
      begin
        Save2XML:=True;
        PNG := TPortableNetworkGraphic.Create;
        try
          with PNG do
          begin
            PNG.Width := PB.Width;
            PNG.Height := PB.Height;
            Dest := Rect(0, 0, Width, Height);
          end;
          with PB do
            Source := Rect(0, 0, Width, Height);
            PNG.Canvas.CopyRect(Dest, PB.Canvas, Source);
            PNG.SaveToFile(SaveDialog.FileName);
        finally
          PNG.Free;
        end;
      end;
      'jpg':
      begin
        Save2XML:=True;
        JPEG := TJPEGImage.Create;
        try
          with JPEG do
          begin
            Width := PB.Width;
            Height := PB.Height;
            Dest := Rect(0, 0, Width, Height);
          end;
          with PB do
            Source := Rect(0, 0, Width, Height);
            JPEG.Canvas.CopyRect(Dest, PB.Canvas, Source);
            JPEG.SaveToFile(SaveDialog.FileName);
        finally
          JPEG.Free;
        end;
      end;
  end;
end;

  if Save2XML=True then
  begin
    TFigure.SaveFile(SaveDialog.FileName);
    Editor.Caption:= SaveDialog.FileName + ' - ' ;
    FileName:= SaveDialog.FileName;
    IsSaved:= True;
    // SavedToCurrent();
  end;
end;

procedure TEditor.SaveClick(Sender: TObject);
begin
  if FileName = '' then
  SaveAs.Click
  else begin
    Editor.Caption:= FileName + ' - ' ;
    //TFigure.SaveFile(FileName);
    //IsSaved:= True;
  end;
end;

procedure TEditor.SelectAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to High(Figures) do Figures[i].Selected := True;
  Invalidate;
end;

procedure TEditor.SelectedDownClick(Sender: TObject);
var
  i, j, k: Integer;
  Figure: TFigure;
begin
  k := 0;
  for i := high(Figures) downto 0 do
  begin
    if (Figures[i].Selected) then
    begin
      for j := i downto k + 1  do
      begin
        Figure := Figures[j];
        Figures[j] := Figures[j-1];
        Figures[j-1] := Figure;
        k := j
      end;
    end;
  end;
  Invalidate;
end;

procedure TEditor.SelectedUpClick(Sender: TObject);
var
  i, j, k: Integer;
  Figure: TFigure;
begin
  k := high(Figures);
  for i := 0 to high(Figures) do
  begin
    if (Figures[i].Selected) then
    begin
      for j := i to k - 1 do
      begin
        Figure := Figures[j];
        Figures[j] := Figures[j+1];
        Figures[j+1] := Figure;
        k := j
      end;
    end;
  end;
  Invalidate;
end;

procedure TEditor.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; ACanvas: TCanvas);
var
  ParamPanel: TPanel;
begin
  IsDrawing := False;
  Invalidate;
  if HelpPaw<>0 then
    CurrentTool.MouseUp(X, Y, PB.Canvas);

  if SelectedCreateParamFlag then begin
    ParamPanel := TPanel.Create(Editor);
    ParamPanel.Top := 1;
    Parampanel.LeFt := 355;
    ParamPanel.Width := 550;
    ParamPanel.Height := 120;
    ParamPanel.Parent := ToolPanel;
    SelectedFigure.ParamsCreate(ParamPanel);
  end;
  SelectedCreateParamFlag:= False;
  if (CheckChange=True) and (isSaved= True) then
    Editor.Caption:= SaveDialog.FileName + ' - изменено';

  CheckChange:=False;
  Invalidate;
end;

procedure TEditor.FormCreate(Sender: TObject);
var
  i: integer;
  ToolButton: TSpeedButton;
  ToolIcon: TBitMap;
begin
  CurrentTool := TPolyLineTool.Create();
  Zoom := 100;

  for i:=0 to high(Tool) do begin
  ToolButton := TSpeedButton.Create(Editor);
  ToolButton.Width := 40;
  ToolButton.Height := 40;
  ToolButton.Top := (i div 5) * 50;
  ToolButton.Left := (i mod 5) * 60;
  ToolButton.Parent := ButtonPanel;
  ToolButton.Tag := i;
  ToolButton.OnClick := @ButtonsDown;
  if i=0 then ToolButton.Click();
  ToolIcon := TBitmap.Create;
  with TPicture.create do
    begin
    LoadFromFile(Tool[i].Icons);
    ToolIcon.Assign(Graphic);
    end;
  ToolButton.Glyph := ToolIcon;
  end;
  Invalidate_:=@Invalidate;
end;

procedure TEditor.MenuItem1Click(Sender: TObject);
begin

end;

procedure TEditor.MenuItemRedoClick(Sender: TObject);
begin


end;

procedure TEditor.MenuItemUndoClick(Sender: TObject);
begin

end;

procedure TEditor.ButtonsDown(Sender: TObject);
var
  Parampanel: TPanel;
  i: Integer;
begin
  CurrentTool := Tool[(Sender as TSpeedbutton).tag];
  ParamPanel := TPanel.Create(Editor);
  ParamPanel.Top := 1;
  Parampanel.LeFt := 350;
  ParamPanel.Width := 550;
  ParamPanel.Height := 120;
  ParamPanel.Parent := ToolPanel;
  CurrentTool.ParamsCreate(ParamPanel);

  for i := 0 to High(Figures) do
  if not ((Sender as TSpeedbutton).tag = 8) then Figures[i].Selected := False;
  Invalidate;
end;


procedure TEditor.BackClick(Sender: TObject);
begin
  if Length(Figures)<>0 then SetLength(Figures, Length(figures) - 1);
  Invalidate;
end;

procedure TEditor.ButtonCopyClick(Sender: TObject);
begin
  //SaveDialog.FileName:='C:\Users\Vladislav Rogovoi\Desktop\VectorEditor\CopySelect';
  {FileCopy:= xml;
  TFigure.SaveFile(FileCopy)
  TFigure.copySelected;}

  TFigure.CopySelected('C:\Users\Vladislav Rogovoi\Desktop\VectorEditor v2\copyselected');
  Invalidate;

end;

procedure TEditor.ButtonPasteClick(Sender: TObject);
begin
  TFigure.pasteSelected;
  Invalidate;
  TFigure.History;
end;


procedure TEditor.ClearClick(Sender: TObject);
begin
  SetLength(Figures,0);
  Invalidate;
end;

procedure TEditor.DeleteSelectedClick(Sender: TObject);
var
  i, j: Integer;
begin
  j := 0;
  for i := 0 to high(Figures) do
  begin
    if (Figures[i].Selected) then
    FreeAndNil(Figures[i])
    else
    begin
      Figures[j] := Figures[i];
      j := j + 1;
    end;
  end;
  setLength(Figures, j);
  TFigure.History;
  Invalidate;
end;

procedure TEditor.FormPaint(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to high(Figures) do begin
  Figures[i].Draw(PB.Canvas);
  if Figures[i].Selected then Figures[i].DrawSelection(Figures[i], PB.Canvas, (Figures[I] AS TLittleFigure).WIDTH);
  end;
  ScrollBarVertical.Max := trunc(MaxPoint.Y);
  ScrollBarVertical.Min := trunc(MinPoint.Y);
  ScrollBarHorizontal.Max := trunc(MaxPoint.X);
  ScrollBarHorizontal.Min := trunc(MinPoint.X);
  ZoomSpinEdit.Value := zoom;
  AHeightPB := PB.Height;
  AWidthPB := PB.Width;
end;

procedure TEditor.SMAboutClick(Sender: TObject);
begin
  ShowMessage('Роговой Владислав - Б8103б - векторный редактор');
end;

procedure TEditor.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  IsDrawing := true;
  CurrentTool.MouseDown(X, Y);
  MaxMin(ScreenToWorld(Point(X,Y)));

  Invalidate;
end;

procedure TEditor.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if IsDrawing then  begin
    CurrentTool.MouseMove(X, Y);
    MaxMin(ScreenToWorld(Point(X,Y)));
    Invalidate;
  end;
end;

procedure TEditor.ScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  Offset := Point(ScrollBarHorizontal.Position, ScrollBarVertical.Position);
  Invalidate;
end;

procedure TEditor.ShowAllButtonClick(Sender: TObject);
begin
  RectZoom(pB.Height,PB.Width,MinPoint,MaxPoint);
  Invalidate;
  ScrollBarVertical.Max:=trunc(MaxPoint.Y);
  ScrollBarVertical.Min:=trunc(MinPoint.Y);
  ScrollBarHorizontal.Max:=trunc(MaxPoint.X);
  ScrollBarHorizontal.Min:=trunc(MinPoint.X);
  Offset.X:=0;
  Offset.Y:=0;
end;

procedure TEditor.ToolPanelClick(Sender: TObject);
begin

end;

procedure TEditor.ZoomChange(Sender: TObject);
begin
  Zoom := ZoomSpinEdit.Value;
  Invalidate;
end;

begin
end.

