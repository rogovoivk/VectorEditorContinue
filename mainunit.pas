unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, ActnList, ColorBox, Spin, ToolsUnit, FiguresUnit, Buttons,
  ExtDlgs, ScaleUnit, LCLType, Laz2_DOM;

type

  { TEditor }

  TPBPic = (NONE, PNG, BMP, JPG);

  TEditor = class(TForm)
    TheTextFont: TButton;
    ButtonRedo: TButton;
    FontDialog1: TFontDialog;
    Undo: TButton;
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
    procedure TheTextFontClick(Sender: TObject);
    procedure ButtonCopyClick(Sender: TObject);
    procedure ButtonPasteClick(Sender: TObject);
    procedure ButtonsDown(Sender: TObject);
    procedure ButtonRedoClick(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure DeleteSelectedClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer; ACanvas: TCanvas);
    procedure FormPaint(Sender: TObject);
    procedure MExitClick(Sender: TObject);
    procedure OpenClick(Sender: TObject);
    procedure UndoClick(Sender: TObject);
    procedure SaveAsClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
    procedure SelectedDownClick(Sender: TObject);
    procedure SelectedUpClick(Sender: TObject);
    procedure SMAboutClick(Sender: TObject);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
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
  IsLoaded:Boolean=False;

implementation

{$R *.lfm}

{procedure TMainForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  86: if shift = [ssCtrl] then
        MenuItemPasteClick(Sender);
end;}

procedure TEditor.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin


  {case key of
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
  end;}
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
  openxml: boolean = True;
  i: Integer;
  NilXML: TXMLDocument;
begin

  SetLength(Figures, 0);
  if OpenDialog1.Execute then
    if (TFigure.LoadFile(OpenDialog1.FileName)) then
    begin
      Editor.Caption := OpenDialog1.FileName + ' - ';
      FileName := OpenDialog1.FileName;
      IsSaved := True;
      for i:=1 to Length(arrayHistory) do
        arrayHistory[i]:=NilXML;
      IsLoaded:=True;
    end;
  Invalidate;
end;


procedure TEditor.UndoClick(Sender: TObject);
var
  NilXML: array [0..1] of TXMLDocument;
begin
  if (HistoryPosition > 1) then
    TFigure.OperationUndo
  else
  if arrayHistory[Length(arrayHistory)] = arrayHistory[Length(arrayHistory) - 1] then
  begin
    if IsLoaded=False then begin
      if SizeHistory<2
        then SizeHistory:=1;
      HistoryPosition := 0;
      SetLength(Figures, 0);
      WasUndo:=True;
    end;
  end;
  Invalidate;
  Undo.Enabled:=False;
  Undo.Enabled:=True;
end;

procedure TEditor.ButtonRedoClick(Sender: TObject);
begin
  TFigure.OperationRedo;
  Invalidate;
  ButtonRedo.Enabled:=False;
  ButtonRedo.Enabled:=True;
end;

procedure TEditor.SaveAsClick(Sender: TObject);
var
  Save2XML: boolean;
  TestFormat: string;
  BitMap: TBitmap;
  Dest: TRect;
  Source: TRect;
  PNG: TPortableNetworkGraphic;
  JPEG: TJPEGImage;
  s: string;
  NRec: pointer;
  k:Integer;
begin
  try
  if SaveDialog.Execute then
  begin
    case copy(SaveDialog.FileName, Length(SaveDialog.FileName) - 3, 4) of
      '.bmp':
      begin
        Save2XML := True;
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
      '.png':
      begin
        Save2XML := True;
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
      '.jpg':
      begin
        Save2XML := True;
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

  if Save2XML = False then
  begin
    TFigure.SaveFile(SaveDialog.FileName);
    Editor.Caption := SaveDialog.FileName + ' - ';
    FileName := SaveDialog.FileName;
    IsSaved := True;
    // SavedToCurrent();
  end;
  except
    on EFCreateError do inc(k);
  end;
end;

procedure TEditor.SaveClick(Sender: TObject);
begin
  if FileName = '' then
    SaveAs.Click
  else
  begin
    Editor.Caption := FileName + ' - ';
    //TFigure.SaveFile(FileName);
    //IsSaved:= True;
  end;
end;

procedure TEditor.SelectAllClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(Figures) do
    Figures[i].Selected := True;
  Invalidate;
  SelectAll.Enabled:=False;
  SelectAll.Enabled:=True;
end;

procedure TEditor.SelectedDownClick(Sender: TObject);
var
  i, j, k: integer;
  Figure: TFigure;
begin
  k := 0;
  for i := high(Figures) downto 0 do
  begin
    if (Figures[i].Selected) then
    begin
      for j := i downto k + 1 do
      begin
        Figure := Figures[j];
        Figures[j] := Figures[j - 1];
        Figures[j - 1] := Figure;
        k := j;
      end;
    end;
  end;
  Invalidate;
  SelectedDown.Enabled:=False;
  SelectedDown.Enabled:=True;
end;

procedure TEditor.SelectedUpClick(Sender: TObject);
var
  i, j, k: integer;
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
        Figures[j] := Figures[j + 1];
        Figures[j + 1] := Figure;
        k := j;
      end;
    end;
  end;
  Invalidate;
  SelectedUp.Enabled:=False;
  SelectedUp.Enabled:=True;
end;

procedure TEditor.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer; ACanvas: TCanvas);
var
  ParamPanel: TPanel;
begin
  IsDrawing := False;
  Invalidate;
  if HelpPaw <> 0 then
    CurrentTool.MouseUp(X, Y, PB.Canvas);

  if SelectedCreateParamFlag then
  begin
    ParamPanel := TPanel.Create(Editor);
    ParamPanel.Top := 1;
    Parampanel.LeFt := 355;
    ParamPanel.Width := 550;
    ParamPanel.Height := 120;
    ParamPanel.Parent := ToolPanel;
    SelectedFigure.ParamsCreate(ParamPanel);
  end;
  SelectedCreateParamFlag := False;
  if (CheckChange = True) and (isSaved = True) then
    Editor.Caption := SaveDialog.FileName + ' - изменено';

  CheckChange := False;
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

  for i := 0 to high(Tool) do
  begin
    ToolButton := TSpeedButton.Create(Editor);
    ToolButton.Width := 50;
    ToolButton.Height := 50;
    ToolButton.Top := (i div 5) * 55;
    ToolButton.Left := 10+(i mod 5) * 65;
    ToolButton.Parent := ButtonPanel;
    ToolButton.Tag := i;
    ToolButton.OnClick := @ButtonsDown;
    //ToolButton.:=True;
    ToolButton.Flat:=True;
    if i = 0 then
      ToolButton.Click();
    ToolIcon := TBitmap.Create;
    with TPicture.Create do
    begin
      LoadFromFile(Tool[i].Icons);
      ToolIcon.Assign(Graphic);
    end;
    ToolButton.Glyph := ToolIcon;
  end;
  Invalidate_ := @Invalidate;
end;

procedure TEditor.FormKeyPress(Sender: TObject; var Key: char);
var
  f:TRectangleText;
  i: Integer;
  st: String;
  NilXML: array [0..1] of TXMLDocument;
begin
  case Key of
  #26:
  begin
    if (HistoryPosition > 1) then
      TFigure.OperationUndo
    else
    if arrayHistory[Length(arrayHistory)] = arrayHistory[Length(arrayHistory) - 1] then
    begin
      if IsLoaded=False then
      begin
        HistoryPosition := 0;
        SetLength(Figures, 0);
        WasUndo:=True;
      end;
    end;
    Invalidate;
    end;
  #25:
  begin
    TFigure.OperationRedo;
    Invalidate;
  end;
  #3:
  begin
    TFigure.CopySelected;
    Invalidate;
  end;
  #22:
  begin
    if copied<>nil then begin
      TFigure.pasteSelected;
      Invalidate;
      TFigure.History;
    end;
  end;
  else
if DoText=True then
begin


    ABrushColor :=clWhite;
    Text2History:=True;

  if ((Figures[high(Figures)]).selected)and(Figures[high(Figures)] is TRectangleText) then
  begin
    if key= #13 then
    begin
      For i:=0 to high(Figures) do
        if (Figures[i]).selected then
          Figures[i].selected:= not Figures[i].Selected;
      DoText:=False;
      Text2History:=False;
      TFigure.History;
    end
    else
    if key=#8 then begin
      //for i:=0 to high((Figures[high(Figures)] as TRectangleText).t)-1 do
        //st:=st+(Figures[high(Figures)] as TRectangleText).t[i];
      (Figures[high(Figures)] as TRectangleText).t:=copy
      ((Figures[high(Figures)] as TRectangleText).t,1,length((Figures[high(Figures)] as TRectangleText).t)-1);

      //f:=(Figures[high(Figures)] as TRectangleText).t;
      //(Figures[high(Figures)] as TRectangleText).t:=f;
    end
    else
    begin
      (Figures[high(Figures)] as TRectangleText).t+=key;
      f:= (Figures[high(Figures)] as TRectangleText);
    end;
  end;
  Invalidate;
end;
end;
end;

procedure TEditor.ButtonsDown(Sender: TObject);
var
  Parampanel: TPanel;
  i: integer;
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
    if not ((Sender as TSpeedbutton).tag = 8) then
      Figures[i].Selected := False;
  Invalidate;
end;


procedure TEditor.BackClick(Sender: TObject);
begin

end;

procedure TEditor.TheTextFontClick(Sender: TObject);
begin
    if FontDialog1.Execute then
  begin
    wasFont:=True;
    TextFont:=FontDialog1.Font;
  end;
  TheTextFont.Enabled:=False;
  TheTextFont.Enabled:=True;
  Invalidate;
end;

procedure TEditor.ButtonCopyClick(Sender: TObject);
begin
  //SaveDialog.FileName:='C:\Users\Vladislav Rogovoi\Desktop\VectorEditor\CopySelect';
  {FileCopy:= xml;
  TFigure.SaveFile(FileCopy)
  TFigure.copySelected;}

  TFigure.CopySelected;
  Invalidate;
  Clear.Enabled:=False;
  Clear.Enabled:=True;

end;

procedure TEditor.ButtonPasteClick(Sender: TObject);
begin
  if copied<>nil then begin
    TFigure.pasteSelected;
    Invalidate;
    TFigure.History;
    ButtonCopy.Enabled:=False;
    ButtonCopy.Enabled:=True;
  end;
end;


procedure TEditor.ClearClick(Sender: TObject);
begin
  SetLength(Figures, 0);
  Invalidate;
  Clear.Enabled:=False;
  Clear.Enabled:=True;
end;

procedure TEditor.DeleteSelectedClick(Sender: TObject);
var
  i, j: integer;
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
  DeleteSelected.Enabled:=False;
  DeleteSelected.Enabled:=True;
end;

procedure TEditor.FormPaint(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to high(Figures) do
  begin
    Figures[i].Draw(PB.Canvas);
    if Figures[i].Selected then
      Figures[i].DrawSelection(Figures[i], PB.Canvas, (Figures[I] as TLittleFigure).Width);
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
  Shift: TShiftState; X, Y: integer);
begin
  IsDrawing := True;
  CurrentTool.MouseDown(X, Y);
  MaxMin(ScreenToWorld(Point(X, Y)));

  Invalidate;
end;

procedure TEditor.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if IsDrawing then
  begin
    CurrentTool.MouseMove(X, Y);
    MaxMin(ScreenToWorld(Point(X, Y)));
    Invalidate;
  end;
end;

procedure TEditor.ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: integer);
begin
  Offset := Point(ScrollBarHorizontal.Position, ScrollBarVertical.Position);
  Invalidate;
end;

procedure TEditor.ShowAllButtonClick(Sender: TObject);
begin
  RectZoom(pB.Height, PB.Width, MinPoint, MaxPoint);
  Invalidate;
  ScrollBarVertical.Max := trunc(MaxPoint.Y);
  ScrollBarVertical.Min := trunc(MinPoint.Y);
  ScrollBarHorizontal.Max := trunc(MaxPoint.X);
  ScrollBarHorizontal.Min := trunc(MinPoint.X);
  Offset.X := 0;
  Offset.Y := 0;
  ShowAllButton.Enabled:=False;
  ShowAllButton.Enabled:=True;
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
