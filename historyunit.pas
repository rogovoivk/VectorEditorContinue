unit HistoryUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Grids, LCLIntf, LCLType, Buttons, GraphMath, Math, Spin,
  FPCanvas, TypInfo, LCL, FiguresUnit, ScaleUnit, Laz2_DOM, laz2_XMLRead, laz2_XMLWrite, LCLProc;

type

  { THistoryBlock }

  THistoryBlock = class
    SavedFigures: string;
    procedure LoadFigures();
    constructor Create(S:string);
  end;

  //SHistoryBlock = ^THistoryBlock;

procedure SendToHistory();

var
  History: array of THistoryBlock;
  Current:Integer;
  Saved:integer;

implementation

procedure SendToHistory();
begin
  Current:=Current+1;
  SetLength(History,Current+1);
  History[Current]:=THistoryBlock.Create(FiguresToString(true));
  if Saved>=Current then Saved:=0;
end;

procedure THistoryBlock.LoadFigures();
var
  S: TStringStream;
  Doc: TXMLDocument;
  b: boolean;
begin
  s := TStringStream.Create(self.SavedFigures);
  s.Position:=0;
  ReadXMLFile(Doc, s);
  b := XMLToFigures(Doc);
  doc.Free;
  s.Free;
end;

constructor THistoryBlock.Create(S: string);
begin
  SavedFigures:=s;
end;

end.

