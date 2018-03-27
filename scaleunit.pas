unit ScaleUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GraphMath,Graphics, Menus, ActnList,LCLType, LCLIntf, LCL;

function WorldToScreen(APoint: TFloatPoint): TPoint;
function ScreenToWorld(APoint: TPoint): TFloatPoint;
procedure MaxMin(APoint: TFloatPoint);
procedure RectZoom(AHeight, AWidth: extended; MinPoint, MaxPoint: TFloatPoint);
function MoveTo(P: TPoint): TFloatPoint;
function CasePenStyle(Index: integer): TPenStyle;
function CaseBrushStyle(Index: integer): TBrushStyle;
var
  zoom: double;
  Offset: TPoint;
  MinPoint, MaxPoint: TFloatPoint;
  AHeight, AWidth: extended;
  AHeightPB, AWidthPB: extended;

implementation

function MoveTo(P: TPoint): TFloatPoint;
begin
  Result.X := (P.x + Offset.x) / Zoom * 100;
  Result.Y := (P.y + Offset.y) / Zoom * 100;
end;


function ScreenToWorld(APoint: TPoint): TFloatPoint;
begin
  ScreenToWorld.X := (APoint.x + Offset.x) / (Zoom / 100);
  ScreenToWorld.Y := (APoint.y + Offset.y) / (Zoom / 100);
end;

function WorldToScreen(APoint: TFloatPoint): TPoint;
begin
  WorldToScreen.X := round(APoint.X * Zoom / 100) - Offset.x;
  WorldToScreen.y := round(APoint.Y * Zoom / 100) - Offset.y;
end;

procedure RectZoom(AHeight, AWidth: extended; MinPoint, MaxPoint: TFloatPoint);
begin
  if (MaxPoint.X - MinPoint.X <> 0) and (MaxPoint.Y - MinPoint.Y <> 0) then
  begin
    if (Awidth / (abs(MaxPoint.X - MinPoint.X))) > (AHeight / (abs(MaxPoint.Y - MinPoint.Y))) then
      Zoom := 100 * AHeight / (abs(MaxPoint.Y - MinPoint.Y))
    else
      Zoom := 100 * AWidth / (abs(MaxPoint.X - MinPoint.X));
    if MinPoint.X < MaxPoint.X then
      Offset.x := round(MinPoint.X * Zoom / 100)
    else
      Offset.x := round(MaxPoint.X * Zoom / 100);
    if MinPoint.Y < MaxPoint.Y then
      Offset.y := round(MinPoint.Y * Zoom / 100)
    else
      Offset.Y := round(MaxPoint.Y * Zoom / 100);
  end;
end;

procedure MaxMin(APoint: TFloatPoint);
begin
  if (APoint.x > MaxPoint.x) then
    MaxPoint.x := APoint.x;
  if (APoint.y > MaxPoint.y) then
    MaxPoint.y := APoint.y;
  if (APoint.x < MinPoint.x) then
    MinPoint.x := APoint.x;
  if (APoint.y < MinPoint.y) then
    MinPoint.y := APoint.y;
end;

function CasePenStyle(Index: integer): TPenStyle;
begin
  case Index of
    0: Result := psSolid;
    7: Result := psClear;
    2: Result := psDot;
    1: Result := psDash;
    3: Result := psDashDot;
    4: Result := psDashDotDot;
  end;
end;

function CaseBrushStyle(Index: Integer): TBrushStyle;
begin
  case Index of
    0: Result := bsSolid;
    1: Result := bsClear;
    2: Result := bsHorizontal;
    3: Result := bsVertical;
    4: Result := bsFDiagonal;
    5: Result := bsBDiagonal;


  end;
end;

begin
end.

