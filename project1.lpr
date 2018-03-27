program VectorEditorFreeEdition;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  MainUnit,
  FiguresUnit,
  ToolsUnit,
  ScaleUnit, Historyunit;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TEditor, Editor);
  Application.Run;
end.
