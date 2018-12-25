program html_example;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
    { $IFDEF UseCThreads}
      cthreads,
    { $ENDIF}
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, formMain, steCache;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

