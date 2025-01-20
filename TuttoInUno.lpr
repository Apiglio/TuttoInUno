program TuttoInUno;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tutto_in_uno_test, aufscript_frame, item, region, player, gamerule,
  script_interface
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm_TuttoInUno, Form_TuttoInUno);
  Application.Run;
end.

