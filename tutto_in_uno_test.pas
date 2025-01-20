unit tutto_in_uno_test;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
    aufscript_frame;

type

    { TForm_TuttoInUno }

    TForm_TuttoInUno = class(TForm)
        Frame_AufScript1: TFrame_AufScript;
        procedure FormCreate(Sender: TObject);
    private

    public

    end;

var
    Form_TuttoInUno: TForm_TuttoInUno;

implementation
uses script_interface;

{$R *.lfm}

{ TForm_TuttoInUno }

procedure TForm_TuttoInUno.FormCreate(Sender: TObject);
begin
    Frame_AufScript1.AufGenerator;
    GenerateAufScriptFunction(Frame_AufScript1.Auf.Script);
end;

end.

