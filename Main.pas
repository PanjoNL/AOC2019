unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, AOCBase, AOCSolutions, Vcl.ExtCtrls,
  System.Generics.Collections, uAOCUtils;

type
  TForm1 = class(TForm)
    btnSolve: TButton;
    cbb1: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure btnSolveClick(Sender: TObject);
  private
    function GetAdventOfCodeName(const aDayIndex: string): string;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var AdventOfCodeClasses: TList<TAdventOfCodeRef>;
    AdventOfCode: TAdventOfCodeRef;
begin
  AllocConsole;
  AOCUtils.Config.LoadConfig;
  AdventOfCodeClasses := AOCUtils.GetAdventOfCode;

  try
    for AdventOfCode in AdventOfCodeClasses Do
      cbb1.Items.AddObject(GetAdventOfCodeName(AOCUtils.DayIndexFromClassName(AdventOfCode.ClassName)), TObject(AdventOfCode));
  finally
    AdventOfCodeClasses.Free;
  end;
  cbb1.ItemIndex := cbb1.Items.Count - 1;

  btnSolveClick(nil);
  Application.Terminate;
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TForm1.btnSolveClick(Sender: TObject);
begin
  AOCUtils.DoAdventOfCode(TAdventOfCodeRef(Cbb1.Items.Objects[cbb1.ItemIndex]));
end;

function TForm1.GetAdventOfCodeName(const aDayIndex: string): string;
var dummy: Integer;
begin
  if TryStrToInt(aDayIndex, Dummy) then
    Result := 'Day ' + aDayIndex
  else
    Result := 'Day 0 ' + aDayIndex;
end;

end.
