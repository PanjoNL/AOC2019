unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, AOCBase, AOCSolutions, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    btnSolve: TButton;
    cbb1: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure btnSolveClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  cbb1.ItemIndex := 15;
  btnSolveClick(nil);
  Application.Terminate;
end;

procedure TForm1.btnSolveClick(Sender: TObject);
var AdventOfCode: TAdventOfCode;
begin
  AdventOfCode := nil;

  Case cbb1.ItemIndex of //TODO remove casestatement
    0: AdventOfCode := TAdventOfCodeDay1.Create;
    1: AdventOfCode := TAdventOfCodeDay2.Create;
    2: AdventOfCode := TAdventOfCodeDay3.Create;
    3: AdventOfCode := TAdventOfCodeDay4.Create;
    4: AdventOfCode := TAdventOfCodeDay5.Create;
    5: AdventOfCode := TAdventOfCodeDay6.Create;
    6: AdventOfCode := TAdventOfCodeDay7.Create;
    7: AdventOfCode := TAdventOfCodeDay8.Create;
    8: AdventOfCode := TAdventOfCodeDay9.Create;
    9: AdventOfCode := TAdventOfCodeDay10.Create;
    10:AdventOfCode := TAdventOfCodeDay11.Create;
    11:AdventOfCode := TAdventOfCodeDay12.Create;
    12:AdventOfCode := TAdventOfCodeDay13.Create;
    13:AdventOfCode := TAdventOfCodeDay14.Create;
    14:AdventOfCode := TAdventOfCodeDay15.Create;
    15:AdventOfCode := TAdventOfCodeDay16.Create;
  end;

  if Assigned(AdventOfCode) then
  try
    AdventOfCode.Solve;
  finally
    AdventOfCode.Free;
  end;
end;

end.
