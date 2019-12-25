unit uCryostasis;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, uAOCUtils, IntComputers, System.Generics.Collections;

type
  TCryostasis = class(TForm)
    pnl1: TPanel;
    pnl2: TPanel;
    btnNorth: TButton;
    btnEast: TButton;
    btnSouth: TButton;
    btnWest: TButton;
    btnInventory: TButton;
    btnTake: TButton;
    btnDrop: TButton;
    edtItem: TEdit;
    btnReset: TButton;
    mmo1: TMemo;
    procedure btnNorthClick(Sender: TObject);
    procedure btnEastClick(Sender: TObject);
    procedure btnSouthClick(Sender: TObject);
    procedure btnWestClick(Sender: TObject);
    procedure btnInventoryClick(Sender: TObject);
    procedure btnTakeClick(Sender: TObject);
    procedure btnDropClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Fprogram: TDictionary<Integer, int64>;
    FComputer: TBasicIntComputer;
    procedure RunInstruction(Const aInstruction: String);
    procedure DoItemAction(const aAction: String);
    procedure ResetComputer;
  public
    { Public declarations }
  end;

var
  Cryostasis: TCryostasis;

implementation

{$R *.dfm}

procedure TCryostasis.btnResetClick(Sender: TObject);
begin
  mmo1.Clear;
  edtItem.Clear;
  ResetComputer;
end;

procedure TCryostasis.btnEastClick(Sender: TObject);
begin
  RunInstruction('east');
end;

procedure TCryostasis.btnInventoryClick(Sender: TObject);
begin
  RunInstruction('inv')
end;

procedure TCryostasis.btnNorthClick(Sender: TObject);
begin
  RunInstruction('north');
end;

procedure TCryostasis.btnSouthClick(Sender: TObject);
begin
  RunInstruction('south');
end;

procedure TCryostasis.btnWestClick(Sender: TObject);
begin
  RunInstruction('west')
end;

procedure TCryostasis.btnTakeClick(Sender: TObject);
begin
  DoItemAction('take');
end;

procedure TCryostasis.btnDropClick(Sender: TObject);
begin
  DoItemAction('drop');
end;

procedure TCryostasis.DoItemAction(const aAction: String);
begin
  RunInstruction(Format('%s %s', [aAction, Trim(edtItem.Text)]));
end;

procedure TCryostasis.FormCreate(Sender: TObject);
var Input: TStringList;
begin
  Input := TStringList.Create;
  Input.LoadFromFile(AOCUtils.Config.BaseFilePath+'\input25.txt');
  Fprogram := TBasicIntComputer.ParseIntput(Input[0]);
  Input.Free;

  ResetComputer;
end;

procedure TCryostasis.FormDestroy(Sender: TObject);
begin
  Fprogram.Free;
  FComputer.Free;
end;

procedure TCryostasis.ResetComputer;
begin
  if Assigned(FComputer) then
    FComputer.Free;

  FComputer := TBasicIntComputer.Create(Fprogram);
  FComputer.StopOnOutPut := True;
  RunInstruction('');
end;

procedure TCryostasis.RunInstruction(Const aInstruction: String);
var Line: string;
    OutPut: Integer;
begin
  if aInstruction <> '' then
    FComputer.QueueASCIICode(aInstruction);

  Line := '';
  while not FComputer.IsStopped do
  begin
    Output := FComputer.Run;

    if Output = 10 then
    begin
      mmo1.Lines.Add(Line);

      if SameText('Command?', Line) then
        Exit;

      Line := '';
    end
    else
      Line := Line + Char(Output);
  end;
end;

end.
