unit AOCBase;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, system.Diagnostics, ClipBrd, system.UITypes ;

type TAdventOfCode = class(TPersistent)
  constructor Create;
  destructor Destroy; override;
  protected
    FInput: TStrings;
    function SolveA: Variant; virtual;
    function SolveB: Variant; virtual;
    procedure BeforeSolve; virtual;
    procedure AfterSolve; virtual;
    function Input: String;
  private
  { Private declarations }
  public
  { Public declarations }
    procedure Solve;
  end;

implementation

constructor TAdventOfCode.Create;
begin
  FInput := TStringList.Create;
  FInput.LoadFromFile(Input);
end;

destructor TAdventOfCode.Destroy;
begin
  FInput.Free;
  inherited;
end;

function TAdventOfCode.Input: string;
var s: string;
    i: Integer;
begin //Some Tricks to determine inputfilepath
  i := Length('TAdventOfCodeDay');
  s := Copy(Self.ClassName, i + 1, Length(Self.ClassName) - i);
  result := 'C:\Users\'+GetEnvironmentVariable('USERNAME')+'\Desktop\AOC2018\Input\input'+s+'.txt'
end;

function TAdventOfCode.SolveA: Variant;
begin
  Result := 'Not implemented'
end;

function TAdventOfCode.SolveB: Variant;
begin
  Result := 'Not implemented'
end;

procedure TAdventOfCode.BeforeSolve;
begin
  //
end;

procedure TAdventOfCode.AfterSolve;
begin
  //
end;

procedure TAdventOfCode.Solve;
var StopWach: TStopwatch;
    iTimeA, iTimeB: Int64;
    SolutionA, SolutionB, TotalTime: string;
begin
  StopWach := StopWach.StartNew;

  BeforeSolve;
  SolutionA := Trim(VarToStr(SolveA));

  iTimeA := StopWach.ElapsedMilliseconds;

  StopWach := StopWach.StartNew;

  SolutionB := Trim(VarToStr(SolveB));
  AfterSolve;

  iTimeB := StopWach.ElapsedMilliseconds;
  TotalTime := IntToStr(iTimeA + iTimeB);

  if (MessageDlg('Solution A: ' + SolutionA + ' Solved in ' + IntToStr(iTimeA) + ' ms.' +#10#13 +
                 'Copy to clipboard?', mtInformation, [mbYes, mbNo], 0) <> Ord(mbNo)) then
    Clipboard.AsText := SolutionA;

  if (MessageDlg('Solution B: ' + SolutionB + ' Solved in ' + IntToStr(iTimeB) + ' ms.' + #10#13 +
                 'Total execution time: ' + TotalTime + ' ms.' + #10#13 +
                 'Copy to clipboard?', mtInformation, [mbYes, mbNo], 0) <> Ord(mbNo)) then
    Clipboard.AsText := SolutionB;
end;

end.
