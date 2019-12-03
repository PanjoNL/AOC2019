unit AOCBase;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, system.Diagnostics, ClipBrd, system.UITypes;

type TAdventOfCode = class(TPersistent)
  constructor Create;
  destructor Destroy; override;
  protected
    FInput: TStrings;
    function SolveA: Variant; virtual;
    function SolveB: Variant; virtual;
    procedure BeforeSolve; virtual;
    procedure AfterSolve; virtual;
  private
    function SaveFilePath: String;
    function InputFilePath: string;
    function MakeFilePath(const aFileName: String): string;
    function DayIndex: String;
  public
  { Public declarations }
    procedure Solve;
  end;

implementation

uses
  uAOCUtils;

function TAdventOfCode.DayIndex: String;
begin
  Result := AOCUtils.DayIndexFromClassName(Self.ClassName);
end;

constructor TAdventOfCode.Create;
var FilePath: String;

  procedure _DownLoadInput;
  begin
    AOCUtils.DownLoadPuzzleInput(FInput, DayIndex);
    FInput.SaveToFile(FilePath);
  end;

begin
  Assert(Self.ClassName.StartsWith('TAdventOfCodeDay'), 'Classname should begin with TAdventOfCodeDay, followd by the dayindex');

  FInput := TStringList.Create;
  FilePath := InputFilePath;
  if FileExists(FilePath) then
  begin
    FInput.LoadFromFile(FilePath);
    if (FInput.Count > 0) And (FInput[0].StartsWith('Please don')) then
      _DownLoadInput //File exists, but downloaded to early, let's try again
  end
  else
    _DownLoadInput
end;

destructor TAdventOfCode.Destroy;
begin
  FInput.Free;
  inherited;
end;

function TAdventOfCode.SaveFilePath: String;
begin
  Result := MakeFilePath('Solution');
end;

function TAdventOfCode.InputFilePath: string;
begin
  Result := MakeFilePath('Input')
end;

function TAdventOfCode.MakeFilePath(const aFileName: String): string;
begin
  result := Format('%s\%s%s.txt', [AOCUtils.Config.BaseFilePath, aFileName, DayIndex])
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
  // To be overriden
end;

procedure TAdventOfCode.AfterSolve;
begin
  // To be overriden
end;


procedure TAdventOfCode.Solve;
var StopWach: TStopwatch;
    TimeA, TimeB: Int64;
    SolutionA, SolutionB, TotalTime: string;
begin
  StopWach := StopWach.StartNew;

  BeforeSolve;
  SolutionA := Trim(VarToStr(SolveA));

  TimeA := StopWach.ElapsedMilliseconds;

  StopWach := StopWach.StartNew;

  SolutionB := Trim(VarToStr(SolveB));
  AfterSolve;

  TimeB := StopWach.ElapsedMilliseconds;
  TotalTime := IntToStr(TimeA + TimeB);

  if (MessageDlg('Solution A: ' + SolutionA + ' Solved in ' + IntToStr(TimeA) + ' ms.' +#10#13 +
                 'Copy to clipboard?', mtInformation, [mbYes, mbNo], 0) <> Ord(mbNo)) then
    Clipboard.AsText := SolutionA;

  if (MessageDlg('Solution B: ' + SolutionB + ' Solved in ' + IntToStr(TimeB) + ' ms.' + #10#13 +
                 'Total execution time: ' + TotalTime + ' ms.' + #10#13 +
                 'Copy to clipboard?', mtInformation, [mbYes, mbNo], 0) <> Ord(mbNo)) then
    Clipboard.AsText := SolutionB;
end;

end.
