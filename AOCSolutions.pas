unit AOCSolutions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Defaults,
  System.Generics.Collections, system.Diagnostics, AOCBase, RegularExpressions, System.DateUtils, system.StrUtils, system.Math;

type TPosition = record
  x: integer;
  y: Integer;
  procedure SetIt(const aX, aY: integer);
  procedure AddDelta(const aX, aY: Integer);
end;

type TAdventOfCodeDayExample = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

type TAdventOfCodeDay1 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  private
    function NeededFeul(Const Mass: Integer): Integer;
  end;

type TAdventOfCodeDay2 = class(TAdventOfCode)
  protected
    Fprogram: TDictionary<Integer, Integer>;
    function RunProgram(const aNoun, aVerb: Integer): Integer;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  end;

type TAdventOfCodeDay3 = class(TAdventOfCode)
  protected
    PointsL, PointsR: TDictionary<TPosition, Integer>;
    function LoadPoints(Const Input: string): TDictionary<TPosition, Integer>;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  end;

implementation

{$Region 'TPosition'}
procedure TPosition.SetIt(const aX: Integer; const aY: Integer);
begin
  x := aX;
  y := aY;
end;

procedure TPosition.AddDelta(const aX, aY: Integer);
begin
  x := x + aX;
  y := y + aY;
end;
{$ENDREGION}

{$Region 'Example' }
function TAdventOfCodeDayExample.SolveA: Variant;
Var s: string;
begin
  Result := 0;
  for s in FInput do
    Result := Result + StrToInt(s); //406
end;

function TAdventOfCodeDayExample.SolveB: Variant;
var frequency, i: Integer;
    frequencys: TDictionary<Integer, string>;
Begin
  frequency := 0;

  frequencys := TDictionary<Integer, string>.Create;
  while true do
  begin
    for i := 0 to FInput.Count -1 do
    begin

      frequency := frequency + StrToInt(FInput[i]);

      if frequencys.ContainsKey(frequency) then
      begin
        result := frequency; //312
        Exit;
      end;
      frequencys.Add(frequency, '');
    end;
  end;

  frequencys.Free;
end;
{$ENDREGION}

{$Region 'TAdventOfCodeDay1'}
function TAdventOfCodeDay1.NeededFeul(Const Mass: Integer): Integer;
begin
  Result := Trunc(Mass/3)-2;
end;

function TAdventOfCodeDay1.SolveA: Variant;
Var s: string;
begin
  Result := 0;
  for s in FInput do
    Result := Result + NeededFeul(StrToInt(s)); //3270717
end;

function TAdventOfCodeDay1.SolveB: Variant;
var fuel: Integer;
    s: string;
Begin
  Result := 0;
  for s in FInput do
  begin
    Fuel := StrToInt(s);
    while Fuel > 6 do
    begin
      Fuel := NeededFeul(Fuel);
      Result := result + Fuel; //4903193
    end;
  end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay2'}
procedure TAdventOfCodeDay2.BeforeSolve;
var Line: TStringList;
    i: Integer;
begin
  Fprogram:= TDictionary<Integer, Integer>.Create;
  line := TStringList.Create;
  line.Delimiter := ',';
  Line.DelimitedText := FInput[0];

  for i := 0 to Line.Count -1 do
    Fprogram.Add(i, StrToInt(Line[i]));

  Line.Free;
end;

procedure TAdventOfCodeDay2.Aftersolve;
begin
  Fprogram.Free;
end;

function TAdventOfCodeDay2.RunProgram(const aNoun, aVerb: Integer): Integer;
var TempProgram: TDictionary<Integer, Integer>;
    position: Integer;
begin
  TempProgram := TDictionary<Integer, Integer>.Create(FProgram);

  TempProgram[1] := aNoun;
  TempProgram[2] := aVerb;

  position := 0;
  while TempProgram[position] <> 99 do
  begin
    case TempProgram[position] of
      1: TempProgram[TempProgram[position+3]] := TempProgram[TempProgram[position+1]] + TempProgram[TempProgram[position+2]];
      2: TempProgram[TempProgram[position+3]] := TempProgram[TempProgram[position+1]] * TempProgram[TempProgram[position+2]];
    else
      raise Exception.Create('Unknown command: ' + IntToStr(TempProgram[position]));
    end;

    position := position + 4;
  end;

  Result := TempProgram[0];
  TempProgram.Free;
end;

function TAdventOfCodeDay2.SolveA: Variant;
begin
  Result := RunProgram(12, 2); //3085697
end;

function TAdventOfCodeDay2.SolveB: Variant;
var noun, verb: Integer;
begin
  Result := 0;

  for noun := 0 to 100 do
    for verb := 0 to 100 do
    begin
      if RunProgram(noun, verb) = 19690720 then
        Exit(100*noun + verb) //9425
    end;
end;
{$ENDREGION}

//{$Region 'TAdventOfCodeDay3}
procedure TAdventOfCodeDay3.BeforeSolve;
begin
  PointsL := LoadPoints(FInput[0]);
  PointsR := LoadPoints(FInput[1]);
end;

procedure TAdventOfCodeDay3.AfterSolve;
begin
  PointsL.Free;
  PointsR.Free;
end;

function TAdventOfCodeDay3.LoadPoints(Const Input: string): TDictionary<TPosition, Integer>;
var Line: TStringList;
    Position: TPosition;
    Steps, i, j, DeltaX, DeltaY: Integer;
begin
  Result := TDictionary<TPosition, Integer>.Create;

  line := TStringList.Create;
  line.Delimiter := ',';
  Line.DelimitedText := Input;

  Position.SetIt(0,0);

  Steps := 0;
  for i := 0 to Line.Count-1 do
  begin
    DeltaX := 0;
    DeltaY := 0;

    case IndexStr(LeftStr(Line[i], 1), ['R','L','U','D']) of
      0: DeltaX := 1;
      1: DeltaX := -1;
      2: DeltaY := 1;
      3: DeltaY := -1;
    else
      raise Exception.Create('Unknown command ' + Line[i] );
    end;

    for j := 0 to StrToInt(RightStr(Line[i], Length(Line[i])-1))-1 do
    begin
      Inc(Steps);
      Position.AddDelta(DeltaX, DeltaY);
      if Not Result.ContainsKey(Position) then
        Result.Add(Position, Steps);
    end;
  end;

  Line.Free;
end;

function TAdventOfCodeDay3.SolveA: Variant;
var Position, StartPosition: TPosition;

  function _Distance(const a, b: TPosition): Integer;
  begin
    Result := Abs(a.x-b.x) + Abs(a.y-b.y);
  end;

begin
  StartPosition.SetIt(0,0);
  Result := MaxInt;
  for Position in PointsL.Keys do
  begin
    if PointsR.ContainsKey(Position) then
      if _Distance(StartPosition, Position) < Result then
        Result := _Distance(StartPosition, Position); //870
  end;
end;

function TAdventOfCodeDay3.SolveB: Variant;
var Point: TPair<TPosition, Integer>;
begin
  Result := MaxInt;

  for Point in PointsL do
  begin
    if PointsR.ContainsKey(Point.key) then
      if (PointsR[Point.key] + Point.Value) < Result then
        Result := (PointsR[Point.key] + Point.Value);
  end;
end;
//{$ENDREGION}


initialization
  RegisterClasses([TAdventOfCodeDayExample, TAdventOfCodeDay1, TAdventOfCodeDay2, TAdventOfCodeDay3

  ]);

end.
