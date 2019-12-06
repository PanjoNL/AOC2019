unit AOCSolutions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Defaults, System.Generics.Collections,
  system.Diagnostics, AOCBase, RegularExpressions, System.DateUtils, system.StrUtils,
  system.Math;

type
  TPosition = record
    x: integer;
    y: Integer;
    procedure SetIt(const aX, aY: integer);
    procedure AddDelta(const aX, aY: Integer);
  end;

type
  TAdventOfCodeDayExample = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

type
  TAdventOfCodeDay1 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  private
    function NeededFeul(const Mass: Integer): Integer;
  end;

type
  TAdventOfCodeDay2 = class(TAdventOfCode)
  protected
    Fprogram: TDictionary<Integer, Integer>;
    function RunProgram(const aNoun, aVerb: Integer): Integer;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  end;

type
  TAdventOfCodeDay3 = class(TAdventOfCode)
  protected
    PointsL, PointsR: TDictionary<TPosition, Integer>;
    function LoadPoints(const Input: string): TDictionary<TPosition, Integer>;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  end;

Type TDoubleCheck = function (Const aStrToCheck: string; Const aIndex: Integer): Boolean of object;

type
  TAdventOfCodeDay4 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    function Solve(aDoubleCheck: TDoubleCheck): Integer;
    function DoubleCheckA(Const aStrToCheck: string; Const aIndex: Integer): Boolean;
    function DoubleCheckB(Const aStrToCheck: string; Const aIndex: Integer): Boolean;
  end;

type
  TAdventOfCodeDay5 = class(TAdventOfCode)
  protected
    Fprogram: TDictionary<Integer, Integer>;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    function RunProgram(const StartOutputParam: Integer): Integer;
  end;

type
  TAdventOfCodeDay6 = class(TAdventOfCode)
  protected
    Map: TDictionary<String,String>;
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
var s: string;
begin
  Result := 0;
  for s in FInput do
    Result := Result + StrToInt(s); //406
end;

function TAdventOfCodeDayExample.SolveB: Variant;
var frequency, i: Integer;
    frequencys: TDictionary<Integer, string>;
begin
  frequency := 0;

  frequencys := TDictionary<Integer, string>.Create;
  while true do
  begin
    for i := 0 to FInput.Count - 1 do
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

function TAdventOfCodeDay1.NeededFeul(const Mass: Integer): Integer;
begin
  Result := Trunc(Mass / 3) - 2;
end;

function TAdventOfCodeDay1.SolveA: Variant;
var s: string;
begin
  Result := 0;
  for s in FInput do
    Result := Result + NeededFeul(StrToInt(s)); //3270717
end;

function TAdventOfCodeDay1.SolveB: Variant;
var fuel: Integer;
    s: string;
begin
  Result := 0;
  for s in FInput do
  begin
    fuel := StrToInt(s);
    while fuel > 6 do
    begin
      fuel := NeededFeul(fuel);
      Result := result + fuel; //4903193
    end;
  end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay2'}

procedure TAdventOfCodeDay2.BeforeSolve;
var Line: TStringList;
    i: Integer;
begin
  Fprogram := TDictionary<Integer, Integer>.Create;
  Line := TStringList.Create;
  Line.Delimiter := ',';
  Line.DelimitedText := FInput[0];

  for i := 0 to Line.Count - 1 do
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
      1: TempProgram[TempProgram[position + 3]] := TempProgram[TempProgram[position + 1]] + TempProgram[TempProgram[position + 2]];
      2: TempProgram[TempProgram[position + 3]] := TempProgram[TempProgram[position + 1]] * TempProgram[TempProgram[position + 2]];
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
        Exit(100 * noun + verb) //9425
    end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay3'}

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

function TAdventOfCodeDay3.LoadPoints(const Input: string): TDictionary<TPosition, Integer>;
var Line: TStringList;
    Position: TPosition;
    Steps, i, j, DeltaX, DeltaY: Integer;
begin
  Result := TDictionary<TPosition, Integer>.Create;

  Line := TStringList.Create;
  Line.Delimiter := ',';
  Line.DelimitedText := Input;

  Position.SetIt(0, 0);

  Steps := 0;
  for i := 0 to Line.Count - 1 do
  begin
    DeltaX := 0;
    DeltaY := 0;

    case IndexStr(LeftStr(Line[i], 1), ['R', 'L', 'U', 'D']) of
      0: DeltaX := 1;
      1: DeltaX := -1;
      2: DeltaY := 1;
      3: DeltaY := -1;
    else
      raise Exception.Create('Unknown command ' + Line[i]);
    end;

    for j := 0 to StrToInt(RightStr(Line[i], Length(Line[i]) - 1)) - 1 do
    begin
      Inc(Steps);
      Position.AddDelta(DeltaX, DeltaY);
      if not Result.ContainsKey(Position) then
        Result.Add(Position, Steps);
    end;
  end;

  Line.Free;
end;

function TAdventOfCodeDay3.SolveA: Variant;
var Position, StartPosition: TPosition;

  function _Distance(const a, b: TPosition): Integer;
  begin
    Result := Abs(a.x - b.x) + Abs(a.y - b.y);
  end;

begin
  StartPosition.SetIt(0, 0);
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
        Result := (PointsR[Point.key] + Point.Value); //13698
  end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay4'}
function TAdventOfCodeDay4.SolveA: Variant;
begin
  Result := Solve(DoubleCheckA); //1665
end;

function TAdventOfCodeDay4.SolveB: Variant;
begin
  Result := Solve(DoubleCheckB); //1131
end;

function TAdventOfCodeDay4.Solve(aDoubleCheck: TDoubleCheck): Integer;
var iNumberToCheck, IndexToCheck, PrevInt, TempInt: Integer;
    sNumberToCheck: string;
    IsIncreasing, ContainsDouble: Boolean;
begin
  Result := 0;
  for iNumberToCheck := StrToInt(FInput[0]) to StrToInt(FInput[1]) do
  begin
    sNumberToCheck := IntToStr(iNumberToCheck);
    IsIncreasing := True;
    ContainsDouble := False;
    PrevInt := StrToInt(sNumberToCheck[1]);
    for IndexToCheck := 1 to Length(sNumberToCheck) -1 do
    begin
      TempInt := StrToInt(sNumberToCheck[IndexToCheck+1]);
      if TempInt < PrevInt then
      begin
        IsIncreasing := False;
        break
      end;
      PrevInt := TempInt;

      ContainsDouble := ContainsDouble or aDoubleCheck(sNumberToCheck, IndexToCheck);
    end;

    if ContainsDouble and IsIncreasing then
      Inc(result);
  end;
end;

function TAdventOfCodeDay4.DoubleCheckA(Const aStrToCheck: string; Const aIndex: Integer): Boolean;
begin
  Result := (aStrToCheck[aIndex] = aStrToCheck[aIndex+1]);
end;

function TAdventOfCodeDay4.DoubleCheckB(Const aStrToCheck: string; Const aIndex: integer): Boolean;
var TempIndex: Integer;
    TempString: string;
begin
  TempIndex := aIndex + 1;
  TempString := 'X' + aStrToCheck + 'X'; //112233 -> X112233X
  Result := ((TempString[TempIndex] = TempString[TempIndex +1])
         and (TempString[TempIndex] <> TempString[TempIndex-1])
         and (TempString[TempIndex] <> TempString[TempIndex+2]));
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay5'}
procedure TAdventOfCodeDay5.BeforeSolve;
var Line: TStringList;
    i: Integer;
begin
  Fprogram := TDictionary<Integer, Integer>.Create;
  Line := TStringList.Create;
  Line.Delimiter := ',';
  Line.DelimitedText := FInput[0];

  for i := 0 to Line.Count - 1 do
    Fprogram.Add(i, StrToInt(Line[i]));

  Line.Free;
end;

procedure TAdventOfCodeDay5.AfterSolve;
begin
  Fprogram.Free;
end;

function TAdventOfCodeDay5.SolveA: Variant;
begin
  Result := RunProgram(1); //2845163
end;

function TAdventOfCodeDay5.SolveB: Variant;
begin
  Result := RunProgram(5); //9436229
end;

function TAdventOfCodeDay5.RunProgram(const StartOutputParam: Integer): Integer;
var TempProgram: TDictionary<Integer, Integer>;
    position, OutPutParam: Integer;
    Command: string;

  function _GetParam(Const aIndex: Integer): Integer;
  begin
    if Command[Length(Command)-1-aIndex] = '1' then
      Result := TempProgram[position + aIndex]
    else
      Result := TempProgram[TempProgram[position + aIndex]]
  end;

begin
  TempProgram := TDictionary<Integer, Integer>.Create(FProgram);

  position := 0;
  OutPutParam := StartOutputParam;
  while TempProgram[position] <> 99 do
  begin
    Command := RightStr('00000'+IntToStr(TempProgram[position]), 5); //104 -> 00104

    case StrToInt(RightStr(Command, 2)) of
      1: begin
          TempProgram[TempProgram[position + 3]] := _GetParam(1) + _GetParam(2);
          position := position + 4;
         end;
      2: begin
          TempProgram[TempProgram[position + 3]] := _GetParam(1) * _GetParam(2);
          position := position + 4;
         end;
      3: begin
          TempProgram[TempProgram[position + 1]] := OutPutParam;
          position := position + 2
         end;
      4: begin
          OutPutParam := _GetParam(1);
          position := position + 2
         end;
      5: begin
          if _GetParam(1) <> 0 then
            position := _GetParam(2)
          else
            position := position + 3
         end;
      6: begin
          if _GetParam(1) = 0 then
            position := _GetParam(2)
          else
            position := position + 3;
         end;
      7: begin
          TempProgram[TempProgram[position + 3]] := Integer(_GetParam(1) < _GetParam(2));
          position := position + 4;
         end;
      8: begin
          TempProgram[TempProgram[position + 3]] := Integer(_GetParam(1) = _GetParam(2));
          position := position + 4;
          end;
    else
      raise Exception.CreateFmt('Unknown command: %s', [Command]);
    end;
  end;

  Result :=  OutPutParam;
  TempProgram.Free;
end;
{$ENDREGION}
//{$Region 'TAdventOfCodeDay6'}
procedure TAdventOfCodeDay6.BeforeSolve;
var line: TStringList;
    i: Integer;
begin
  Map := TDictionary<String,String>.Create;;
  Line := TStringList.Create;
  for i := 0 to FInput.Count -1 do
  begin
    Line.Delimiter := ')';
    Line.DelimitedText := FInput[i];
    Map.Add(Line[1], Line[0]);
  end;
  line.free;
end;

procedure TAdventOfCodeDay6.AfterSolve;
begin
  Map.Free
end;

function TAdventOfCodeDay6.SolveA: Variant;

  function _CountOrbits(const From: String; Level: integer): Integer;
  var Pair: tPair<string, String>;
  begin
    Result := 0;
    for pair in map do
      if Pair.Value = From then
        Result := Result + level + _CountOrbits(Pair.Key, Level+1);
  end;

begin
  Result := _CountOrbits('COM', 1); //119831
end;

function TAdventOfCodeDay6.SolveB: Variant;

  procedure _FindRoute(From, dest: String; route: TList<String>);
    var Pair: tPair<string, String>;
  begin
    Result := 0;

    if route.Contains(dest) then
      exit;

    for pair in map do
    begin
      if Pair.Value = From then
      begin
        route.Add(Pair.Key);
        _FindRoute(Pair.Key, dest, route);
        if route.Contains(dest) then
          exit ;

        route.Remove(Pair.Key);
      end;
    end;
  end;

var route1, Route2: TList<String>;
begin
  route1 := TList<String>.Create;
  _FindRoute('COM', 'YOU', route1);

  route2 := TList<String>.Create;
  _FindRoute('COM', 'SAN', route2);

  while route1[0] = route2[0] do
  begin
    route1.Remove(route1[0]);
    route2.Remove(route2[0]);
  end;

  Result := route1.Count + Route2.Count -2; //322
  route1.Free;
  Route2.Free;
end;
//{$ENDREGION}


initialization
  RegisterClasses([TAdventOfCodeDayExample, TAdventOfCodeDay1, TAdventOfCodeDay2, TAdventOfCodeDay3, TAdventOfCodeDay4,
  TAdventOfCodeDay5, TAdventOfCodeDay6

]);

end.

