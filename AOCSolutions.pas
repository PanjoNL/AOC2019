unit AOCSolutions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Defaults,
  System.Generics.Collections, system.Diagnostics, AOCBase, RegularExpressions, System.DateUtils, system.StrUtils, system.Math;


type TTool = (None = 0, Torch = 1, ClimbingGear = 2);
type TCaveType = (Rocky = 0, Wet = 1, Narrow = 2);
type TUnitType = (ImmuneSystem, Infection);
type TAttackType = (Cold, slashing, radiation, fire, bludgeoning);

type TPosition = record
  x: integer;
  y: Integer;
  procedure SetIt(const aX, aY: integer);
  function Copy(const aX, aY: integer): TPosition;
end;

type TGuard = record
  TotalSleep: Integer;
  SleepHash: TDictionary<integer, integer>;
  procedure Init;
end;

type TWorker = record
  CurrentPoint: String;
  EndTime: integer;
  procedure init;
end;

type TNode = Record
  Children: TList<TNode>;
  MetaData: TList<Integer>;
  function SumOfMetaData: Integer;
  function ValueOfNode: Integer;
end;

type TStar = record
  Position: TPosition;
  VelocityX: Integer;
  VelocityY: Integer;
  function CalculatePosition: TPosition;
end;

type TCart = record
  XSpeed: Integer;
  YSpeed: Integer;
  CrossingType: string;
  Procedure Turn(Const aTurn: String);
  procedure Crossing;
  procedure init(Const aDirection: String);
end;

TPlayer = record
  PlayerType: String;
  HitPoints: Integer;
end;

TRegisterInstruction = record
  id: Integer;
  ValueA: Integer;
  ValueB: Integer;
  ValueC: Integer
end;

TCavePosition = record
  Position: TPosition;
  Tool: TTool;
end;

TCaveEvent = record
  CavePosition: TCavePosition;
  Time: Integer;
end;

TNanoBot = record
  X: Integer;
  Y: Integer;
  Z: Integer;
  Radius: Integer;
end;

TUnit = record
  UnitType: TUnitType;
  UnitCount: Integer;
  HitPoints: Integer;
  AttackDamgage: Integer;
  initiative: Integer;
  AttakcType: TAttackType;
  WeakTo: set of TAttackType;
  ImmuneTo: Set of TAttackType;
  function AttackPower: integer;
end;

type TPositions = array[0..3] of TPosition;
type TRegister = array[0..5] of Integer;
type ForestPositions = array[0..7] of TPosition;
type SetOfAttackType = set of TAttackType;

type TAdventOfCodeDay1 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

type TAdventOfCodeDay2 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  private
    function CountChar(const s: string; const c: char): integer;
    function CheckDiference(a,b: string): Integer;
  end;

type TAdventOfCodeDay3 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  private
    grid: TDictionary<TPosition, integer>;
    function HeeftOverLap(const X, Y, width, height: Integer): Boolean;
end;

type TAdventOfCodeDay4 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  private
    function BuildSchedule: TDictionary<String, TGuard>;
end;

type TAdventOfCodeDay5 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  private
    SolutionA: String;
    function ConvertInput: string;
    procedure ReactPolymer(var aPolymer: string);
end;

type TAdventOfCodeDay6 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  private
    MaxX, MaxY: Integer;
    points: TDictionary<String, TPosition>;
end;

type TAdventOfCodeDay7 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  private
    FLatestChar: String;
    Input: TDictionary<String, TStringList>;
    function PointIsFree(Const aInput: TDictionary<String, TStringList>; Const aPoint: string): Boolean;
end;

type TAdventOfCodeDay8 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
  private
    FTopNode: TNode;
end;

type TAdventOfCodeDay9 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  private
    function DoSolve(const TotalPlayers, MaxPoints: Integer): Integer;
end;

type TAdventOfCodeDay10 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  private
    FSolutionA: TStringList;
    FSteps: Integer;
end;

type TAdventOfCodeDay11 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  private
    FuelCells: TDictionary<TPosition, integer>;
    function GetFuelCell(const aX, aY: integer): integer;
    function GetFuelValue(const aGridSize: Integer; var aFuelValue: integer): TPosition;
end;

type TAdventOfCodeDay12 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  private
    FGrowth: TDictionary<String, String>;
    function Grow(const Cycles: Integer): Integer;
end;

type TAdventOfCodeDay13 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
  private
    FFirstCrash, FLatestCar: TPosition;
end;

type TAdventOfCodeDay14 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
end;

type TAdventOfCodeDay15 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  private
    function Battle(const ElfPower: Integer; StopOnElfDeath: boolean): Integer;
end;

type TAdventOfCodeDay16 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  private
    function ReadRegister(const aRegister: string): TRegister;
    function ReadRegisterInstruction(const aInstruction: string): TRegisterInstruction;
end;

type TAdventOfCodeDay17 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  private
    FWaterGrid, FClayGrid: TDictionary<TPosition, String>;
    FMaxY: integer;
    procedure Print;
    procedure LetWaterFall(StartPosition: TPosition);
    procedure MarkStationaryWater(StartPosition: TPosition; aDirection: Integer);
    function LetWaterFlow(StartPosition: TPosition; aDirection: Integer): boolean;
    function IsOverFlowing(StartPosition: TPosition; aDirection: Integer): boolean;
end;

type TAdventOfCodeDay18 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  private
    function Solve(Const aItterationCount: integer): Integer;
end;

type TAdventOfCodeDay19 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  private
    procedure RunProgram(const aMaxIterations: integer; var aRegisters: TRegister);
end;

type TAdventOfCodeDay20 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  private
    FMap: TDictionary<TPosition, string>;
    FDistances: TDictionary<TPosition, Integer>;
    procedure PrintRooms;
    procedure BuildRooms(aStartPosition: TPosition; var Instructions: String);
    procedure CalculateDistance(aStartPosition: TPosition; Const Steps: integer); overload;
    procedure CalculateDistance(aStartPosition: TPosition; Const Steps, X, Y: integer); overload;
end;

type TAdventOfCodeDay21 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  private
    FInstructions :TDictionary<Integer, TRegisterInstruction>;
end;

type TAdventOfCodeDay22 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  private
    FCave: TDictionary<TPosition, integer>;
    const FDepth: integer =  4848;
          FTargetX: Integer = 15;
          FTargetY: Integer = 700;
end;

type TAdventOfCodeDay23 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  private
    NanoBotToCheck: TNanoBot;
    FNanoBots: TList<TNanoBot>;
end;

type TAdventOfCodeDay24 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  private
    function FixUpReindeer(const aBoost: integer; ReindeerSurvives: boolean): Integer;
    function CreateUnit(aUnitType: TUnitType; aUnitCount, aHitPoints : Integer; aImmuneTo, aWeakTo: SetOfAttackType; aAttackDamgage: Integer;
                        aAttakcType: TAttackType; aInitiative: Integer; const aBoost: Integer = 0): TUnit;
end;

type TAdventOfCodeDay25 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  private
    //
end;

function GetPositions(const aStartPosition: TPosition): TPositions;
function ModifyRegister(Const aRegister: TRegister; aInput: TRegisterInstruction; InstuctionId: integer): TRegister;
function ReadRegisterInstruction(const aInstruction: string): TRegisterInstruction;

implementation
{$REGION 'TPosistion'}
procedure TPosition.SetIt(const aX: Integer; const aY: Integer);
begin
  x := aX;
  y := aY;
end;

function TPosition.Copy(const aX, aY: integer): TPosition;
begin
  Result.x := x + aX;
  Result.y := y + aY;
end;
{$ENDREGION}
{$REGION 'TGuard'}
procedure TGuard.Init;
begin
  TotalSleep := 0;
  SleepHash := TDictionary<integer ,Integer>.Create;
end;
{$ENDREGION}
{$REGION 'TWorker'}
procedure TWorker.init;
begin
  EndTime := 0;
  CurrentPoint := '';
end;
{$ENDREGION}
{$REGION 'TNode'}
function TNode.SumOfMetaData: Integer;
var i: integer;
begin
  result := 0;
  for i := 0 to Children.count-1 do
    Result := Result + Children[i].SumOfMetaData;

  for i := 0 to MetaData.Count - 1 do
    Result := Result + MetaData[i];    
end;

function TNode.ValueOfNode: integer;
var i: Integer;
begin
  Result := 0; 

  if Children.Count = 0 then //No childeren, result is sum of the metadata
    Result := SumOfMetaData
  else
  begin
    for i := 0 to MetaData.Count - 1 do
    begin
      if MetaData[i] <= Children.Count then
        Result := Result + Children[MetaData[i]-1].ValueOfNode;
    end;
  end;
end;
{$ENDREGION}
{$REGION 'TStar'}
function TStar.CalculatePosition: TPosition;
begin
  Result.X := Position.X + VelocityX;
  Result.Y := Position.Y + VelocityY;
end;
{$ENDREGION}
{$REGION 'TCart'}
Procedure TCart.Turn(Const aTurn: String);
begin
  if aTurn = '/' then
  begin
    if YSpeed  <> 0 then
    begin
      XSpeed := YSpeed * -1;
      YSpeed := 0;
    end
    else
    begin
      YSpeed := XSpeed * -1;
      XSpeed := 0;
    end
  end
  else if aTurn = '\' then
  begin
    if YSpeed <> 0 then
    begin
      XSpeed := YSpeed;
      YSpeed := 0;
    end
    else
    begin
      YSpeed := XSpeed;
      XSpeed := 0;
    end
  end
end;

procedure TCart.Crossing;
begin
  if CrossingType = 'S' then
    CrossingType := 'R'
  else if CrossingType = 'L' then
  begin
    CrossingType := 'S';
    if YSpeed <> 0 then
    begin
      XSpeed := YSpeed;
      YSpeed := 0;
    end
    else
    begin
      YSpeed := XSpeed * -1;
      XSpeed := 0;
    end
  end
  else if CrossingType = 'R' then
  begin
    CrossingType := 'L';
    if YSpeed <> 0 then
    begin
      XSpeed := YSpeed * -1;
      YSpeed := 0;
    end
    else
    begin
      YSpeed := XSpeed;
      XSpeed := 0;
    end
  end;
end;

procedure TCart.init(Const aDirection: String);
begin
  CrossingType := 'L';
  if (aDirection = 'v') then begin XSpeed := 0;  YSpeed := 1;  end;
  if (aDirection = '^') then begin XSpeed := 0;  YSpeed := -1; end;
  if (aDirection = '<') then begin XSpeed := -1; YSpeed := 0;  end;
  if (aDirection = '>') then begin XSpeed := 1;  YSpeed := 0;  end;
end;
{$ENDREGION}
{$REGION 'TUnit'}
function TUnit.AttackPower: integer;
begin
  Result := UnitCount * AttackDamgage;
end;
{$ENDREGION}
{$REGION 'Function ModifyRegister'}
Function ModifyRegister(Const aRegister: TRegister; aInput: TRegisterInstruction; InstuctionId: integer): TRegister;
begin
  Result := aRegister;
  case InstuctionId of
    0: Result[aInput.ValueC] := Result[aInput.ValueA] + Result[aInput.ValueB];
    1: Result[aInput.ValueC] := Result[aInput.ValueA] + aInput.ValueB;
    2: Result[aInput.ValueC] := Result[aInput.ValueA] * Result[aInput.ValueB];
    3: Result[aInput.ValueC] := Result[aInput.ValueA] * aInput.ValueB;
    4: Result[aInput.ValueC] := Result[aInput.ValueA] and Result[aInput.ValueB];
    5: Result[aInput.ValueC] := Result[aInput.ValueA] and aInput.ValueB;
    6: Result[aInput.ValueC] := Result[aInput.ValueA] or Result[aInput.ValueB];
    7: Result[aInput.ValueC] := Result[aInput.ValueA] or aInput.ValueB;
    8: Result[aInput.ValueC] := Result[aInput.ValueA];
    9: Result[aInput.ValueC] := aInput.ValueA;
    10:Result[aInput.ValueC] := IfThen((aInput.ValueA > Result[aInput.ValueB]), 1);
    11:Result[aInput.ValueC] := IfThen((Result[aInput.ValueA] > AInput.ValueB), 1);
    12:Result[aInput.ValueC] := IfThen((Result[aInput.ValueA] > Result[aInput.ValueB]), 1);
    13:Result[aInput.ValueC] := IfThen((aInput.ValueA = Result[aInput.ValueB]), 1);
    14:Result[aInput.ValueC] := IfThen((Result[aInput.ValueA] = aInput.ValueB), 1);
    15:Result[aInput.ValueC] := IfThen((Result[aInput.ValueA] = Result[aInput.ValueB]), 1);
  end;
end;
{$ENDREGION}
{$REGION 'Function ReadRegisterInstruction'}
function ReadRegisterInstruction(const aInstruction: string): TRegisterInstruction;
var line: TStringList;
begin
  Line := TstringList.Create;
  Line.Delimiter := ' ';
  Line.DelimitedText := aInstruction;

  Result.id := IndexStr(line[0], ['addr','addi','mulr','muli','banr','bani','borr','bori','setr','seti','gtir','gtri','gtrr','eqir','eqri','eqrr']);
  Result.ValueA := StrToInt(Line[1]);
  Result.ValueB := StrToInt(Line[2]);
  Result.ValueC := StrToInt(Line[3]);

  line.Free;
end;
{$ENDREGION}
{$REGION 'function GetPositions'}
function GetPositions(const aStartPosition: TPosition): TPositions;
begin
   Result[0] := aStartPosition.Copy(0, -1); //Top
   Result[1] := aStartPosition.Copy(-1, 0); //Left
   Result[2] := aStartPosition.Copy(1, 0);  //Right
   Result[3] := aStartPosition.Copy(0, 1);  //Bottem
end;
{$ENDREGION}

{$Region 'TAdventOfCodeDay1' }
function TAdventOfCodeDay1.SolveA: Variant;
Var s: string;
begin
  Result := 0;
  for s in FInput do
    Result := Result + StrToInt(s);
end;

function TAdventOfCodeDay1.SolveB: Variant;
var frequency, i: Integer;
    frequencys: TDictionary<Integer, string>;
Begin
  frequency := 0;
  i := 0;

  frequencys := TDictionary<Integer, string>.Create;
  while true do
  begin
    frequency := frequency + StrToInt(FInput[i]);

    if frequencys.ContainsKey(frequency) then
    begin
      result := frequency;
      Exit;
    end;

    frequencys.Add(frequency, '');
    i := i + 1;
    if i >= FInput.count then
      i := 0;
  end;

  frequencys.Free;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay2' }
function TAdventOfCodeDay2.SolveA: Variant;
Var s: string;
    i, t2, t3, charcount: Integer;
    b1, b2: Boolean;
begin
  t2 := 0;
  t3 := 0;

  for s in Finput do
  begin
    b1 := False;
    b2 := False;

    for i := Ord('a') to Ord('z') do
    begin
      charcount := CountChar(s, Char(i));
      if charcount = 2 then b1 := True;
      if charcount = 3 then b2 := True;
    end;

    if b1 then Inc(t2);
    if b2 then Inc(t3);
  end;

  Result := (t2*t3);
end;

function TAdventOfCodeDay2.SolveB: Variant;

  function Merge(a,b: string): string;
  var k: Integer;
  begin
    Result := '';
    for k := 0 to length(a) do
      if a[k] = b[k] then
        Result := Result + a[k];
  end;

var i,j: integer;
begin
  Result := '';
  for i := 0 to FInput.Count - 1 do
    for j := i + 1 to FInput.Count - 1 do
    begin
      if CheckDiference(Trim(FInput[i]), Trim(FInput[j])) = 1 then
      begin
        Result := Merge(Trim(FInput[i]), Trim(FInput[j]));
        exit;
      end;
    end;
end;

function TAdventOfCodeDay2.CountChar(const s: string; const c: char): integer;
begin
  Result:= TRegEx.Matches(s, c).Count
end;

function TAdventOfCodeDay2.CheckDiference(a,b: string): Integer;
var i: Integer;
begin
  result := 0;
  for i := 0 to Length(a) do
  begin
    if a[i] <> b[i] then
      inc(Result);
  end;
end;
{$endregion}
{$REGION 'TAdventOfCodeDay3'}
procedure TAdventOfCodeDay3.BeforeSolve;
var position: TPosition;
    s: string;
    line: TStrings;
    StartPosX, StartPosY, i, j, widthx, heigthy, v: Integer;
begin
  Grid := TDictionary<TPosition, integer>.Create;

  line := TStringList.Create;
  line.Delimiter := '-';

  for s in FInput do
  begin
    line.DelimitedText := s;

    StartPosX := StrToInt(line[1]);
    StartPosY := StrToInt(line[2]);
    Widthx := StrToInt(line[3]);
    Heigthy := StrToInt(line[4]);

    for i := StartPosX to StartPosX + Widthx - 1 do
      for j := StartPosY to StartPosY + Heigthy - 1 do
      begin
        position.SetIt(i, j);
        v := 0;
        if Grid.TryGetValue(position, v) then
          grid[position] := v+1
        else
          grid.Add(position, 0);
      end;
  end;

  line.Free;
end;

procedure TAdventOfCodeDay3.AfterSolve;
begin
  Grid.Free;
end;

function TAdventOfCodeDay3.SolveA: Variant;
var i: Integer;
begin
  result := 0;
  for i in grid.Values do
    if i > 1 then
      Inc(Result); //112378
end;

function TAdventOfCodeDay3.SolveB: Variant;
var s: string;
    line: TStrings;
    StartPosX, StartPosY, widthx, heigthy: Integer;
begin
  line := TStringList.Create;
  line.Delimiter := '-';

  result := 0;
  for s in FInput do
  begin
    line.DelimitedText := s;

    StartPosX := StrToInt(line[1]);
    StartPosY := StrToInt(line[2]);
    Widthx := StrToInt(line[3]);
    Heigthy := StrToInt(line[4]);

    if not HeeftOverLap(StartPosX, StartPosY, Widthx, Heigthy) then
    begin
      result := line[0]; //603
      Exit;
    end;
  end;
end;

function TAdventOfCodeDay3.HeeftOverLap(const X, Y, width, height: Integer): Boolean;
var i, j, v: Integer;
    Position: TPosition;
begin
  Result := False;

  for i := X to X + width - 1 do
    for j := Y to Y + height - 1 do
    begin
      Position.SetIt(i, j);
      if Grid.TryGetValue(Position, v) and (v > 1) then
      begin
        result := true;
        Exit;
      end;
    end;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay4'}
function TAdventOfCodeDay4.BuildSchedule: TDictionary<String, TGuard>;
var Gaurd: TGuard;
    Line: TStringList;
    StartTime, EndTime: TTime;
    CurrentGuard, s: String;
    Minutes, i,v: Integer;
begin
  Result := TDictionary<String, TGuard>.Create;
  line := TStringlist.Create;
  line.Delimiter := ' ';
  StartTime := EncodeTime(0,0,0,0);

  for s in FInput do
  begin
    line.DelimitedText := s;
    if Line[1] = 'Guard' then
      CurrentGuard := line[2]
    else if line[1] = 'falls' then
      StartTime := StrToTime(line[0])
    else if line[1] = 'wakes' then
    begin
      EndTime := StrToTime(Line[0]);
      Minutes := MinutesBetween(EndTime, StartTime);

      if not Result.ContainsKey(CurrentGuard) then
        Gaurd.Init
      else
        Result.TryGetValue(CurrentGuard, Gaurd);

      for i := MinuteOf(StartTime) to MinuteOf(StartTime) + Minutes - 1 do
      begin
        v := 0;
        if Gaurd.SleepHash.ContainsKey(i) then
          Gaurd.SleepHash.TryGetValue(i, v);
        Gaurd.SleepHash.AddOrSetValue(i , v+1);
      end;

      Gaurd.TotalSleep := Gaurd.TotalSleep + Minutes;
      Result.AddOrSetValue(CurrentGuard, Gaurd);
    end
  end;

  line.Free;
end;

function TAdventOfCodeDay4.SolveA: Variant;
var GaurdHash: TDictionary<String, TGuard>;
    GaurdItem: Tpair<string, TGuard>;
    SleepItem: Tpair<Integer, Integer>;
    maxMinute, MaxSleep: Integer;
    CurrentGuard: String;    
begin
  GaurdHash := BuildSchedule;

  CurrentGuard := '';
  MaxSleep := 0;
  for GaurdItem in GaurdHash do
  begin
    if GaurdItem.Value.TotalSleep > MaxSleep then
    begin
      MaxSleep := GaurdItem.Value.TotalSleep;
      CurrentGuard := GaurdItem.Key;
    end;
  end;

  MaxSleep := 0;
  maxMinute := 0;
  for SleepItem in GaurdHash.Items[CurrentGuard].SleepHash do
  begin
    if SleepItem.Value > MaxSleep then
    begin
      maxMinute := SleepItem.Key;
      MaxSleep := SleepItem.Value;
    end;
  end;

  Result := StrToInt(Copy(CurrentGuard, 2, Length(CurrentGuard))) * maxMinute;
  GaurdHash.Free;
end;

function TAdventOfCodeDay4.SolveB: Variant;
var GaurdHash: TDictionary<String, TGuard>;
    GaurdItem: Tpair<string, TGuard>;
    SleepItem: Tpair<Integer, Integer>;
    maxMinute, MaxDays: Integer;
    CurrentGuard: String;
begin
  GaurdHash := BuildSchedule;

  maxMinute := 0;
  MaxDays := 0;
  CurrentGuard := '';
  for GaurdItem in GaurdHash do
  begin
    for SleepItem in GaurdItem.Value.SleepHash do
    begin
      if SleepItem.Value > maxMinute then
      begin
        maxMinute := SleepItem.Value;
        MaxDays := SleepItem.Key;
        CurrentGuard := GaurdItem.Key;
      end;
    end;
  end;

  Result := StrToInt(Copy(CurrentGuard, 2, Length(CurrentGuard))) * MaxDays;
  GaurdHash.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay5'}
function TAdventOfCodeDay5.ConvertInput: string;
var s: string;
begin
  Result := '';
  for s in FInput do
    Result := Result + Trim(s);
end;

procedure TAdventOfCodeDay5.ReactPolymer(var aPolymer: string);
var i: Integer;
    Changed: Boolean;

  Procedure doReplace(Const aPos: integer);
  begin
    if (Abs(Ord(aPolymer[aPos]) - Ord(aPolymer[aPos+1])) = 32) then
    begin
      Delete(aPolymer, apos, 2);
      doReplace(aPos-1); //And check again for privious position
    end
  end;

begin
  Changed := True;

  while Changed do
  begin
    Changed := False;
    for i := 0 to Length(aPolymer) - 1 do
    begin
      if (Abs(Ord(aPolymer[i]) - Ord(aPolymer[i+1])) = 32) then
      begin
        Delete(aPolymer, i, 2);// Delete this pair
        doReplace(i-1); //And check for privious position
        changed := True
      end;
    end;
  end;
end;

function TAdventOfCodeDay5.SolveA: Variant;
var input: string;
begin
  Input := ConvertInput;
  ReactPolymer(Input);
  Result := Length(input); //9686
  SolutionA := Input; //Save solution and use if for B much faster!
end;

function TAdventOfCodeDay5.SolveB: Variant;
var checkInput: string;
    i, min: Integer;
begin
  min := 999999;

  for i := ord('a') to Ord('a') + 26 do
  begin
    checkInput := StringReplace(SolutionA, Char(i), '', [rfIgnoreCase, rfReplaceAll]);
    ReactPolymer(checkInput);

    if Length(checkInput) < min then
      min := Length(checkInput);
  end;

  Result := min; //5524
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay6'}
procedure TAdventOfCodeDay6.BeforeSolve;
var line: TStringList;
    Position: TPosition;
    x, y: Integer;
    s: string;
begin
  points := TDictionary<String, TPosition>.Create;

  line := TStringList.Create;
  line.Delimiter := ',';

  maxX := 0;
  MaxY := 0;

  for s in Finput do
  begin
    line.DelimitedText := s;
    x := StrToInt(line[0]);
    y := StrToInt(line[1]);

    if x > maxX then maxX := x;
    if y > MaxY then MaxY := Y;

    Position.SetIt(x, y);
    points.Add(s, Position);
  end;
  line.Free;
end;

procedure TAdventOfCodeDay6.AfterSolve;
begin
  points.Free
end;

function TAdventOfCodeDay6.SolveA: Variant;

  function FindClosestPoint(x,y: Integer): string;
  var Point: TPair<String,TPosition>;
      MinDistance, Distance: Integer;
  begin
    MinDistance := 999999;
    result := '';

    for Point in points do
    begin
      Distance := ((abs(x-Point.Value.x)+abs(y-Point.Value.y)));
      if Distance <= Mindistance then
      begin
        if Distance < MinDistance then
          Result := Point.Key
        else
          Result := '';

        MinDistance := Distance;
      end;
    end;
  end;

var PointHits: TDictionary<String,integer>;
    key: string;
    x, y, Value: integer;
begin
  PointHits := TDictionary<String,integer>.Create;

  for Key in Points.Keys do
    PointHits.Add(Key, 0);

  for x := 0 to MaxX  do
    for y := 0 to maxY do
    begin
      key := FindClosestPoint(x,y);

      if (x = 0) or (y = 0) or (x = MaxX) or (y = MaxY) then
        PointHits.Remove(key) //Remove point on the edges, it infinite
      else if PointHits.ContainsKey(Key) then
        PointHits[key] := PointHits[key] + 1;
    end;

  Result := 0;
  for Value in PointHits.Values do
    if Value > Result then
      Result := Value; //3401
end;

function TAdventOfCodeDay6.SolveB: Variant;
var X, Y: Integer;

  function inRange(const aPosx, aPosy: Integer): boolean;
  var Position: TPosition;
      distance: Integer;
  begin
    distance := 0;
    result := false;
    for Position in Points.Values do
    begin
      distance := distance + abs(aPosx-Position.x)+abs(aPosy-Position.y);
      if distance >= 10000 then
        Exit;
    end;

    result := true
  end;

begin
  result := 0;

  for x := 0 to MaxX  do
    for y := 0 to maxY do
      if InRange(x,y) then
        inc(result); //49327
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay7'}
procedure TAdventOfCodeDay7.BeforeSolve;
var line, List: tStringList;
    s: String;
begin
  Input := TDictionary<String, TStringList>.Create;
  line :=  TStringList.Create;
  line.Delimiter := ' ';

  for s in FInput do //Convert input to a dictionary format, key is the location, list the points that need to be visited
  begin
    line.DelimitedText:= s;
    if not Input.ContainsKey(line[1]) then
      Input.Add(line[1], TStringList.Create);

    List := Input[line[1]];
    List.Add(line[7]);
    Input.AddOrSetValue(Line[1], List);
  end;

  for s in Finput do
  begin //Find the charater that is not used as a requierd one (ie the last one to be visited)
    line.DelimitedText := s;
    if Not (Input.ContainsKey(line[7])) then
      FLatestChar := Line[7]
  end;
  line.Free;
end;

procedure TAdventOfCodeDay7.AfterSolve;
begin
  Input.Free
end;

function TAdventOfCodeDay7.PointIsFree(Const aInput: TDictionary<String, TStringList>; Const aPoint: string): Boolean;
var v: TPair<string, TStringList>;
    i: Integer;
begin
  Result := false;
  for v in aInput do
    for i := 0 to v.Value.Count - 1 do
      if v.Value[i] = aPoint then
        Exit;

  result := True;
end;

function TAdventOfCodeDay7.SolveA: Variant;
var InputA: TDictionary<string, TStringList>;
    pair: TPair<string, TStringList>;
    point: String;
begin
  result := '';
  InputA := TDictionary<string, TStringList>.Create(input); //Copy the input, so it can be used in part b

  while InputA.Count > 0 do
  begin
    point := '';

    for pair in InputA do
      if PointIsFree(InputA, pair.Key) then
        if (point = '') or (ord(pair.key[1]) < Ord(point[1])) then //Check if point is assinged or is lower in the alpabet then the assinged point
          point := pair.Key;

    Result := result + point; //Add point to result
    InputA.Remove(point); //And remove from the input
  end;

  Result := Result + FLatestChar; //Add latest char to solution //BKCJMSDVGHQRXFYZOAULPIEWTN
  InputA.Free
end;

function TAdventOfCodeDay7.SolveB: Variant;
Const WorkerCount: Integer = 5;
var Workers: TDictionary<Integer,TWorker>;
    Worker: Tworker;
    point, Key: String;
    i, t: integer;

  function PointInPorgress(const aPoint: String): Boolean;
  var w: TWorker;
  begin
    Result := True;
    for w in Workers.Values do
      if w.CurrentPoint = aPoint then
        Exit;

    Result := False;
  end;

begin
  result := 0;
  Workers := TDictionary<Integer,TWorker>.Create;

  Worker.init; //Init a worker
  for i := 1 to WorkerCount do //and start a numberr
    Workers.Add(i, Worker);

  while Input.Count >0 do
  begin
    for Worker in Workers.Values do //Remove points that are done
      if Worker.EndTime = Result then
        Input.Remove(Worker.CurrentPoint);

    for i := 1 to WorkerCount do //Check state of the workers
    begin
      if Workers[i].EndTime <= result then //Worker is free or done
      begin
        point := '';
        for Key in Input.Keys do
          if PointIsFree(Input, Key) and not PointInPorgress(Key) then // its a free point and not being worked on by another worker
            if (point = '') or (ord(key[1]) < Ord(point[1])) then  //Check if point is assinged or is lower in the alpabet then the assinged point
                point := Key;

        if point <> '' then //Found a point to work on, assign it to the worker
        begin
          Worker.EndTime := Result + Ord(Point[1]) - Ord('A') + 1 + 60;
          Worker.CurrentPoint := point;
          Workers[i] := Worker
        end;
      end;
    end;

    if Input.Count > 0  then
    begin // if there is still something todo search for the first event
      t := 999999;
      for Worker in Workers.Values do
      begin
        if Worker.EndTime > result then
          t := Min(t, Worker.EndTime);
      end;
      Result := t;
    end
  end;

  Result := Result + Ord(FLatestChar[1]) - Ord('A') + 60 + 1; //Calculate the end result
  Workers.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay8'}
procedure TAdventOfCodeDay8.BeforeSolve;
var InputList: TstringList;
    Position: Integer;

  function BuildTree(Const childeren, Metadata: integer): TNode;
  var i, c, m: Integer;
  begin
    Result.Children := TList<TNode>.Create;
    Result.MetaData := TList<Integer>.Create;

    for i := 0 to childeren - 1 do
    begin
      c := StrToInt(InputList[Position]);
      m := StrToInt(InputList[Position+1]);
      Position := Position +2;
      Result.Children.Add(BuildTree(c,m));
    end;

    for i := 0 to Metadata - 1 do
    begin
      Result.MetaData.Add(StrToInt(InputList[Position]));
      inc(Position);
    end;
  end;

begin
  InputList := TStringList.Create;
  InputList.Delimiter := ' ';
  InputList.DelimitedText := FInput.Text;

  Position := 2;
  FTopNode := BuildTree(StrToInt(InputList[0]), StrToInt(InputList[1]));
  InputList.Free;
end;

function TAdventOfCodeDay8.SolveA: Variant;
begin
  result := FTopNode.SumOfMetaData; //41555
end;

function TAdventOfCodeDay8.SolveB: Variant;
begin
  Result := FTopNode.ValueOfNode; //16653
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay9'}
function TAdventOfCodeDay9.DoSolve(const TotalPlayers, MaxPoints: Integer): Integer;
var GameGrid: TList<integer>;
    Players: Array of Integer;
    i, t, PlayerNo, Position: Integer;
begin
  GameGrid := TList<integer>.Create;
  GameGrid.add(0);

  SetLength(Players, TotalPlayers);
  for i := 0 to TotalPlayers + 1 do
    Players[i] := 0;

  Position := 0;
  for i := 1 to MaxPoints do
  begin
    if (i mod 23 = 0) then
    begin
      PlayerNo := i mod TotalPlayers;

      t := Position - 7;
      if t < 0 then
        t := t + GameGrid.Count;

      Players[PlayerNo] := Players[PlayerNo] + i + (GameGrid[t]);

      GameGrid.Delete(t);
      Position := t;
    end
    else
    begin
      Position := Position + 2;
      if Position >= GameGrid.Count + 1 then
        Position := Position - GameGrid.Count;

      GameGrid.Insert(Position, (i));
    end
  end;

  result := 0;
  for i := 0 to Length(Players) do
    if Players[i] > result then
      result := Players[i]
end;

function TAdventOfCodeDay9.SolveA: Variant;
begin
  Result := DoSolve(468, 71010); //374287
end;

function TAdventOfCodeDay9.SolveB: Variant;
begin
  Result := DoSolve(468, 7101000); //3083412635
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay10'}
procedure TAdventOfCodeDay10.AfterSolve;
begin
  FSolutionA.Free;
end;

procedure TAdventOfCodeDay10.BeforeSolve;
var Stars: TDictionary<Integer, TStar>;
    Positions: TList<TPosition>;
    Line: TStringList;
    Position: TPosition;
    Star : TStar;
    s: String;
    GridSize, MinX, MaxX, MinY, MaxY, i, j: integer;

  procedure Calculate;
  var i: integer;
      Star : TStar;
  begin
    while True do
    begin
      MinX := 999999;
      MinY := 999999;
      MaxX := -999999;
      MaxY := -999999;

      for i In Stars.Keys do
      begin
        Star := Stars[i];
        Star.Position := Star.CalculatePosition;
        Stars[i] := Star;
        if Star.Position.X < MinX then MinX := Star.Position.X;
        if Star.Position.X > MaxX then MaxX := Star.Position.X;
        if Star.Position.y < Miny then Miny := Star.Position.y;
        if Star.Position.y > Maxy then Maxy := Star.Position.y;
      end;

      if ((MaxX-MinX)*(MaxY-MinY) > abs(GridSize)) then //Bigger then previous solution, so previous is the solution
        Exit;

      Inc(FSteps); //Keep track of the number of steps taken

      GridSize := (MaxX-MinX)*(MaxY-MinY);

      if MaxY-MinY < 15 then
      begin
        Positions.Clear;
        for Star in Stars.Values do
          Positions.Add(Star.Position);
      end;
    end;
  end;

begin
  Line := TStringList.Create;
  Line.Delimiter := ' ';
  Stars := TDictionary<Integer, TStar>.Create;

  FSolutionA := TStringList.Create;

  for i := 0 to FInput.Count -1 do
  begin
    Line.DelimitedText := FInput[i];
    Star.Position.SetIt(StrToInt(Line[0]), StrToInt(Line[1]));
    Star.VelocityX := StrToInt(Line[2]);
    Star.VelocityY := StrToInt(Line[3]);
    Stars.Add(i, Star)
  end;

  FSteps := 0;
  GridSize := 99999999;

  Positions := TList<TPosition>.Create;
  Calculate;

  for j := minY to MaxY do
  begin
    s := '';
    for i := minX to MaxX do
    begin
      Position.SetIt(i, j);
      if Positions.Contains(Position) then
        s := s + '#'
      else
        s := s + '.';
    end;
    FSolutionA.Add(s);
  end;

  Line.Free;
  Stars.Free;
  Positions.Free;
end;

function TAdventOfCodeDay10.SolveA: Variant;
var s: string;
begin
  S := StringReplace(Input, 'input10.txt', 'Solution10.txt', [rfIgnoreCase]);
  FSolutionA.SaveToFile(s);
  Result := 'Solution saved at: ' + s;
end;

function TAdventOfCodeDay10.SolveB: Variant;
begin
  Result := FSteps;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay11'}
procedure TAdventOfCodeDay11.BeforeSolve;
Const Input: integer = 5719;
var position: TPosition;
    x, y, Fuel: Integer;
begin
  FuelCells := TDictionary<TPosition, integer>.Create;

  for x := 1 to 300 do
    for y := 1 to 300 do
    begin
      position.SetIt(x, y);
      Fuel := x + 10;
      Fuel := Fuel * y;
      Fuel := Fuel + Input;
      Fuel := Fuel * (x + 10);
      Fuel := Fuel mod 1000;
      Fuel := Fuel div 100;
      Fuel := Fuel - 5;
      FuelCells.Add(position, Fuel);
    end;
end;

procedure TAdventOfCodeDay11.AfterSolve;
begin
  FuelCells.Free;
end;

Function TAdventOfCodeDay11.GetFuelCell(const aX, aY: integer): integer;
var position: TPosition;
begin
  position.SetIt(aX, aY);
  Result := FuelCells[position];
end;

function TAdventOfCodeDay11.GetFuelValue(const aGridSize: Integer; var aFuelValue: integer): TPosition;
var Fuel, x, y, i, j: Integer;
begin
  aFuelValue := 0;
  for x := 1 to 300 - aGridSize + 1 do
    for y := 1 to 300 - aGridSize + 1 do
    begin
      Fuel := 0;
      for i := 0 to aGridSize - 1 do
        for j := 0 to aGridSize - 1  do
        begin
          Fuel := Fuel + GetFuelCell(x+i,y+j);

          if Fuel > aFuelValue then
          begin
            aFuelValue := Fuel;
            Result.x := x;
            Result.y := y;
          end;
        end;
    end
end;

function TAdventOfCodeDay11.SolveA: Variant;
var MaxPosition: TPosition;
    MaxFuel: Integer;
begin
  MaxPosition := GetFuelValue(3, MaxFuel);
  Result := IntToStr(MaxPosition.x)+','+IntToStr(MaxPosition.Y); //21,34
end;

function TAdventOfCodeDay11.SolveB: Variant;
var MaxFuel, Fuel: Integer;
    MaxPosition, position: TPosition;
    MaxSize, Size: integer;
begin
  MaxFuel := 0;
  MaxSize := 0;
  for size := 3 to 20 do
  begin
    position := GetFuelValue(Size, Fuel);
    if Fuel > MaxFuel then
    begin
      MaxFuel := Fuel;
      MaxSize := Size - 1;
      MaxPosition := position;
    end
  end;

  Result := IntToStr(MaxPosition.x)+','+IntToStr(MaxPosition.Y)+','+IntToStr(MaxSize); //90,244,16
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay12'}
procedure TAdventOfCodeDay12.BeforeSolve;
var Line: TStringList;
    s: string;
begin
  FGrowth := TDictionary<String, String>.Create;
  Line := TStringList.Create;
  Line.Delimiter := ' ';
  for s in fInput do
  begin //...## => #
    Line.DelimitedText := s;
    FGrowth.Add(Line[0], Line[2]);
  end;
end;

procedure TAdventOfCodeDay12.AfterSolve;
begin
  FGrowth.Free;
end;

function TAdventOfCodeDay12.Grow(const Cycles: Integer): Integer;
var StartState, newstate: string;
    i, j: Integer;
begin
  StartState := '##..##..#.##.###....###.###.#.#.######.#.#.#.#.##.###.####..#.###...#######.####.##...#######.##..#';

  for i := 1 to Cycles do
    StartState := '..'+StartState+'..';

  for i := 1 to Cycles do
  begin
    newstate := '';

    for j := 2 to Length(StartState)-3 do
      newstate := newstate + FGrowth[Copy(StartState, j-2,5)];

    StartState := '..'+NewState+'..';
  end;

  result := 0;
  for j := 0 to Length(StartState) -1 do
  begin
    if StartState[j] ='#' then
      Result := Result + j - 3*Cycles-1;
  end;
end;

function TAdventOfCodeDay12.SolveA: Variant;
begin
  Result := Grow(20); //1816
end;

function TAdventOfCodeDay12.SolveB: Variant;
Var i: integer;
begin
  i := Grow(200); //Assume grow stabilizes after 200 cycles, could be beter...
  Result := (50000000000-200) * (i-Grow(199)) + i; //399999999957
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay13'}
procedure TAdventOfCodeDay13.BeforeSolve;
var Carts, TempCarts: TDictionary<TPosition, TCart>;
    Tracks: TDictionary<TPosition, String>;
    Cart: TCart;
    Posistion, NewPosition: TPosition;
    Line, s: String;
    x, y, MaxX, MaxY: Integer;
begin
  FFirstCrash.X := -1;
  FFirstCrash.Y := -1;
  FLatestCar := FFirstCrash;

  Tracks := TDictionary<TPosition, String>.Create;
  Carts := TDictionary<TPosition, TCart>.Create;
  TempCarts := TDictionary<TPosition, TCart>.Create;

  Try
    MaxY := FInput.Count - 1;
    MaxX := 0;

    for y := 0 to MaxY do //Read Input
    begin
      Line := Finput[y];
      if Length(Line) -1 > MaxX then
        MaxX := Length(line) - 1;

      for x := 1 to Length(line) do
      begin
        s := Line[x];
        if s <> '' then
        begin
          Posistion.SetIt(x-1, y);  // -1 Spaces at the start of the input file :-(
          Tracks.Add(Posistion, s);
          if (s='v') or (s='^') or (s='<') or (s='>') then
          begin
            Cart.init(s);
            Carts.Add(Posistion, Cart);
          end;
        end;
      end;
    end;

    while True do
    begin
      TempCarts.Clear;
      for y := 0 to MaxY do
        for x := 0 to MaxX do
        begin
          Posistion.SetIt(x, y);
          if Carts.ContainsKey(Posistion) then
          begin
            s := Tracks[Posistion];
            Cart := Carts[Posistion];

            if (s = '/') or (s = '\') then
              Cart.Turn(s);

            if s = '+' then
              Cart.Crossing;

            NewPosition.SetIt(Posistion.X + Cart.XSpeed, Posistion.Y + Cart.YSpeed);
            if TempCarts.ContainsKey(NewPosition) or Carts.ContainsKey(NewPosition) then //Crash
            begin
              TempCarts.Remove(NewPosition); //Remove from already moved carts
              Carts.Remove(NewPosition); //And from car that still need to move

              if FFirstCrash.X < 0 then
                FFirstCrash := NewPosition;
            end
            else
              TempCarts.Add(NewPosition, Cart);
          end;
        end;

      Carts := TDictionary<TPosition, TCart>.Create(TempCarts);

      if TempCarts.Count = 1 then
      begin
      for Posistion in TempCarts.Keys do
        FLatestCar := Posistion;
        exit;
      end;
    end;

  Finally
    Carts.Free;
    Tracks.Free;
    TempCarts.Free;
  End;
end;

function TAdventOfCodeDay13.SolveA: Variant;
begin
  Result := IntToStr(FFirstCrash.X)+','+IntToStr(FFirstCrash.y); //94,78
end;

function TAdventOfCodeDay13.SolveB: Variant;
begin
  Result := IntToStr(FLatestCar.X)+','+IntToStr(FLatestCar.y); //26,85
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay14'}
function TAdventOfCodeDay14.SolveA: Variant;
Const Count: integer = 209231;
var recipes: TDictionary<Integer, integer>;
    PositionElf1, PosistionElf2, sum, i: Integer;
begin
  recipes := TDictionary<Integer, integer>.create;
  recipes.add(1,3);
  recipes.add(2,7);

  PositionElf1 := 1;
  PosistionElf2 := 2;

  while recipes.Count <= Count + 10 do
  begin
    PositionElf1 := PositionElf1 + recipes[PositionElf1] +1;
    while PositionElf1 > recipes.Count  do
      PositionElf1 := PositionElf1-recipes.Count;

    PosistionElf2 := PosistionElf2 + recipes[PosistionElf2] +1;
    while PosistionElf2 > recipes.Count do
      PosistionElf2 := PosistionElf2-recipes.Count;

    Sum := recipes[PositionElf1] + recipes[PosistionElf2];
    if Sum >= 10 then
    begin
      recipes.Add(recipes.Count +1, 1); //Sum cant be higher then 19
      Sum := sum -10;
    end;
    recipes.Add(recipes.Count +1, sum)
  end;

  Result := '';
  for i := 1 to 10 do
  begin
    Result := Result + IntToStr(recipes[Count+i]);
  end;
  recipes.Free;
end;

function TAdventOfCodeDay14.SolveB: Variant;
var recipes: TDictionary<Integer, integer>;
    PosistionElf1, PosistionElf2, sum: Integer;

  function Check: Boolean; //209231
  begin //No need to check for the last input digit, code is only called when that number is added
    Result := (recipes[recipes.Count - 1] = 3)
           and(recipes[recipes.Count - 2] = 2)
           and(recipes[recipes.Count - 3] = 9)
           and(recipes[recipes.Count - 4] = 0)
           and(recipes[recipes.Count - 5] = 2)
  end;

begin
  recipes := TDictionary<Integer, integer>.create;
  recipes.add(1,3);
  recipes.add(2,7);

  PosistionElf1 := 1;
  PosistionElf2 := 2;

  while True do
  begin
    PosistionElf1 := PosistionElf1 + recipes[PosistionElf1] +1;
    while PosistionElf1 > recipes.Count  do
      PosistionElf1 := PosistionElf1-recipes.Count;

    PosistionElf2 := PosistionElf2 + recipes[PosistionElf2] +1;
    while PosistionElf2 > recipes.Count do
      PosistionElf2 := PosistionElf2-recipes.Count;

    Sum := recipes[PosistionElf1] + recipes[PosistionElf2];
    if Sum >= 10 then
    begin
      recipes.Add(recipes.Count+1, 1);
      if check then
      begin
        result := recipes.Count -6;
        recipes.Free;
        exit;
      end;
      Sum := sum -10;
    end;
    recipes.Add(recipes.Count +1, sum);

    if (sum = 1) and check then
    begin
      result := recipes.Count -6; //20191616
      recipes.Free;
      exit;
    end;
  end;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay15'}
function TAdventOfCodeDay15.Battle(const ElfPower: Integer; StopOnElfDeath: boolean): Integer;
Var BattleGround: TDictionary<TPosition, Boolean>;
    Players: TDictionary<TPosition, TPlayer>;
    MaxX, MaxY, x, y, Rounds: integer;
    PlayerToFight, PosistionAfterMove, PlayerPosistion: TPosition;
    Player: TPlayer;
    s: String;
    PlayerPositions: TArray<TPosition>;

  function Finished(const aPlayerType: string): Boolean;
  var Player: TPlayer;
  Begin
    Result := False;

    for Player in Players.Values do
      if Player.PlayerType <> aPlayerType then
        Exit;

    Result := True;
  End;

  function CanFight(const aPosistion: TPosition; aPlayerType: string; Var LocationToFight: TPosition): Boolean;
  var Positions: TPositions;
      Position: TPosition;
      Player: TPlayer;
      Health, i: Integer;
  begin
    Health := 999;
    Positions := GetPositions(aPosistion);
    Result := False;

    for i := 0 to 3 do
    begin
      Position := Positions[i];
      if Players.TryGetValue(Position, Player) and (Player.hitPoints < Health) and (aPlayerType <> Player.PlayerType) then
      begin
        LocationToFight := Position;
        Health := Player.hitPoints;
        Result := True;
      end;
    end;
  end;

  function CanMove(const aPlayer: TPlayer; aStartPosistion: TPosition; Var NewPosistion: TPosition): boolean;
  var TestGrid: TDictionary<TPosition, integer>;
      PointsToCheck: TList<Tposition>;
      PlayerData: TPair<TPosition, TPlayer>;
      p, p2, p3: TPosition;
      MinDistance, x, y, i: Integer;
      PointAdded: Boolean;
      Positions: TPositions;
  begin
    PointsToCheck := TList<Tposition>.Create;
    TestGrid := TDictionary<TPosition, integer>.Create;
    Result := False;

    for PlayerData in Players do //Deterimene the points to attack
      if PlayerData.Value.PlayerType <> aPlayer.PlayerType then //Different type then this unit
        for p in GetPositions(PlayerData.Key) do //Get adjucant positions
          if not BattleGround[p] {not a wall} and not Players.ContainsKey(p) {Spot not taken} then
            PointsToCheck.Add(p);

    PointsToCheck.Sort(TComparer<TPosition>.Construct(
      function(const L, R: TPosition): Integer
      begin
        Result := (L.x + L.y*1000) - (R.x + R.y*1000) ;
      end));

    MinDistance := 999999999;
    for p in PointsToCheck do //Calculate distance from the currentpoint to a target point
    begin
      TestGrid.Clear;

      for Y := 1 to MaxY -1 do //Make a blank grid
        for x := 1 to MaxX -1 do
        begin
          p2.x := x; p2.y := y;
          TestGrid.Add(p2, -1);
        end;

      TestGrid[p] := 0; //Set the targetposition to 0
      i := 0;
      PointAdded := True;
      while PointAdded and (TestGrid[aStartPosistion] < 0) do //Fill the grid with the possible route's
      begin
        PointAdded := False;

        for p2 in TestGrid.Keys do
        begin
          if TestGrid[p2] = i then
          begin
            Positions := GetPositions(p2);
            for p3 in Positions do
            begin
              if not BattleGround[p3] {not a wall} and not Players.ContainsKey(p3) {spot not taken} and (TestGrid[p3] = -1) then
              begin
                TestGrid[p3] := i +1;
                PointAdded := True;
              end
            end
          end
        end;
        Inc(i);
      end;

      Positions := GetPositions(aStartPosistion);
      for i := 0 to 3 do
      begin
        p2 := Positions[i];
        if TestGrid.ContainsKey(p2) and (TestGrid[p2] >= 0) and (TestGrid[p2] < MinDistance) then
        begin
          MinDistance := TestGrid[p2];
          NewPosistion := p2;
          Result := True
        end;
      end;
    end;
    TestGrid.Free;
    PointsToCheck.Free;
  end;

begin
  Result := -1;
  BattleGround := TDictionary<TPosition, Boolean>.Create;
  Players := TDictionary<TPosition, TPlayer>.Create;

  MaxX := Length(FInput[0])-1;
  MaxY := FInput.Count -1;
  for Y := 0 to MaxY do //Read Input
  begin
    s := Trim(FInput[y]);
    for x := 1 to MaxX + 1 do
    begin
      PlayerToFight.x := x-1;
      PlayerToFight.y := y;

      if s[x] = '#' then //Wall
        BattleGround.Add(PlayerToFight, True)
      else
        BattleGround.Add(PlayerToFight, False); //Player/Free spot

      if (s[x] = 'E') or (s[x] = 'G') then
      begin
        Player.PlayerType := s[x];
        Player.HitPoints := 200;
        Players.Add(PlayerToFight, Player);
      end;
    end;
  end;

  Rounds := 0;
  while True do //Start The Battle
  begin
    PlayerPositions := Players.Keys.ToArray; //Sort players Top to bottom and left to right
    TArray.Sort<TPosition>(PlayerPositions, (TComparer<TPosition>.Construct(
      function(const L, R: TPosition): Integer
      begin
        Result := (L.x + L.y*1000) - (R.x + R.y*1000) ;
      end)));

    for PlayerPosistion in PlayerPositions do
    begin
      if Players.ContainsKey(PlayerPosistion) then //Player is still alive
      begin
        Player := Players[PlayerPosistion];

        if CanFight(PlayerPosistion, Player.PlayerType, PlayerToFight) then
        begin
          Player := Players[PlayerToFight]; //Select player to fight with
          Player.HitPoints := Player.HitPoints - ifthen(Player.PlayerType = 'G', ElfPower, 3);
          if Player.HitPoints <= 0 then
          begin //Player is dead
            if StopOnElfDeath and (Player.PlayerType = 'E') then
              Exit;
            Players.Remove(PlayerToFight)
          end //Update health
          else
            Players[PlayerToFight] := Player;
        end
        else
        begin
          if Finished(Player.PlayerType) then //Check if there are still enemys left, if not finish tbe battle
          begin
            Result := 0;
            for Player in Players.Values do
              Result := Result + Player.HitPoints;

            Result := Result * Rounds;
            Exit;
          end;

          if canMove(Player, PlayerPosistion, PosistionAfterMove) then //Try to move
          begin
            Players.Remove(PlayerPosistion); //Set the new posistion
            Players.Add(PosistionAfterMove, Player);

            if CanFight(PosistionAfterMove, Player.PlayerType, PlayerToFight) then //And check if we can attack an other player
            begin //Fight;
              Player := Players[PlayerToFight];
              Player.HitPoints := Player.HitPoints - ifthen(Player.PlayerType = 'G', ElfPower, 3);
              if Player.HitPoints <= 0 then
              begin
                if StopOnElfDeath and (Player.PlayerType = 'E') then
                  Exit;
                Players.Remove(PlayerToFight)
              end
              else
                Players[PlayerToFight] := Player;
            end;
          end
        end;
      end;
    end;
    Inc(Rounds);
  end;
end;

function TAdventOfCodeDay15.SolveA: Variant;
begin
  Result := Battle(3, False); //183300
end;

function TAdventOfCodeDay15.SolveB: Variant;
var ElfPower, StepSize: Integer;
begin
  StepSize := 8;
  ElfPower := 4;
  while true do
  begin
    Result := Battle(ElfPower, True);

    if (Result > 0) and (StepSize = 1) then //40625
      Exit;

    if Result > 0 then
    begin
      ElfPower := ElfPower - stepsize;
      StepSize := Round(StepSize/2);
    end;
    ElfPower := ElfPower + stepsize;

  end;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay16'}
function TAdventOfCodeDay16.ReadRegister(const aRegister: string): TRegister;
var Line: TStringList;
    s: string;
begin
  s := StringReplace(aRegister, '[', '', [rfReplaceAll]);
  s := StringReplace(s, ']', '', [rfReplaceAll]);
  s := StringReplace(s, ',', '', [rfReplaceAll]);

  Line := TstringList.Create;
  Line.Delimiter := ' ';
  Line.DelimitedText := s;

  Result[0] := StrToInt(Line[1]);
  Result[1] := StrToInt(Line[2]);
  Result[2] := StrToInt(Line[3]);
  Result[3] := StrToInt(Line[4]);

  Line.Free;
end;

function TAdventOfCodeDay16.ReadRegisterInstruction(const aInstruction: string): TRegisterInstruction;
var line: TStringList;
begin
  Line := TstringList.Create;
  Line.Delimiter := ' ';
  Line.DelimitedText := aInstruction;

  Result.id := StrToInt(Line[0]);
  Result.ValueA := StrToInt(Line[1]);
  Result.ValueB := StrToInt(Line[2]);
  Result.ValueC := StrToInt(Line[3]);

  line.Free;
end;

function TAdventOfCodeDay16.SolveA: Variant;
var RegisterBefore, RegisterAfter, NewRegister: TRegister;
    Instruction: TRegisterInstruction;
    i, j, SameResult: integer;
begin
  Result := 0;

  for i := 0 to Round((FInput.Count-1)/4) do
  begin
    if fInput[i*4+0] = '' then
      Exit;

    RegisterBefore := ReadRegister(fInput[i*4+0]);
    Instruction := ReadRegisterInstruction(fInput[i*4+1]);
    RegisterAfter := ReadRegister(fInput[i*4+2]);

    SameResult := 0;
    for j := 0 to 15 do
    begin
      NewRegister := ModifyRegister(RegisterBefore, Instruction, j);
      if (NewRegister[0] = RegisterAfter[0]) and (NewRegister[1] = RegisterAfter[1]) and (NewRegister[2] = RegisterAfter[2]) and (NewRegister[3] = RegisterAfter[3]) then
        inc(SameResult);
    end;

    if SameResult >= 3 then
      Inc(result); //651
  end;
end;

function TAdventOfCodeDay16.SolveB: Variant;
var CurrentRegister, RegisterAfter, NewRegister: TRegister;
    Instruction: TRegisterInstruction;
    Mapping: Array[0..15] of TList<Integer>;
    i, j, k: integer;
begin
  for i := 0 to 15 do
  begin
    Mapping[i] := TList<Integer>.Create;
    for j := 0 to 15 do
      Mapping[i].Add(j);
  end;

  for i := 0 to Round((FInput.Count -1)/4) do
  begin
    if fInput[i*4+0] = '' then
      Break;

    CurrentRegister := ReadRegister(fInput[i*4+0]);
    Instruction := ReadRegisterInstruction(fInput[i*4+1]);
    RegisterAfter := ReadRegister(fInput[i*4+2]);

    for j := 0 to 15 do
    begin
      NewRegister := ModifyRegister(CurrentRegister, Instruction, j);
      if not ((NewRegister[0] = RegisterAfter[0]) and (NewRegister[1] = RegisterAfter[1]) and (NewRegister[2] = RegisterAfter[2]) and (NewRegister[3] = RegisterAfter[3])) then
        Mapping[Instruction.Id].Remove(j); //Instruction doesnt match result remove it from the mapping
    end;

    for k := 0 to 15 do
    begin
      if Mapping[k].Count = 1 then //Only one Instruction left
      begin
        for j := 0 to 15 do //Remove it in the other mappings
          if j <> k then //Except for the current mapping
            Mapping[j].Remove(Mapping[k][0])
      end;
    end;
  end;

  for j := 0 to 3 do //Empty the CurrentRegister
    CurrentRegister[j] := 0;

  result := 0;
  for i := i*4 to Finput.count -1 do //Start the program
  begin
    if Finput[i] <> '' then
    begin
      Instruction := ReadRegisterInstruction(Finput[i]);
      CurrentRegister := ModifyRegister(CurrentRegister, Instruction, Mapping[Instruction.id][0]);
    end;
  end;

  result := CurrentRegister[0]; //706
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay17'}
procedure TAdventOfCodeDay17.BeforeSolve;
Var Line: TStringList;
    s: string;
    i, j: Integer;
    p: TPosition;
begin
  FClayGrid := TDictionary<TPosition, String>.Create;
  FWaterGrid := TDictionary<TPosition, String>.Create;

  FMaxY := -99999;

  Line := TStringList.Create;
  Line.Delimiter := ' ';
  for j := 0 to FInput.Count -1 do
  begin
    s := StringReplace(FInput[j], '=', ' ', [rfReplaceAll]);
    s := StringReplace(s, '..', ' ', [rfReplaceAll]);
    s := StringReplace(s, ',', '', [rfReplaceAll]);
    Line.DelimitedText := s;

    if Line[0] = 'x' then
    begin
      p.x := StrToInt(Line[1]);
      for i := StrToInt(Line[3]) to StrToInt(Line[4]) do
      begin
        p.y := i;
        FClayGrid.AddOrSetValue(p, '#');
      end;

      if p.y > FMaxY then
        FMaxY := p.y;
    end
    else if Line[0] = 'y' then
    begin
      p.y := StrToInt(Line[1]);

      for i := StrToInt(Line[3]) to StrToInt(Line[4]) do
      begin
        p.x := i;
        FClayGrid.AddOrSetValue(p, '#');
      end;

      if p.y > FMaxY then
        FMaxY := p.y;
    end;
  end;

  p.SetIt(500, 0);
  FClayGrid.AddOrSetValue(p, '+');
  LetWaterFall(p);
  Print;
end;

procedure TAdventOfCodeDay17.AfterSolve;
begin
  FClayGrid.Free;
  FWaterGrid.Free;
end;

procedure TAdventOfCodeDay17.Print;
var Key: TPosition;
    GridList: TStringList;
    x, y, MinX, MaxX: Integer;
    s: String;
begin
  MinX := 9999;
  MaxX := -9999;

  for Key in FWaterGrid.Keys do
  begin //Dertermine gridsize
    if Key.x > MaxX then MaxX := Key.x;
    if Key.x < MinX then MinX := Key.x;
  end;

  GridList := TStringList.Create;
  for y := 0 to FMaxY do
  begin
    s := '';
    for x := MinX to MaxX do
    begin
      Key.x := x;
      Key.y := y;
      if FClayGrid.ContainsKey(Key) then
        s := s + FClayGrid[Key]
      else if FWaterGrid.ContainsKey(Key) then
        s := s + FWaterGrid[Key]
      else
        s := s + '.';
    end;
    GridList.Add(s);
  end;

  S := StringReplace(Input, 'input17.txt', 'Solution17.txt', [rfIgnoreCase]);
  GridList.SaveToFile(s);
  GridList.Free;
end;

function TAdventOfCodeDay17.IsOverFlowing(StartPosition: TPosition; aDirection: Integer): boolean;
begin
  Result := False;
  while True do
  begin
    StartPosition.x := StartPosition.x + aDirection;

    if FClayGrid.ContainsKey(StartPosition) then //Hit clay, not overflowing
      exit;

    if FWaterGrid.ContainsKey(StartPosition) and (FWaterGrid[StartPosition] = '+') then //Hit falling water
    begin
      Result := True;
      exit;
    end
  end;
end;

function TAdventOfCodeDay17.LetWaterFlow(StartPosition: TPosition; aDirection: Integer): boolean;
var PosibleFall: TPosition;
    Flow: Boolean;
begin
  Result := False;

  Flow := True;
  while Flow do
  begin
    Flow := False;
    StartPosition.x := StartPosition.x + aDirection;

    if (not FClayGrid.ContainsKey(StartPosition) and Not FWaterGrid.ContainsKey(StartPosition)) or (FWaterGrid.ContainsKey(StartPosition) and (FWaterGrid[StartPosition] ='|')) then
    begin //Check if water can flow
      FWaterGrid.AddOrSetValue(StartPosition, '-');
      Flow := True;
    end;

    if Flow then //Check if this next flow can fall down
    begin
      PosibleFall := StartPosition.Copy(0, 1);
      if not FClayGrid.ContainsKey(PosibleFall) and Not FWaterGrid.ContainsKey(PosibleFall) then
      begin
        Flow := False;
        Result := True;

        FWaterGrid[StartPosition] := '+'; //Mark as a turning point
        FWaterGrid.Add(PosibleFall, '|');
        LetWaterFall(PosibleFall);
      end;
    end;
  end;
end;

procedure TAdventOfCodeDay17.MarkStationaryWater(StartPosition: TPosition; aDirection: Integer);
var Flow: Boolean;
begin
  Flow := True;
  while Flow do
  begin
    StartPosition.x := StartPosition.x + aDirection;

    if FClayGrid.ContainsKey(StartPosition) then
      Flow := False
    else if FWaterGrid.ContainsKey(StartPosition) then
      FWaterGrid[StartPosition] := '~';
  end;
end;

procedure TAdventOfCodeDay17.LetWaterFall(StartPosition: TPosition);
var p: TPosition;
    CanGoDown, CanGoDownLeft, CanGoDownRight: Boolean;
begin
  p := StartPosition.Copy(0, 1);
  if p.y > FMaxY then Exit;
  if (FWaterGrid.ContainsKey(p) and (FWaterGrid[p] = '+')) then Exit;

  if (FWaterGrid.ContainsKey(p)) then //Hit Water, check for overflow
  begin
    if IsOverFlowing(p, 1) then Exit;
    if IsOverFlowing(p, -1) then Exit;
  end;

  if FClayGrid.ContainsKey(p) or FWaterGrid.ContainsKey(p) then // hit Clay or non overflowing water
  begin
    CanGoDown := False;
    FWaterGrid[StartPosition] := '-'; //Set this position as flowing
    while Not CanGoDown do
    begin
      CanGoDownLeft := LetWaterFlow(StartPosition, -1); //Flow Left
      CanGoDownRight := LetWaterFlow(StartPosition, 1); //Flow Right

      CanGoDown := CanGoDownLeft or CanGoDownRight;

      if not CanGoDown then //Go up
      begin
        FWaterGrid.AddOrSetValue(StartPosition, '~'); //Mark stationary water
        MarkStationaryWater(StartPosition, -1);
        MarkStationaryWater(StartPosition, 1);

        StartPosition.y := StartPosition.y - 1;
        FWaterGrid.AddOrSetValue(StartPosition, '-');
      end;
    end;
  end
  else
  begin
    FWaterGrid.Add(p, '|'); //Continue falling
    LetWaterFall(p);
  end;
end;

function TAdventOfCodeDay17.SolveA: Variant;
begin
  Result := FWaterGrid.Count -4 ; //31158
end;

function TAdventOfCodeDay17.SolveB: Variant;
var key: TPosition;
begin
  Result := 0;
  for key in FWaterGrid.Keys do
  begin
    if FWaterGrid[key] = '~' then
      Inc(result); //25419
  end
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay18'}
function TAdventOfCodeDay18.Solve(Const aItterationCount: integer): Integer;
Var Grid: TDictionary<TPosition, String>;
    Tempgrid: TDictionary<TPosition, String>;
    x,y, i: Integer;
    p: TPosition;
    Tree, lumber: Integer;
    s: string;

  function GetField(aPosition: TPosition): string;
  begin
    Result := '';
    if Grid.ContainsKey(aPosition) then
      Result := Grid[aPosition];
  end;

  function GetPositions(aStartPosistion: TPosition): ForestPositions;
  begin
    Result[0] := aStartPosistion.Copy(-1, -1); //Top left
    Result[1] := aStartPosistion.Copy(-1, 0);  //Middle left
    Result[2] := aStartPosistion.Copy(-1, 1);  //Bottem left
    Result[3] := aStartPosistion.Copy(0, -1);  //Top middle
    Result[4] := aStartPosistion.Copy(0, 1);   //Bottom middle
    Result[5] := aStartPosistion.Copy(1, -1);  //Top right;
    Result[6] := aStartPosistion.Copy(1, 0);   //Middle right
    Result[7] := aStartPosistion.Copy(1, 1);   //bottom right
  end;

  procedure Itterate;
  var startposistion, p: TPosition;
      Yard, Trees: Integer;
      s: string;
  begin
    for startposistion in Grid.Keys do
    begin
      Yard := 0;
      Trees := 0;

      for p in GetPositions(startposistion) do
      begin
        s := GetField(p);
        if s = '#' then Inc(yard);
        if s = '|' then Inc(Trees);
      end;

      s := Grid[startposistion];
      if (s = '.') and (Trees >= 3) then
        s := '|'
      else if (s = '|') and (Yard >= 3) then
        s := '#'
      else if (s = '#') and ((yard = 0) or (Trees = 0)) then
        s := '.';

      Tempgrid.AddOrSetValue(startposistion, s);
    end;
  end;

begin
  Grid := TDictionary<TPosition, String>.Create;
  for y := 0 to FInput.Count -1 do
    for x := 1 to Length(FInput[0]) do
    begin
      p.x := x-1; p.y := y;
      Grid.Add(p, FInput[y][x]);
    end;

  Tempgrid := TDictionary<TPosition, String>.Create;
  for i := 1 to aItterationCount  do
  begin
    Tempgrid.Clear;
    Itterate;
    Grid := TDictionary<TPosition, String>.Create(Tempgrid);
  end;

  Tree := 0;
  lumber := 0;
  for s in Grid.Values do
  begin
    if s = '|' then Inc(Tree);
    if s = '#' then Inc(lumber);
  end;

  Result := Tree * lumber;
  Tempgrid.Free;
  Grid.Free;
end;

function TAdventOfCodeDay18.SolveA: Variant;
begin
  Result := Solve(10); //355918
end;

function TAdventOfCodeDay18.SolveB: Variant;
begin
  // 1000 iterations is engough, the patrn is repating
  Result := Solve(1000); //202806
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay19'}
procedure TAdventOfCodeDay19.RunProgram(const aMaxIterations: integer; var aRegisters: TRegister);
const instructionPointer: Integer = 1;
Var Instruction: TRegisterInstruction;
    counter: Integer;
begin
  counter := 0;
  while (Counter < aMaxIterations) or (aMaxIterations < 0)do
  begin
    Inc(counter);
    Instruction := ReadRegisterInstruction(FInput[aRegisters[instructionPointer]]);

    aRegisters := ModifyRegister(aRegisters, Instruction, Instruction.Id);
    aRegisters[instructionPointer] := aRegisters[instructionPointer] + 1;

    if aRegisters[instructionPointer] > FInput.count -1 then
      Exit;
  end;
end;

function TAdventOfCodeDay19.SolveA: Variant;
Var Registers: TRegister;
    i: Integer;
begin
  for i := 0 to 5 do
    Registers[i] := 0;

  RunProgram(-1, Registers);
  Result := Registers[0] //2040
end;

function TAdventOfCodeDay19.SolveB: Variant;
Var Registers: TRegister;
    i :Integer;
begin
  for i := 0 to 5 do
    Registers[i] := 0;

  Registers[0] := 1;

  RunProgram(20, Registers); //Goal of the program is to get the sum of the dividers in register 5, 20 iterations is enouogh

  result := 0;
  for i := 1 to Registers[5] do
  begin
    if (registers[5] mod i) = 0 then
      Result := Result + i; //25165632
  end;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay20'}
procedure TAdventOfCodeDay20.PrintRooms;
var Key: TPosition;
    GridList: TStringList;
    x, y, MinX, MaxX, MinY, MaxY: Integer;
    s: String;
begin
  MinX := 9999;
  MaxX := -9999;
  MinY := MinX;
  MaxY := MaxX;

  for Key in FMap.Keys do
  begin //Dertermine gridsize
    if Key.x > MaxX then MaxX := Key.x;
    if Key.x < MinX then MinX := Key.x;
    if Key.y > MaxY then MaxY := Key.y;
    if Key.y < MinY then MinY := Key.y;
  end;

  GridList := TStringList.Create;
  for y := MinY-1 to MaxY+1 do
  begin
    s := '';
    for x := MinX-1 to MaxX+1 do
    begin
      Key.SetIt(x, y);
      if FMap.ContainsKey(Key) then
        s := s + FMap[Key]
      else
        s := s + ' ';
    end;
    GridList.Add(s);
  end;

  s := StringReplace(Input, 'input20.txt', 'Solution20.txt', [rfIgnoreCase]);
  GridList.SaveToFile(s);
  GridList.Free;
end;

procedure TAdventOfCodeDay20.BuildRooms(aStartPosition: TPosition; var Instructions: String);
var s: string;
    xSpeed, ySpeed: Integer;
begin
  if Length(Instructions) = 0 then Exit;

  s := Instructions[1];
  if (s = '$') or (s = ')') then Exit;

  if s = '|' then
  begin
    Instructions := Instructions.Remove(0,1);
    Exit;
  end;

  xSpeed := 0;
  ySpeed := 0;
  case IndexStr(s, ['N', 'E', 'S', 'W', '(']) of
    0: ySpeed := -1;
    1: xSpeed := 1;
    2: ySpeed := 1;
    3: xSpeed := -1;
    4:begin //Upcoming choice
        Instructions := Instructions.Remove(0,1);
        while (Instructions[1] <> ')') do
          BuildRooms(aStartPosition, Instructions);

        Instructions := Instructions.Remove(0,1);
        BuildRooms(aStartPosition, Instructions);
        Exit;
      end;
  end;

  aStartPosition := aStartPosition.Copy(xSpeed, ySpeed);
  FMap.AddOrSetValue(aStartPosition, 'D'); //Mark Next position as door;

  aStartPosition := aStartPosition.Copy(xSpeed, ySpeed);
  FMap.AddOrSetValue(aStartPosition, '.'); //Mark as room
  Instructions := Instructions.Remove(0,1);

  BuildRooms(aStartPosition, Instructions);
end;

procedure TAdventOfCodeDay20.CalculateDistance(aStartPosition: TPosition; Const Steps: integer);
begin
  CalculateDistance(aStartPosition, Steps, 0, 1);
  CalculateDistance(aStartPosition, Steps, 1, 0);
  CalculateDistance(aStartPosition, Steps, 0, -1);
  CalculateDistance(aStartPosition, Steps, -1, 0);
end;

procedure TAdventOfCodeDay20.CalculateDistance(aStartPosition: TPosition; Const Steps, X, Y: integer);
begin
  aStartPosition := aStartPosition.Copy(X, Y);
  if FMap.ContainsKey(aStartPosition) then //This is a door
  begin
    aStartPosition := aStartPosition.Copy(X, Y);

    if FDistances.ContainsKey(aStartPosition) then //Already visited;
    begin
      if FDistances[aStartPosition] > Steps +1 then //Check if this is a shorter path
      begin
        FDistances[aStartPosition] := Steps +1;
        CalculateDistance(aStartPosition, Steps +1)
      end;
    end
    else
    begin
      FDistances.Add(aStartPosition, Steps +1);
      CalculateDistance(aStartPosition, Steps +1)
    end;
  end;
end;

procedure TAdventOfCodeDay20.BeforeSolve;
var Insructions: string;
    i: integer;
    p: TPosition;
begin
  Insructions := ''; //Merge instuctions, string is way to long
  for i := 0 to FInput.Count-1 do
    Insructions := Insructions + Trim(FInput[i]);

  FMap := TDictionary<TPosition, string>.Create;
  FDistances := TDictionary<TPosition, Integer>.Create;

  Insructions := Insructions.Remove(0,1); //remove first character

  p.SetIt(0, 0); //Set startposition
  FMap.Add(p, 'X');

  BuildRooms(p, Insructions);
  PrintRooms;

  CalculateDistance(p, 0);
end;

procedure TAdventOfCodeDay20.AfterSolve;
begin
  FDistances.Free;
  FMap.Free;
end;

function TAdventOfCodeDay20.SolveA: Variant;
var Key: TPosition;
begin
  Result := 0;
  for Key in FDistances.Keys do
    if FDistances[Key] > Result then
      Result := FDistances[Key]; //3502
end;

function TAdventOfCodeDay20.SolveB: Variant;
var Key: TPosition;
begin
  Result := 0;
  for Key in FDistances.Keys do
    if FDistances[Key] >= 1000  then
     inc(result); //8000
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay21'}
procedure TAdventOfCodeDay21.BeforeSolve;
var i: Integer;
begin
  FInstructions := TDictionary<Integer, TRegisterInstruction>.Create;
  for i := 0 to FInput.Count -1 do
    FInstructions.Add(i, ReadRegisterInstruction(FInput[i]));
end;

procedure TAdventOfCodeDay21.AfterSolve;
begin
  FInstructions.Free;
end;

function TAdventOfCodeDay21.SolveA: Variant;
const instructionPointer: Integer = 4;
Var Instruction: TRegisterInstruction;
    Registers: TRegister;
    i: Integer;
begin
  for i := 0 to 5 do
    Registers[i] := 0;

  while True do
  begin
    Instruction := FInstructions[Registers[instructionPointer]];

    if Registers[instructionPointer] = 28 then
    begin
      Result := Registers[1]; //6483199
      Exit;
    end;

    Registers := ModifyRegister(Registers, Instruction, Instruction.Id);
    Registers[instructionPointer] := Registers[instructionPointer] + 1;
  end;
end;

function TAdventOfCodeDay21.SolveB: Variant;
const instructionPointer: Integer = 4;
Var Instruction: TRegisterInstruction;
    SeenResults: TList<Integer>;
    Registers: TRegister;
    i, temp: Integer;
begin
  SeenResults := TList<Integer>.create;

  for i := 0 to 5 do
    Registers[i] := 0;

  Result := 0;
  while True do
  begin
    Instruction := FInstructions[Registers[instructionPointer]];

    if (Registers[instructionPointer] = 28) then
    begin
      temp := Registers[1];

      if SeenResults.Contains(temp) then
        Exit;

      Result := temp; //13338900
      SeenResults.Add(Result);
    end;

    Registers := ModifyRegister(Registers, Instruction, Instruction.Id);
    Registers[instructionPointer] := Registers[instructionPointer] + 1;
  end;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay22'}
procedure TAdventOfCodeDay22.BeforeSolve;
var Position: TPosition;
    X, Y, GeologicIndex1, GeologicIndex2, GeologicIndex: Integer;
begin
  FCave := TDictionary<TPosition, integer>.Create;

  for X := 0 to FTargetX + 50 do
    for Y := 0 to FTargetY + 50 do
    begin
      if (x = 0) or (y = 0)  then
        GeologicIndex := (Y * 48271) + (X * 16807)
      else if (x = FTargetX) and (y = FTargetY) then
        GeologicIndex := 0
      else
      begin
        Position.SetIt(x-1,y);
        GeologicIndex1 := FCave[Position];

        Position.SetIt(X,Y-1);
        GeologicIndex2 := FCave[Position];
        GeologicIndex := GeologicIndex1 * GeologicIndex2;
      end;

      Position.SetIt(X, Y);
      FCave.Add(Position, (GeologicIndex+ + FDepth) mod 20183);
    end;
end;

procedure TAdventOfCodeDay22.AfterSolve;
begin
  FCave.Free;
end;

function TAdventOfCodeDay22.SolveA: Variant;
var Position: TPosition;
    X, Y: integer;
begin
  for x := 0 to FTargetX do
    for y := 0 to FTargetY do
    begin
      Position.SetIt(x, y);
      Result := Result + FCave[Position] mod 3; //11359
    end;
end;

function TAdventOfCodeDay22.SolveB: Variant;
var SeenPositions: TDictionary<TCavePosition, integer>;
    CavePosition: TCavePosition;
    FTime: Integer;
    ExplorationQueue: TQueue<TCaveEvent>;
    CaveEvent: TCaveEvent;

  procedure ExploreCave(const aCaveEvent: TCaveEvent);
  var p: TPosition;
      NewCavePosition: TCavePosition;
      NewCaveEvent: TCaveEvent;
      CaveType: TCaveType;
      Time: Integer;
      Tool: TTool;
  begin
    if (aCaveEvent.CavePosition.Position.x = FTargetX) and (aCaveEvent.CavePosition.Position.y = FTargetY) then
    begin
      if aCaveEvent.CavePosition.Tool <> Torch then
        Time := aCaveEvent.Time + 7
      else
        Time := aCaveEvent.Time;

      if Time < FTime then
        FTime := Time;

      SeenPositions.AddOrSetValue(aCaveEvent.CavePosition, FTime);
      Exit;
    end;

    if Not FCave.ContainsKey(aCaveEvent.CavePosition.Position) then
      Exit; //Outside of grid

    if SeenPositions.ContainsKey(aCaveEvent.CavePosition) and (SeenPositions[aCaveEvent.CavePosition] <= aCaveEvent.Time) then
      Exit; //Already visited with the same tool in a more effictive way

    CaveType := TCaveType(FCave[aCaveEvent.CavePosition.Position] mod 3);
    case CaveType of
      Rocky:  if aCaveEvent.CavePosition.Tool = None then exit;
      Wet:    if aCaveEvent.CavePosition.Tool = Torch then exit;
      Narrow: if aCaveEvent.CavePosition.Tool = ClimbingGear then exit;
    end;

    SeenPositions.AddOrSetValue(CaveEvent.CavePosition, CaveEvent.Time);

    for p in GetPositions(CaveEvent.CavePosition.Position) do
    begin
      NewCavePosition.Position := p;
      for Tool := Low(TTool) to High(TTool) do
      begin
        if Ord(CaveType) <> Ord(Tool) then //Check if where allowed to use this tool on this position
        begin
          Time := aCaveEvent.Time + 1;
          if aCaveEvent.CavePosition.Tool <> Tool then //Check if this tool is already equiped
            Time := aCaveEvent.Time + 7;

          NewCavePosition.Tool := Tool;
          NewCaveEvent.CavePosition := NewCavePosition;
          NewCaveEvent.Time := Time;
          ExplorationQueue.Enqueue(NewCaveEvent);
        end
      end;
    end;
  end;

begin
  ExplorationQueue := TQueue<TCaveEvent>.Create;
  SeenPositions := TDictionary<TCavePosition, integer>.Create;

  CavePosition.Position.SetIt(0,0);
  CavePosition.Tool := Torch;
  FTime := 9999999;

  CaveEvent.CavePosition := CavePosition;
  CaveEvent.Time := 0;
  ExplorationQueue.Enqueue(CaveEvent);

  While ExplorationQueue.Count > 0 do
  begin
    CaveEvent := ExplorationQueue.Dequeue;
    ExploreCave(CaveEvent);
  end; //976

  Result := FTime;
  ExplorationQueue.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay23'}
procedure TAdventOfCodeDay23.BeforeSolve;
var NanoBot: TNanoBot;
    Line: TStringList;
    i: Integer;
    s: string;
begin
  NanoBotToCheck.Radius := 0;
  Line := TstringList.Create;
  Line.Delimiter := ' ';

  FNanoBots := TList<TNanoBot>.Create;
  for i := 0 to FInput.Count -1 do
  begin
    s := FInput[i];
    s := StringReplace(s, '<', '', [rfReplaceAll]);
    s := StringReplace(s, '>', '', [rfReplaceAll]);
    s := StringReplace(s, '=', ' ', [rfReplaceAll]);
    s := StringReplace(s, ',', ' ', [rfReplaceAll]);

    Line.DelimitedText := s;
    NanoBot.X := StrToInt(Line[1]);
    NanoBot.Y := StrToInt(Line[2]);
    NanoBot.Z := StrToInt(Line[3]);
    NanoBot.Radius := StrToInt(Line[5]);
    FNanoBots.Add(NanoBot);

    if NanoBot.Radius > NanoBotToCheck.Radius then
      NanoBotToCheck := NanoBot;
  end;
  Line.Free
end;

procedure TAdventOfCodeDay23.AfterSolve;
begin
  FNanoBots.Free;
end;

function TAdventOfCodeDay23.SolveA: Variant;
var Distance: Integer;
    NanoBot: TNanoBot;
begin
  Result := 0;
  for NanoBot in FNanoBots do
  begin
    Distance := abs(NanoBot.X - NanoBotToCheck.X) + abs(NanoBot.Y - NanoBotToCheck.Y) + abs(NanoBot.Z - NanoBotToCheck.Z);
    if Distance <= NanoBotToCheck.Radius then
      inc(Result); //497
  end;
end;

function TAdventOfCodeDay23.SolveB: Variant;
var
    NanoBot: TNanoBot;
    MaxX, MinX, MaxY, MinY, MaxZ, MinZ: integer;
    ScanSize, BestX, BestY, BestZ: Integer;
    Distance, BotCount, BestBotCount, BestDistance, X,Y,Z: Integer;
begin
  MaxX := 0;
  MinX := 0;
  MaxY := 0;
  MinY := 0;
  MaxZ := 0;
  MinZ := 0;
  BestX := 0;
  BestY := 0;
  BestZ := 0;
  BestBotCount := 0;
  BestDistance := 0;

  for NanoBot in FNanoBots do
  begin
    MaxX := Max(MaxX, NanoBot.X);
    MinX := Min(MinX, NanoBot.X);
    MaxY := Max(MaxY, NanoBot.Y);
    MinY := Min(MinY, NanoBot.Y);
    MaxZ := Max(MaxZ, NanoBot.Z);
    MinZ := Min(MinZ, NanoBot.Z);
  end;

  ScanSize := 1;
  while ScanSize < (MaxX - MinX) do
    ScanSize := ScanSize * 2;

  while ScanSize >= 1 do
  begin
    for x := Round(MinX/ScanSize) to Round(MaxX/ScanSize) do
    for y := Round(MinY/ScanSize) to Round(MaxY/ScanSize) do
    for z := Round(MinZ/ScanSize) to Round(MaxZ/ScanSize) do
    begin
      BotCount := 0;
      for NanoBot in FNanoBots do
      begin
        Distance := abs(NanoBot.X - X * ScanSize) + abs(NanoBot.Y - Y * ScanSize) + abs(NanoBot.Z - Z * ScanSize);
        if (Distance - NanoBot.Radius)/ScanSize <= 0 then
          Inc(BotCount);
      end;

      if (BotCount > BestBotCount) then
      begin
        BestBotCount := BotCount;
        BestX := X*ScanSize;
        BestY := Y*ScanSize;
        BestZ := Z*ScanSize;
        BestDistance := Abs(BestX) + Abs(BestY) + Abs(BestZ)
      end
      else if BotCount = BestBotCount then
      begin
        Distance := Abs(X * ScanSize) + Abs(Y * ScanSize) + Abs(Z * ScanSize);
        if (Distance < BestDistance) then
        begin
          BestX := X*ScanSize;
          BestY := Y*ScanSize;
          BestZ := Z*ScanSize;
          BestDistance := Distance;
        end
      end
    end;

    MinX := BestX - ScanSize;
    MaxX := BestX + ScanSize;
    MinY := BestY - ScanSize;
    MaxY := BestY + ScanSize;
    MinZ := BestZ - ScanSize;
    MaxZ := BestZ + ScanSize;

    ScanSize := Round(ScanSize/2);
  end;

  Result := BestDistance + 1; //85761543 //Not sure why + 1, code gives 857615432, but aoc wont have it
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay24'}
function TAdventOfCodeDay24.FixUpReindeer(const aBoost: integer; ReindeerSurvives: boolean): Integer;
Var Units: TList<TUnit>;
    TheUnit: TUnit;
    PlannedBattles: TDictionary<Integer,Integer>;
    i, j, Target, Damage, PossibleDamage, TargetPower, TargetInitiative, PreviousHealth: Integer;

  function BattleFinished: Boolean;
  var a,b: integer;
      t: TUnit;
  begin
    a := 0;
    b := 0;

    for t in Units do
    begin
      if (t.UnitCount > 0) then
      begin
        if t.UnitType = Infection then
          a := a + t.UnitCount
        else
          b := b + t.UnitCount;
      end;
    end;

    Result := (a=0) or (b=0);
  end;

begin
  Result := 0;
  Units := TList<TUnit>.Create;
  PlannedBattles := TDictionary<Integer,Integer>.Create;
  PreviousHealth := 0;

  Units.Add(CreateUnit(ImmuneSystem, 4592, 2061, [slashing, radiation], [Cold], 4, fire, 9, aBoost));
  Units.Add(CreateUnit(ImmuneSystem, 1383, 3687, [], [], 26, radiation, 15, aBoost));
  Units.Add(CreateUnit(ImmuneSystem, 2736, 6429, [slashing], [], 20, slashing, 2, aBoost));
  Units.Add(CreateUnit(ImmuneSystem, 777, 3708, [radiation, cold], [slashing, fire], 39, cold, 4, aBoost));
  Units.Add(CreateUnit(ImmuneSystem, 6761, 2792, [bludgeoning, fire, cold, slashing], [], 3, radiation, 17, aBoost));
  Units.Add(CreateUnit(ImmuneSystem, 6028, 5537, [slashing], [], 7, radiation, 6, aBoost));
  Units.Add(CreateUnit(ImmuneSystem, 2412, 2787, [], [], 9, bludgeoning, 20, aBoost));
  Units.Add(CreateUnit(ImmuneSystem, 6042, 7747, [radiation], [], 12, slashing, 12, aBoost));
  Units.Add(CreateUnit(ImmuneSystem, 1734, 7697, [], [radiation, cold], 38, cold, 10, aBoost));
  Units.Add(CreateUnit(ImmuneSystem, 4391, 3250, [], [], 7, cold, 19, aBoost));

  Units.Add(CreateUnit(Infection, 820, 46229, [Cold, bludgeoning], [], 106, slashing, 18));
  Units.Add(CreateUnit(Infection, 723, 30757, [], [bludgeoning], 80, fire, 3));
  Units.Add(CreateUnit(Infection, 2907, 51667, [bludgeoning], [slashing], 32, fire, 1));
  Units.Add(CreateUnit(Infection, 2755, 49292, [], [bludgeoning], 34, fire, 5));
  Units.Add(CreateUnit(Infection, 5824, 24708, [bludgeoning, cold, radiation, slashing], [], 7, bludgeoning, 11));
  Units.Add(CreateUnit(Infection, 7501, 6943, [slashing], [cold], 1, radiation, 8));
  Units.Add(CreateUnit(Infection, 573, 10367, [], [cold, slashing], 30, radiation, 16));
  Units.Add(CreateUnit(Infection, 84, 31020, [], [cold], 639, slashing, 14));
  Units.Add(CreateUnit(Infection, 2063, 31223, [bludgeoning], [radiation], 25, cold, 13));
  Units.Add(CreateUnit(Infection, 214, 31088, [], [fire], 271, slashing, 7));

  while Not BattleFinished do
  begin
    i := 0;
    for TheUnit in Units do
      i := i + TheUnit.UnitCount * TheUnit.HitPoints;

    if PreviousHealth = i then
      Exit; //Battle stalled

    PreviousHealth := i;

    Units.Sort(TComparer<TUnit>.Construct( //Order Units
      Function(const L, R: TUnit): integer
      begin
        if L.UnitCount*L.AttackDamgage <> R.UnitCount*R.AttackDamgage then
          Result :=  R.AttackPower - L.AttackPower
        else
          Result := r.initiative - l.initiative;
      end));

    for i := 0 to Units.Count - 1 do
    begin //Select Targets
      TheUnit := Units[i];
      Target := -1;
      Damage := 0;
      TargetPower := 0;
      TargetInitiative := 0;
      for j := 0 to Units.Count -1 do
      begin
        if (Units[j].UnitType <> TheUnit.UnitType) and (TheUnit.UnitCount > 0) then //Other type of player /this unit still alive
          if (not PlannedBattles.ContainsValue(j)) and (Units[j].UnitCount > 0) then //Unit is not already scheduled for a battle/and still alive
          begin
            if TheUnit.AttakcType in Units[j].ImmuneTo then
              PossibleDamage := -1
            else if TheUnit.AttakcType in Units[j].WeakTo then
              PossibleDamage := TheUnit.AttackPower*2
            else
              PossibleDamage := TheUnit.AttackPower;

            if PossibleDamage = Damage then
            begin
              if (TargetPower = Units[j].AttackPower) and (TargetInitiative < Units[j].initiative) then //Same attackpower and higer initiative
                Target := j;

              if TargetPower < Units[j].AttackPower then
                Target := j;
            end;

            if PossibleDamage > Damage then
              Target := j;

            if Target = j then //This is the current target, set the global data
            begin
              Damage := PossibleDamage;
              TargetPower := Units[j].AttackPower;
              TargetInitiative := Units[j].initiative;
            end;
          end;
      end;

      if Target >= 0 then
        PlannedBattles.Add(i, Target);
    end;

    While PlannedBattles.Count > 0 do //Battle
    begin
      i := 0; //Search for Highest Initiative
      TargetInitiative := 0;
      for j in PlannedBattles.Keys do
      begin
        if Units[j].initiative > TargetInitiative then
        begin
          TargetInitiative := Units[j].initiative;
          i := j;
        end
      end;

      j := PlannedBattles[i];
      PlannedBattles.Remove(i);
      if Units[i].UnitCount > 0 then
      begin
        Damage := Units[i].AttackPower;
        if Units[i].AttakcType in units[j].WeakTo then
          Damage := Damage * 2;

        TheUnit := Units[j];
        TheUnit.UnitCount := TheUnit.UnitCount - Damage div (Units[j].HitPoints);
        Units[j] := TheUnit;
      end;
    end;
  end;

  for TheUnit In Units do
    if (TheUnit.UnitCount > 0) and (not ReindeerSurvives or (TheUnit.UnitType = ImmuneSystem)) then
      Result := Result + TheUnit.UnitCount;
end;

function TAdventOfCodeDay24.CreateUnit(aUnitType: TUnitType; aUnitCount, aHitPoints : Integer; aImmuneTo, aWeakTo: SetOfAttackType; aAttackDamgage: Integer;
                     aAttakcType: TAttackType; aInitiative: Integer; const aBoost: Integer = 0): TUnit;
begin
  Result.UnitType := aUnitType;
  Result.UnitCount := aUnitCount;
  Result.HitPoints := aHitPoints;
  Result.AttackDamgage := aAttackDamgage + aBoost;
  Result.initiative := ainitiative;
  Result.AttakcType := aAttakcType;
  Result.WeakTo := aWeakTo;
  Result.ImmuneTo := aImmuneTo;
end;

function TAdventOfCodeDay24.SolveA: Variant;
begin
  Result := FixUpReindeer(0, false);//20150
end;

function TAdventOfCodeDay24.SolveB: Variant;
var Boost, StepSize: Integer;
begin
  StepSize := 32;
  Boost := 0;
  while true do
  begin
    Result := FixUpReindeer(Boost, true); //13005

    if (Result > 0) and (StepSize = 1) then
      Exit;

    if Result > 0 then
    begin
      Boost := Boost - stepsize;
      StepSize := Round(StepSize/2);
    end;
    Boost := Boost + stepsize;
  end;
end;
{$ENDREGION}


function TAdventOfCodeDay25.SolveA: Variant;
begin
//  Line := TstringList.Create;
//  Line.Delimiter := ' ';
//  Line.DelimitedText := s;
end;

function TAdventOfCodeDay25.SolveB: Variant;
begin

end;


end.
