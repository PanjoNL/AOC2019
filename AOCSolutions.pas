unit AOCSolutions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Defaults, System.Generics.Collections,
  system.Diagnostics, AOCBase, RegularExpressions, System.DateUtils, system.StrUtils,
  system.Math, IntComputers, uAOCUtils;

type TDirection = (Up = 0, Right, Down, Left); //If updated test day 11
  
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
    Fprogram: TDictionary<Integer, int64>;
    function RunProgram(const aNoun, aVerb: Integer): Integer;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  end;

type
  TAdventOfCodeDay3 = class(TAdventOfCode)
  protected
    PointsL, PointsR: TAOCDictionary<TPosition, Integer>;
    function LoadPoints(const Input: string): TAOCDictionary<TPosition, Integer>;
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
    function SolveA: Variant; override;
    function SolveB: Variant; override;
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

type
  TAdventOfCodeDay7 = class(TAdventOfCode)
  protected
    Fprogram: TDictionary<Integer, int64>;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    function GetPhaseSettings(Const StartAt, StopAt: Integer): TList<String>;
  end;

type
  TAdventOfCodeDay8 = class(TAdventOfCode)
  protected
    PictureDate: TList<string>;
    const PictureWith: integer = 25;
          PictureHeight: Integer = 6;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

type TAdventOfCodeDay9 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
end;

type
  TAdventOfCodeDay10 = class(TAdventOfCode)
  protected
    Map: TDictionary<TPosition, Boolean>;
    MonitoringStation : TPosition; //Needed for part b
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    function ScanAstroids(Const ScanLocation: TPosition; OutPutMap: TDictionary<Extended, TPosition>): Integer;
    function CalcAstroidAngle(Station, Astroid: TPosition): Extended;
  end;

type TAdventOfCodeDay11 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure PaintPanels(PaintedPanels: TDictionary<TPosition, Integer>);
end;

type TMoon = class
public
  X, Y, Z, VelocityX, VelocityY, VelocityZ: Integer;
  constructor Create(const MoonCoordinates: String);
  procedure CalcGravity(OtherMoon: TMoon);
  procedure ApplyVelocity;
  function CalcEnergy: Integer;
end;

type TAdventOfCodeDay12 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
end;

type TAdventOfCodeDay13 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
end;

implementation

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
begin
  Fprogram := TBasicIntComputer.ParseIntput(FInput[0]);
end;

procedure TAdventOfCodeDay2.Aftersolve;
begin
  Fprogram.Free;
end;

function TAdventOfCodeDay2.RunProgram(const aNoun, aVerb: Integer): Integer;
var Computer: TBasicIntComputer;
begin
  Computer := TBasicIntComputer.Create(Fprogram);
  Computer.WriteMemory(1, aNoun);
  Computer.WriteMemory(2, aVerb);
  Computer.Run;
  Result := Computer.GetMemory(0);
  Computer.Free;
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

function TAdventOfCodeDay3.LoadPoints(const Input: string): TAOCDictionary<TPosition, Integer>;
var Line: TStringList;
    Position: TPosition;
    Steps, i, j, DeltaX, DeltaY: Integer;
begin
  Result := TAOCDictionary<TPosition, Integer>.Create;

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
      Result.AddOrIgnoreValue(Position, Steps);
    end;
  end;

  Line.Free;
end;

function TAdventOfCodeDay3.SolveA: Variant;
var Position: TPosition;
begin
  Result := MaxInt;
  for Position in PointsL.Keys do
    if PointsR.ContainsKey(Position) then
      Result := Min(Result, Abs(Position.x) + Abs(Position.y)); //870
end;

function TAdventOfCodeDay3.SolveB: Variant;
var Point: TPair<TPosition, Integer>;
begin
  Result := MaxInt;
  for Point in PointsL do
    if PointsR.ContainsKey(Point.key) then
      Result := Min(Result, PointsR[Point.key] + Point.Value); //13698
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
        break;
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
function TAdventOfCodeDay5.SolveA: Variant;
begin
  Result := TBasicIntComputer.RunProgram(FInput[0], 1);  //2845163
end;

function TAdventOfCodeDay5.SolveB: Variant;
begin
  Result := TBasicIntComputer.RunProgram(FInput[0], 5); //9436229
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay6'}
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
{$ENDREGION}
{$Region 'TAdventOfCodeDay7'}
procedure TAdventOfCodeDay7.BeforeSolve;
begin
  Fprogram := TBasicIntComputer.ParseIntput(FInput[0]);
end;

procedure TAdventOfCodeDay7.AfterSolve;
begin
  Fprogram.Free;
end;

function TAdventOfCodeDay7.GetPhaseSettings(Const StartAt, StopAt: Integer): TList<String>;
var a,b,c,d,e: Integer;
    PhaseCheck: TDictionary<Integer,Boolean>;
begin
  PhaseCheck := TDictionary<Integer,Boolean>.Create;
  Result := TList<String>.Create;

  for A := StartAt to StopAt do
  for B := StartAt to StopAt do
  for C := StartAt to StopAt do
  for D := StartAt to StopAt do
  for E := StartAt to StopAt do
  begin
    PhaseCheck.Clear;
    PhaseCheck.AddOrSetValue(A,True);
    PhaseCheck.AddOrSetValue(B,True);
    PhaseCheck.AddOrSetValue(C,True);
    PhaseCheck.AddOrSetValue(D,True);
    PhaseCheck.AddOrSetValue(E,True);
    if PhaseCheck.Count = 5 then
      Result.Add(IntToStr(A)+IntToStr(B)+IntToStr(C)+IntToStr(D)+IntToStr(E));
  end;
  PhaseCheck.Free;
end;

function TAdventOfCodeDay7.SolveA: Variant;
var PhaseSettings: TList<String>;
    PhaseSetting: string;
    InputSignal, i: Integer;
    Amplifier: TAmplifierController;
begin
  Result := 0;
  PhaseSettings := GetPhaseSettings(0,4);

  for PhaseSetting in PhaseSettings do
  begin
    InputSignal := 0;
    for i := 0 to 4 do
    begin
      Amplifier := TAmplifierController.Create(Fprogram, StrToInt(PhaseSetting[i+1]), False);
      Amplifier.SetPhaseSetting(InputSignal);
      InputSignal := Amplifier.Run;
      Amplifier.Free;
    end;
    Result := Max(Result, InputSignal);
  end;
  PhaseSettings.Free;
end;

function TAdventOfCodeDay7.SolveB: Variant;
var PhaseSettings: TList<String>;
    PhaseSetting: string;
    i, InputSignal: Integer;
    Amplifiers: TList<TAmplifierController>;
begin
  PhaseSettings := GetPhaseSettings(5,9);
  Amplifiers := TList<TAmplifierController>.Create;

  Result := 0;

  for PhaseSetting in PhaseSettings do
  begin
    for i := 0 to 4 do
      Amplifiers.Add(TAmplifierController.Create(Fprogram, StrToInt(PhaseSetting[i+1]), True));

    InputSignal := 0;
    while not  Amplifiers[4].IsStopped do
    begin
      for i := 0 to 4 do
      begin
        Amplifiers[i].SetPhaseSetting(InputSignal);
        InputSignal := Amplifiers[i].Run;
      end;
    end;

    Result := Max(Result, Amplifiers[4].LastOutput); //33660560
    for i := 0 to 4 do
      Amplifiers[i].Free;
    Amplifiers.Clear;
  end;

  PhaseSettings.Free;
  Amplifiers.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay8'}
procedure TAdventOfCodeDay8.BeforeSolve;
var i, PictureLength: Integer;
begin
  PictureDate := TList<String>.Create;
  PictureLength := PictureHeight * PictureWith;
  i := 1;
  while i < Length(FInput[0]) do
  begin
    PictureDate.Add(Copy(FInput[0], i, PictureLength));
    Inc(i, PictureLength);
  end;
end;

procedure TAdventOfCodeDay8.AfterSolve;
begin
  PictureDate.Free;
end;

function TAdventOfCodeDay8.SolveA: Variant;
var i, Digits0, Digits1, Digits2, CountDigits0: Integer;
    PictureLayer: string;
begin
  CountDigits0 := MaxInt;
  Result := 0;
  for PictureLayer in PictureDate do
  begin
    Digits0 := 0;
    Digits1 := 0;
    Digits2 := 0;
    for i := 1 to Length(PictureLayer) do
    case StrToInt(PictureLayer[i]) of
      0: Inc(Digits0);
      1: Inc(Digits1);
      2: Inc(Digits2);
    end;

    if (Digits0 < CountDigits0) then
    begin
      Result := Digits1 * Digits2; //1463
      CountDigits0 := Digits0;
    end
  end;
end;

function TAdventOfCodeDay8.SolveB: Variant;
var i, j, X, y: Integer;
    Picture: TAOCDictionary<TPosition, Boolean>;
    Position: TPosition;
    Line: string;
    OutPut: TStringList;
begin
  Picture := TAOCDictionary<TPosition, Boolean>.Create;
  for i := 0 to PictureDate.Count - 1 do
  begin
    x := 0;
    y := 0;

    for j := 1 to Length(PictureDate[i]) do
    begin
      if x >= PictureWith then
      begin
        Dec(x, PictureWith);
        Inc(y);
      end;
      Position.SetIt(x, y);

      if StrToInt(PictureDate[i][j]) in [0, 1] then
        Picture.AddOrIgnoreValue(Position, StrToBool(PictureDate[i][j]));

      Inc(X);
    end;
  end;

  OutPut := TStringList.Create;
  for y := 0 to PictureHeight-1 do
  begin
    Line := '';
    for x:= 0 to PictureWith-1 do
    begin
      Position.SetIt(x, y);
      Line := Line + IfThen(Picture[Position], '#', ' ')
    end;
    OutPut.Add(Line);
    Writeln(Line);
  end;
  OutPut.SaveToFile(SaveFilePath);
  Result := Format('Solution saved at: %s', [SaveFilePath]);

  OutPut.Free;
  Picture.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay9'}
function TAdventOfCodeDay9.SolveA: Variant;
begin
  Result := TBasicIntComputer.RunProgram(FInput[0], 1); //2789104029
end;

function TAdventOfCodeDay9.SolveB: Variant;
begin
  Result := TBasicIntComputer.RunProgram(FInput[0], 2); //32869
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay10'}
procedure TAdventOfCodeDay10.BeforeSolve;
var Position: TPosition;
    i, j: Integer;
begin
  Map := TDictionary<TPosition, Boolean>.Create;
  for i := 0 to FInput.Count - 1 do
    for j := 1 to Length(FInput[i]) do
    begin
      Position.SetIt(j-1, i );
      Map.Add(Position, FInput[i][j] = '#')
    end;
end;

procedure TAdventOfCodeDay10.AfterSolve;
begin
  Map.Free;
end;

function TAdventOfCodeDay10.ScanAstroids(Const ScanLocation: TPosition; OutPutMap: TDictionary<Extended, TPosition>): Integer;
var Position, TempPosistion: TPosition;
    D, DeltaX, DeltaY, DistanceX, DistanceY: Integer;
begin
  result := 0;
  for Position in Map.Keys do
  begin
    if Position.Equals(ScanLocation) or not Map[Position] then
      Continue;

    DistanceX := ScanLocation.x - Position.x;
    DistanceY := ScanLocation.y - Position.y;
    DeltaX := 0;
    DeltaY := 0;

    if DistanceX = 0 then
      DeltaY := Sign(DistanceY)
    else if DistanceY = 0 then
      DeltaX := Sign(DistanceX)
    else
    begin
      D := GCD(DistanceX, DistanceY);
      DeltaX := Round(DistanceX / D);
      DeltaY := Round(DistanceY / D);
    end;

    TempPosistion.SetIt(Position.x, Position.y);
    TempPosistion.AddDelta(DeltaX, DeltaY);

    while Map.ContainsKey(TempPosistion) do
    begin
      if TempPosistion.Equals(ScanLocation) then
      begin
        Inc(Result);
        if Assigned(OutPutMap) then
          OutPutMap.Add(CalcAstroidAngle(ScanLocation, Position), Position);
        break;
      end;

      if Map[TempPosistion] then
        Break;

      TempPosistion.AddDelta(DeltaX, DeltaY);
    end;
  end;
end;

function TAdventOfCodeDay10.CalcAstroidAngle(Station, Astroid: TPosition): Extended;
begin
  Result := ArcTan2(Astroid.x-Station.x, Station.y-Astroid.y);
  if Result < 0 then
    Result := Result + 2*Pi;
end;

function TAdventOfCodeDay10.SolveA: Variant;
var Position: TPosition;
    Temp: Integer;
begin
  Result := 0;
  for Position in Map.Keys do
    if Map[Position] then //Must be build on an astroid
    begin
      Temp := ScanAstroids(Position, nil);
      Result := Max(Result, Temp); //303
      if Result = Temp then
        MonitoringStation := Position;
    end;
end;

function TAdventOfCodeDay10.SolveB: Variant;
var VisibleAstroids: TDictionary<Extended, TPosition>;
    AngleArray: TArray<Extended>;
    Posistion200: TPosition;
begin
  VisibleAstroids := TDictionary<Extended, TPosition>.Create;
  ScanAstroids(MonitoringStation, VisibleAstroids);
  AngleArray := VisibleAstroids.Keys.ToArray;
  TArray.Sort<Extended>(AngleArray);
  Posistion200 := VisibleAstroids[AngleArray[199]];
  Result := 100*Posistion200.x + Posistion200.y; //408
  VisibleAstroids.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay11'}
procedure TAdventOfCodeDay11.PaintPanels(PaintedPanels: TDictionary<TPosition, Integer>);
var CurPosistion: TPosition;
    Computer: TBasicIntComputer;
    Move, Instrcution: Integer;
    Direction: TDirection;
begin
  Computer := TBasicIntComputer.Create(FInput[0]);
  Computer.StopOnOutPut := True;
  Direction := Up;
  CurPosistion.SetIt(0,0);
  while not Computer.IsStopped do
  begin
    if not PaintedPanels.TryGetValue(CurPosistion, Instrcution) then
      Instrcution := 0; //Default black

     //Paint 
    Computer.LastOutput := Instrcution;
    PaintedPanels.AddOrSetValue(CurPosistion, Computer.Run);
    
    //Move
    case Computer.Run of
      0: Move := Ord(Direction) -1;  
      1: Move := Ord(Direction) +1
    else
      raise Exception.Create('Unknown move');
    end;

    if Move > 3 then Dec(Move, 4);
    if Move < 0 then Inc(Move, 4);

    Direction := TDirection(Move);
    case Direction of
      Up:    CurPosistion.AddDelta(0, 1);
      down:  CurPosistion.AddDelta(0, -1);
      Left:  CurPosistion.AddDelta(-1, 0);
      Right: CurPosistion.AddDelta(1, 0);
    end;    
  end;
  Computer.Free;
end;

function TAdventOfCodeDay11.SolveA: Variant;
var PaintedPanels: TDictionary<TPosition, integer>;
begin
  PaintedPanels := TDictionary<TPosition, integer>.Create;
  PaintPanels(PaintedPanels);
  Result := PaintedPanels.Count; //2469
  PaintedPanels.Free;
end;

function TAdventOfCodeDay11.SolveB: Variant;
var CurPosistion: TPosition;
    PaintedPanels: TDictionary<TPosition, integer>;
    MaxX, MaxY, MinX, MinY, X ,Y: Integer;
    Line: string;
    OutPut: TStringList;
begin
  PaintedPanels := TDictionary<TPosition, integer>.Create;

  CurPosistion.SetIt(0,0);
  PaintedPanels.Add(CurPosistion, 1);
  PaintPanels(PaintedPanels);

  MaxX := -MaxInt;
  MaxY := -MaxInt;
  MinX := MaxInt;
  MinY := MaxInt;
  for CurPosistion in PaintedPanels.Keys do
  begin
    MaxX := Max(MaxX, CurPosistion.X);
    MinX := Min(MinX, CurPosistion.X);
    MaxY := Max(MaxY, CurPosistion.Y);
    MinY := Min(MinY, CurPosistion.Y);
  end;

  OutPut := TStringList.Create;
  for Y := MaxY downto MinY do
  begin
    Line := '';
    for x := MinX to MaxX do
    begin
      CurPosistion.SetIt(x, y);
      if PaintedPanels.ContainsKey(CurPosistion) and (PaintedPanels[CurPosistion] = 1) then
        Line := Line + '#'
      else
        Line := Line + ' ' ;
    end;

    OutPut.Add(Line);
    Writeln(Line);
  end;

  OutPut.SaveToFile(SaveFilePath);
  Result := Format('Solution saved at: %s', [SaveFilePath]);
  OutPut.Free;
  PaintedPanels.Free;
end;
{$ENDREGION}
{$REGION 'TMoon'}
constructor TMoon.Create(const MoonCoordinates: String);
var Line: TStringList;
begin
  Line := TStringList.Create;
  Line.Delimiter := ',';
  Line.DelimitedText := Copy(MoonCoordinates, 2, Length(MoonCoordinates)-2);

  X := StrToInt(RightStr(Line[0], Length(Line[0])-2));
  Y := StrToInt(RightStr(Line[1], Length(Line[1])-2));
  Z := StrToInt(RightStr(Line[2], Length(Line[2])-2));
  VelocityX := 0;
  VelocityY := 0;
  VelocityZ := 0;
  Line.Free;
end;

procedure TMoon.CalcGravity(OtherMoon: TMoon);

  function _Calc(This, Other: integer): integer;
  begin
    if This = Other then
      Result := 0
    else if This > Other then
      Result := -1
    else
      Result := 1;
  end;

begin
  Inc(VelocityX, _Calc(X, OtherMoon.X));
  Inc(VelocityY, _Calc(Y, OtherMoon.Y));
  Inc(VelocityZ, _Calc(Z, OtherMoon.Z));
end;

procedure TMoon.ApplyVelocity;
begin
  Inc(X, VelocityX);
  Inc(Y, VelocityY);
  Inc(Z, VelocityZ)
end;

function TMoon.CalcEnergy: Integer;
begin
  Result := (Abs(X) + Abs(Y) + Abs(Z)) * (Abs(VelocityX) + Abs(VelocityY) + Abs(VelocityZ));
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay12'}
function TAdventOfCodeDay12.SolveA: Variant;
var Moons: TList<TMoon>;
    Moon, OtherMoon: TMoon;
    i: Integer;
begin
  Moons := TList<TMoon>.Create;
  for i := 0 to FInput.Count -1 do
    Moons.Add(TMoon.Create(FInput[i]));

  for i := 1 to 1000 do
  begin
    for Moon in Moons do
      for OtherMoon in Moons do
        Moon.CalcGravity(OtherMoon);

    for Moon in Moons do
      Moon.ApplyVelocity;
  end;

  Result := 0;
  for Moon in Moons do
  begin
    Inc(Result, Moon.CalcEnergy); //12490
    Moon.Free;
  end;

  Moons.Free;
end;

function TAdventOfCodeDay12.SolveB: Variant;
var Moons: TList<TMoon>;
    Moon, OtherMoon: TMoon;
    MoonStringX, MoonStringY, MoonStringZ: string;
    SeenDictionaryX, SeenDictionaryY, SeenDictionaryZ: TDictionary<string, Integer>;
    i, Counter: Integer;
    CycleTimeX, CycleTimeY, CycleTimeZ, Temp: Int64;

  procedure _DoCheck(Const MoonString: String; SeenDictionary: TDictionary<string, Integer>; var CycleTime: Int64);
  begin
    if CycleTime > 0 then
      Exit;

    if SeenDictionary.ContainsKey(MoonString) then
      CycleTime := Counter - SeenDictionary[MoonString]
    else
      SeenDictionary.Add(MoonString, Counter);
  end;

begin
  Counter := 0;
  CycleTimeX := -1;
  CycleTimeY := -1;
  CycleTimeZ := -1;
  SeenDictionaryX := TDictionary<string, Integer>.Create;
  SeenDictionaryY := TDictionary<string, Integer>.Create;
  SeenDictionaryZ := TDictionary<string, Integer>.Create;

  Moons := TList<TMoon>.Create;
  for i := 0 to FInput.Count -1 do
    Moons.Add(TMoon.Create(FInput[i]));

  while (CycleTimeX < 0) or (CycleTimeY < 0) or (CycleTimeZ < 0) do
  begin
    MoonStringX := '';
    MoonStringY := '';
    MoonStringZ := '';
    for Moon in Moons do
    begin
      MoonStringX := Format('%s%d#%d#)', [MoonStringX, Moon.X, Moon.VelocityX]);
      MoonStringY := Format('%s%d#%d#)', [MoonStringY, Moon.Y, Moon.VelocityY]);
      MoonStringZ := Format('%s%d#%d#)', [MoonStringZ, Moon.Z, Moon.VelocityZ]);

      for OtherMoon in Moons do
        Moon.CalcGravity(OtherMoon);
    end;

    _DoCheck(MoonStringX, SeenDictionaryX, CycleTimeX);
    _DoCheck(MoonStringY, SeenDictionaryY, CycleTimeY);
    _DoCheck(MoonStringZ, SeenDictionaryZ, CycleTimeZ);

    for Moon in Moons do
      Moon.ApplyVelocity;

    Inc(Counter);
  end;

  Temp := round((CycleTimeX*CycleTimeY)/GCD(CycleTimeX, CycleTimeY));
  Result := Round((Temp*CycleTimeZ)/GCD(Temp, CycleTimeZ)); //392733896255168

  for Moon in Moons do
    Moon.Free;

  Moons.Free;
  SeenDictionaryX.Free;
  SeenDictionaryY.Free;
  SeenDictionaryZ.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay13'}
function TAdventOfCodeDay13.SolveA: Variant;
var Computer: TBasicIntComputer;
begin
  Result := 0;
  Computer := TBasicIntComputer.Create(FInput[0]);
  Computer.StopOnOutPut := True;
  while Not Computer.IsStopped do
  begin
    Computer.Run; //Run untill X-output
    Computer.Run; //Run untill Y-Output
    if Computer.Run = 2 then //Block
      Inc(Result);
  end;

  Computer.Free;
end;

function TAdventOfCodeDay13.SolveB: Variant;
var Computer: TBasicIntComputer;
    X, Y, BallX, PaddleX: Integer;
begin
  BallX := 0;
  PaddleX := 0;
  Result := 0;

  Computer := TBasicIntComputer.Create(FInput[0]);
  Computer.StopOnOutPut := True;
  Computer.WriteMemory(0, 2);

  while Not Computer.IsStopped do
  begin
    Computer.LastOutput := Sign(BallX - PaddleX);

    X := Computer.Run;
    Y := Computer.Run;

    if (X = -1) and (y = 0) then
      Result := Computer.Run //13140
    else
    case Computer.Run of
      3: PaddleX := X; //Paddle
      4: BallX := X; //Ball
    end;
  end;
  Computer.Free;
end;
{$ENDREGION}
initialization
  RegisterClasses([TAdventOfCodeDay1, TAdventOfCodeDay2, TAdventOfCodeDay3, TAdventOfCodeDay4, TAdventOfCodeDay5,
    TAdventOfCodeDay6, TAdventOfCodeDay7, TAdventOfCodeDay8, TAdventOfCodeDay9, TAdventOfCodeDay10, TAdventOfCodeDay11,
    TAdventOfCodeDay12, TAdventOfCodeDay13

]);

end.

