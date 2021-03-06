unit AOCSolutions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Defaults, System.Generics.Collections,
  system.Diagnostics, AOCBase, RegularExpressions, System.DateUtils, system.StrUtils,
  system.Math, IntComputers, uAOCUtils;

type TBotMovement = (North = 1, South, West, East);

type TReaction = record
  NumberOfProducts: Int64;
  NeededIngredients: TDictionary<String, Int64>
end;

type TExploreData = record
  Position: TPosition;
  StepsTaken: Integer;
  Level: Integer;
  CollectedKeys: string;
  function SetPosition(Pos: TPosition): TExploreData;
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
    MonitoringStation: TPosition; //Needed for part b
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

type TAdventOfCodeDay14 = class(TAdventOfCode)
  private
    OldRoundMode: TRoundingMode;
    Reactions: TDictionary<String, TReaction>;
    function CalcOreAmountNeededForFuel(Const FuelAmmount: Int64): Int64;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
end;

type TAdventOfCodeDay15 = class(TAdventOfCode)
  private
    OxygenSystemPosistion: TPosition;
    PathTakenToOxygenSytem: String;
    procedure FindPath(const RouteTaken: string; const StartPosition: TPosition; Seen: TDictionary<TPosition, Integer>; Computer: TBasicIntComputer);
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
end;

type TAdventOfCodeDay16 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
end;

type TAdventOfCodeDay17 = class(TAdventOfCode)
  private
    Map: TList<TPosition>;
    VacuumRobotPosition: TPosition;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
end;

type TAdventOfCodeDay18 = class(TAdventOfCode)
  private
    function ExploreMap(Const Map: TDictionary<TPosition, char>; Const StartPosition: TPosition; Const KeysToCount: Integer): Integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
end;

type TAdventOfCodeDay19 = class(TAdventOfCode)
  private
    Fprogram: TDictionary<Integer, int64>;
    function IsTractorBeam(Const aX, aY: Integer): Boolean;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
end;

type TAdventOfCodeDay20 = class(TAdventOfCode)
  private
    Map: TDictionary<TPosition, Char>;
    Portals: TDictionary<TPosition, String>;
    MaxX, MaxY: Integer;
    StartPortal, FinalPortal: TPosition;
    function GetPortal(const PortalCode: string; Const CurrentPortal: TPosition): TPosition;
    function SolveDonutMaze(Const UseRecursiveSpaces: boolean): Integer;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
end;

type TAdventOfCodeDay21 = class(TAdventOfCode)
  private
    procedure RunProgram(aComputer: TBasicIntComputer);
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
end;

type
  TAdventOfCodeDay22 = class(TAdventOfCode)
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
  end;

type
  TAdventOfCodeDay23 = class(TAdventOfCode)
  private
    Fprogram: TDictionary<Integer, int64>;
    procedure HandleNoInputValue(var Input: Int64; Var Stop: Boolean);
    function BuildAndRunIntCluster(Const ReturnFirstPackage: Boolean): Int64;
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
  end;

type TAdventOfCodeDay24 = class(TAdventOfCode)
  private
    procedure GridNotify(Sender: TObject; const Item: TDictionary<TPosition, Boolean>; Action: TCollectionNotification);
  protected
    function SolveA: Variant; override;
    function SolveB: Variant; override;
end;

type
  TAdventOfCodeDay25 = class(TAdventOfCode)
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
    Amplifier: TBasicIntComputer;
begin
  Result := 0;
  PhaseSettings := GetPhaseSettings(0,4);

  for PhaseSetting in PhaseSettings do
  begin
    InputSignal := 0;
    for i := 0 to 4 do
    begin
      Amplifier := TBasicIntComputer.Create(Fprogram);
      Amplifier.QueueInput(StrToInt(PhaseSetting[i+1]));
      Amplifier.QueueInput(InputSignal);
      InputSignal := Amplifier.Run;
      Amplifier.Free;
    end;
    Result := Max(Result, InputSignal); //38500
  end;
  PhaseSettings.Free;
end;

function TAdventOfCodeDay7.SolveB: Variant;
var PhaseSettings: TList<String>;
    PhaseSetting: string;
    i, InputSignal: Integer;
    Amplifiers: TList<TBasicIntComputer>;
    Amplifier: TBasicIntComputer;
begin
  PhaseSettings := GetPhaseSettings(5,9);
  Amplifiers := TList<TBasicIntComputer>.Create;

  Result := 0;

  for PhaseSetting in PhaseSettings do
  begin
    for i := 0 to 4 do
    begin
      Amplifier := TBasicIntComputer.Create(Fprogram);
      Amplifier.StopOnOutPut := True;
      Amplifier.QueueInput(StrToInt(PhaseSetting[i+1]));
      Amplifiers.Add(Amplifier);
    end;

    InputSignal := 0;
    while not  Amplifiers[4].IsStopped do
    begin
      for i := 0 to 4 do
      begin
        Amplifiers[i].QueueInput(InputSignal);
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
begin
  Inc(VelocityX, Sign(OtherMoon.X - X));
  Inc(VelocityY, Sign(OtherMoon.Y - Y));
  Inc(VelocityZ, Sign(OtherMoon.Z - Z));
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
      Inc(Result); //273
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
{$Region 'TAdventOfCodeDay14'}
procedure TAdventOfCodeDay14.BeforeSolve;
var Line: TStringList;
    i, j: integer;
    Reaction: TReaction;
begin
  Reactions := TDictionary<String, TReaction>.Create;

  Line := TStringList.Create;
  Line.Delimiter := ',';
  for i := 0 to FInput.Count -1 do
  begin
    Line.DelimitedText := FInput[i];
    Reaction.NumberOfProducts := StrToInt64(Line[Line.Count - 2]);
    Reaction.NeededIngredients := TDictionary<String, Int64>.Create;
    for j := 0 to Trunc((Line.Count - 4)/2) do
      Reaction.NeededIngredients.Add(Line[j*2+1], StrToInt64(Line[j*2]));

    Reactions.Add(Line[Line.Count - 1], Reaction);
  end;

  Line.Free;
  OldRoundMode := GetRoundMode;
  SetRoundMode(rmUp);
end;

procedure TAdventOfCodeDay14.AfterSolve;
var Reaction: TReaction;
begin
  SetRoundMode(OldRoundMode);
  for Reaction in Reactions.Values do
      Reaction.NeededIngredients.Free;
  Reactions.Free;
end;

function TAdventOfCodeDay14.CalcOreAmountNeededForFuel(Const FuelAmmount: Int64): Int64;
var Storage: TDictionary<String, Int64>;

  procedure _CalcOreNeeded(Const ProductName: String; NumberOfUnitsToProduce: Int64; Var OreNeeded: Int64);
  var Reaction: TReaction;
      Pair: TPair<string, Int64>;
      Batches: Int64;
  begin
    if not Storage.ContainsKey(ProductName) then
      Storage.Add(ProductName, - NumberOfUnitsToProduce)
    else
      Storage[ProductName] := Storage[ProductName] - NumberOfUnitsToProduce;

    if Storage[ProductName] < 0 then
    begin
      Reaction := Reactions[ProductName];
      Batches := Round(-Storage[ProductName] / Reaction.NumberOfProducts);
      for Pair in Reaction.NeededIngredients do
      begin
        if Pair.Key <> 'ORE' then
          _CalcOreNeeded(Pair.Key, Batches*Pair.Value, OreNeeded)
        else
          Inc(OreNeeded, Batches*Pair.Value);
      end;
      Storage[ProductName] := Storage[ProductName] + Batches*Reaction.NumberOfProducts;
    end;
  end;

begin
  Result := 0;
  Storage := TDictionary<string, Int64>.Create;
  _CalcOreNeeded('FUEL', FuelAmmount, Result);
  Storage.Free;
end;

function TAdventOfCodeDay14.SolveA: Variant;
begin
  Result := CalcOreAmountNeededForFuel(1); //522031
end;

function TAdventOfCodeDay14.SolveB: Variant;
var SearchBase, Search: int64;
begin
  Search := 0;
  SearchBase := Round(Power(2, 18));
  Result := 0;
  while Result = 0 do
  begin
    Inc(Search, SearchBase);

    if CalcOreAmountNeededForFuel(Search) > 1000000000000 then
    begin
      if SearchBase = 1 then
        Result := Search - 1; //3566577

      Dec(Search, SearchBase); //Back to previous state
      SearchBase := Round(SearchBase / 2); //divide searchbase by 2
    end;
  end;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay15'}
procedure TAdventOfCodeDay15.FindPath(const RouteTaken: string; const StartPosition: TPosition; Seen: TDictionary<TPosition, Integer>; Computer: TBasicIntComputer);

  procedure _Run(Direction: TBotMovement; Route: String);
  var StatusCode, StepsTaken, DeltaX, DeltaY: Integer;
  begin
    Computer.LastOutput := Ord(Direction);
    StepsTaken := Seen[StartPosition];
    Route := Format('%s#%d', [Route, Ord(Direction)]);
    StatusCode := Computer.Run;

    if StatusCode = 0 then
      Exit; //Hit a wall, stop

    DeltaX := 0;
    DeltaY := 0;
    case Direction of
      North: DeltaY := 1;
      South: DeltaY := -1 ;
      East:  DeltaX := -1;
      West:  DeltaX := 1;
    end;
    StartPosition.AddDelta(DeltaX, DeltaY);

    if StatusCode = 2 then
    begin
      OxygenSystemPosistion := StartPosition;
      PathTakenToOxygenSytem := Route;
    end;

    if not (Seen.ContainsKey(StartPosition) and (Seen[StartPosition] <= StepsTaken +1)) then
    begin //If we visited this location for the first time, or we have a more effective route, find all the paths from this posistion
      Seen.AddOrSetValue(StartPosition, StepsTaken+1);
      FindPath(Route, StartPosition, Seen, Computer);
    end;

    //Take a step back
    StartPosition.AddDelta(-DeltaX, -DeltaY);
    if (Direction in [North, West]) then
      Direction := TBotMovement(Ord(Direction)+1)
    else
      Direction := TBotMovement(Ord(Direction)-1);

    Computer.LastOutput := Ord(Direction);
    Assert(Computer.Run <> 0);
  end;

begin
  _Run(North, RouteTaken);
  _Run(East, RouteTaken);
  _Run(South, RouteTaken);
  _Run(West, RouteTaken);
end;

function TAdventOfCodeDay15.SolveA: Variant;
var Position: TPosition;
    Computer: TBasicIntComputer;
    Seen: TDictionary<TPosition, Integer>;
begin
  Computer := TBasicIntComputer.Create(FInput[0]);
  Computer.StopOnOutPut := True;

  Seen := TDictionary<TPosition, Integer>.Create;
  Position.SetIt(0, 0);
  Seen.Add(Position, 0);

  FindPath('', Position, Seen, Computer);
  Result := Seen[OxygenSystemPosistion]; //300

  Computer.Free;
  Seen.Free;
end;

function TAdventOfCodeDay15.SolveB: Variant;
var Computer: TBasicIntComputer;
    Seen: TDictionary<TPosition, Integer>;
    Line: TStringList;
    i: Integer;
begin
  Computer := TBasicIntComputer.Create(FInput[0]);
  Computer.StopOnOutPut := True;

  //Move the robot to the oxygen system
  Line := TStringList.Create;
  Line.Delimiter := '#';
  Line.DelimitedText := PathTakenToOxygenSytem; //From part a
  for i := 1 to Line.Count -1 do
  begin
    Computer.LastOutput := StrToInt(Line[i]);
    Assert(Computer.Run <> 0);
  end;
  Assert(Computer.LastOutput = 2);
  Line.Free;

  //Find the distance to all locations
  Seen := TDictionary<TPosition, Integer>.Create;
  Seen.Add(OxygenSystemPosistion, 0); //From part a
  FindPath('', OxygenSystemPosistion, Seen, Computer);

  Result := 0;
  for i in Seen.Values do
    Result := Max(Result, i);

  Seen.Free;
  Computer.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay16'}
function TAdventOfCodeDay16.SolveA: Variant;

  function pattern(Const LineIndex, NumberIndex: int64): int64;
  begin
    case (NumberIndex mod (LineIndex*4)) div LineIndex of
      0: Result := 0;
      1: Result := 1;
      2: Result := 0;
      3: Result := -1;
    else
      raise Exception.Create('UnKnow partern');
    end;
  end;

var i, j, Temp, Counter: int64;
    Input, NewInput: TList<Integer>;
begin
  Input := TList<Integer>.Create;
  NewInput := TList<Integer>.Create;

  for i := 1 to Length(FInput[0]) do
    Input.Add(StrToInt(FInput[0][i]));

  Counter := 0;
  while Counter < 100 do
  begin
    Inc(Counter);

    NewInput.Clear;
    for i := 0 to Input.Count-1 do
    begin
      Temp := 0;
      for j := 0 to Input.Count - 1 do
        Inc(Temp, pattern(i+1, j+1)*Input[j]);

      NewInput.Add(Abs(Temp) mod 10);
    end;

    Input.Clear;
    Input.AddRange(NewInput);
  end;

  Result := '';
  for i := 0 to 7 do
    Result := Result + IntToStr(Input[i]); //42945143

  Input.Free;
  NewInput.Free;;
end;

function TAdventOfCodeDay16.SolveB: Variant;
var i, j, Counter, Offset, Temp: Int64;
    Input: TList<Int64>;
begin
  Input := TList<Int64>.Create;
  Offset := StrToInt(LeftStr(FInput[0], 7));

  for i := 1 to 100000 do
    for j := 1 to Length(FInput[0]) do
      Input.Add(StrToInt64(FInput[0][j]));

  Counter := 0;
  while Counter < 100 do
  begin
    Inc(Counter);
    Temp := 0;
    for i := Input.Count -1  downto Offset-1   do
    begin
      Inc(Temp, Input[i]);
      Input[i] := Temp mod 10;
    end;

    Writeln(Counter);
  end;

  Result := '';
  for i := Offset to Offset + 7 do
    Result := Result + IntToStr(Input[i]);  //99974970

  Input.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay17'}
procedure TAdventOfCodeDay17.BeforeSolve;
begin
  Map := TList<TPosition>.Create;
end;

procedure TAdventOfCodeDay17.AfterSolve;
begin
  Map.Free;
end;

function TAdventOfCodeDay17.SolveA: Variant;

  function _IsIntersection(aPosition: TPosition): Boolean;
  begin
    Result := Map.ConTains(aPosition.Clone.ApplyDirection(Up))
          and Map.ConTains(aPosition.Clone.ApplyDirection(Down))
          and Map.ConTains(aPosition.Clone.ApplyDirection(Left))
          and Map.ConTains(aPosition.Clone.ApplyDirection(Right))
  end;

var Position: TPosition;
    Computer: TBasicIntComputer;
    X, Y, OutPut: Integer;
begin
  Computer := TBasicIntComputer.Create(FInput[0]);
  X := -1;
  Y := 0;

  Computer.StopOnOutPut := True;
  while not Computer.IsStopped do
  begin
    Inc(X);
    Position.SetIt(X, Y);

    OutPut := Computer.Run;
    case OutPut of
      35: Map.Add(Position);
      46: ; //
      10: begin
            X := -1;
            Inc(Y)
          end;
      94: begin
            VacuumRobotPosition := Position;
            Map.Add(Position);
          end
    else
      raise Exception.CreateFmt('Unknown output %d', [Output]);
    end;
  end;

  Result := 0;
  for Position in Map do
    if _IsIntersection(Position) then
      Inc(Result, Position.x * Position.Y);

  Computer.Free;
end;

function TAdventOfCodeDay17.SolveB: Variant;
var Position, RobotPosistion: TPosition;
    Computer: TBasicIntComputer;
    StepsTaken, Move: Integer;
    Instruction: string;
    Direction, TempDir: TDirection;

  function _IsIntersection(aPosition: TPosition): Boolean;
  var Count: integer ;
  begin
      Count := 0;
      if Map.Contains(aPosition.Clone.ApplyDirection(Up)) then inc(Count);
      if Map.Contains(aPosition.Clone.ApplyDirection(Down)) then inc(Count);
      if Map.Contains(aPosition.Clone.ApplyDirection(Left)) then inc(Count);
      if Map.Contains(aPosition.Clone.ApplyDirection(Right)) then inc(Count);
      Result := Count = 3; //One of the 4 legs is already removed, so when there are 3 left its a Intersection
  end;

Var FunctionA, FunctionB, FunctionC, MainProgram: String;
begin
  Instruction := '';
  Direction := Up;
  StepsTaken := 0;
  RobotPosistion := VacuumRobotPosition;
  while Map.Count > 1 do //Final position isn't removed
  begin
    Position := RobotPosistion.Clone.ApplyDirection(Direction);

    if Map.Contains(Position) then
    begin
      if not _IsIntersection(RobotPosistion) then
        Map.Remove(RobotPosistion); //Dont remove intersections, otherwise the bot gets stuck

      RobotPosistion := Position;
      Inc(StepsTaken);
    end
    else
    begin
      if StepsTaken > 0 then //Add stepstaken to instruction
        Instruction := Instruction + IntToStr(StepsTaken) + ',';
      StepsTaken := 0;

      Move := Ord(Direction) -1; //Left
      if Move < 0 then Inc(Move, 4);
        TempDir := TDirection(Move);

      Position := RobotPosistion.Clone.ApplyDirection(TempDir);
      if Map.Contains(Position) then //Try to go left
      begin
        Direction := TempDir;
        Instruction := Instruction + 'L,'
      end
      else //Can't go left, so we should go right
      begin
        Move := Ord(TempDir) -2;
        if Move < 0 then Inc(Move, 4);
          Direction := TDirection(Move);

        Instruction := Instruction + 'R,'
      end;
    end;
  end;
  Instruction := Instruction + IntToStr(StepsTaken); //Add final counter

  Writeln(Instruction);
  MainProgram := 'A,B,A,C,B,C,B,C,A,C';
  FunctionA := 'R,12,L,6,R,12';
  FunctionB := 'L,8,L,6,L,10';
  FunctionC := 'R,12,L,10,L,6,R,10';

  Computer := TBasicIntComputer.Create(FInput[0]);
  Computer.WriteMemory(0, 2);

  Computer.QueueASCIICode(MainProgram);
  Computer.QueueASCIICode(FunctionA);
  Computer.QueueASCIICode(FunctionB);
  Computer.QueueASCIICode(FunctionC);
  Computer.QueueASCIICode('n');

  Result  := Computer.Run;
  Computer.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay18'}
function TExploreData.SetPosition(Pos: TPosition): TExploreData;
begin
  Position := pos;
  Result := Self;
end;

function TAdventOfCodeDay18.ExploreMap(Const Map: TDictionary<TPosition, char>; Const StartPosition: TPosition; Const KeysToCount: Integer): Integer;

  function _StateKey(Const aPosition: TPosition; Const Keys: String): String;
  var Line: TStringList;
      i: Integer;
  begin
    Line := TStringList.Create;

    for i := 1 to Length(Keys) do
      Line.Add(Keys[i]);

    Line.Sort;
    Result := '';
    for i := 0 to Line.Count - 1 do
      Result := Result + Line[i];

    Result := Result + IntToStr(1000*aPosition.x+aPosition.y);
    Line.Free;
  end;

var Position: TPosition;
    TempChar: Char;
    ExplorationQueue: TQueue<TExploreData>;
    Explore: TExploreData;
    StateKey: String;
    States: TDictionary<String, Integer>;
begin
  Explore.Position := StartPosition;
  Explore.CollectedKeys := '';
  Explore.StepsTaken := 0;

  ExplorationQueue := TQueue<TExploreData>.Create;
  States := TDictionary<String, Integer>.Create;
  ExplorationQueue.Enqueue(Explore);

  Result := MaxInt;
  while ExplorationQueue.Count > 0 do
  begin
    Explore := ExplorationQueue.Dequeue;

    Position := Explore.Position;
    if not Map.TryGetValue(Position, TempChar) then
      Continue; //Position does not exist

    StateKey := _StateKey(Position, Explore.CollectedKeys);
    if States.ContainsKey(StateKey) then
      Continue; //This state is already seen
    States.Add(StateKey, 1);

    if (TempChar <> '.') then // its a key or a door
    begin
      if (TempChar = Lowercase(TempChar)) then //Its a key
      begin
        if not ContainsText(Explore.CollectedKeys, TempChar) then //Its a new key, add to collection
          Explore.CollectedKeys := Explore.CollectedKeys + TempChar;
        if Length(Explore.CollectedKeys) = KeysToCount then //We have all keys!
          Result := Min(Result, Explore.StepsTaken);
      end
      else //It must be a door
        if not ContainsText(Explore.CollectedKeys, TempChar) then
          Continue; //we dont have the key
    end;

    Explore.StepsTaken := Explore.StepsTaken + 1;
    ExplorationQueue.Enqueue(Explore.SetPosition(Position.Clone.ApplyDirection(Up)));
    ExplorationQueue.Enqueue(Explore.SetPosition(Position.Clone.ApplyDirection(Down)));
    ExplorationQueue.Enqueue(Explore.SetPosition(Position.Clone.ApplyDirection(Left)));
    ExplorationQueue.Enqueue(Explore.SetPosition(Position.Clone.ApplyDirection(Right)));
  end;

  ExplorationQueue.Free;
  States.Free;
end;

function TAdventOfCodeDay18.SolveA: Variant;
var Map: TDictionary<TPosition, Char>;
    x, y, KeyCount: Integer;
    Position, StartPosition: TPosition;
    TempChar: Char;
begin
  KeyCount := 0;
  Map := TDictionary<TPosition, Char>.Create;
  for y := 0 to FInput.Count-1 do
    for x := 1 to Length(FInput[y]) do
    begin
      Position.SetIt(x-1, y);
      TempChar := FInput[y][x];

      if TempChar = '#' then  //Dont add walls
        Continue;

      if TempChar = '@' then
      begin
        StartPosition := Position;
        TempChar := '.'
      end;

      if (TempChar = LowerCase(TempChar)) AND (TempChar <> '.') then
        Inc(KeyCount);

      Map.Add(Position, TempChar);
    end;

  Result := ExploreMap(Map, StartPosition, KeyCount); //1660
  Map.Free;
end;

function TAdventOfCodeDay18.SolveB: Variant;
var Map: TDictionary<TPosition, char>;

  function CountKeys(Const StartPosition: TPosition): Integer;
  var Seen: TList<TPosition>;

    procedure _Run(var Counter: Integer; Const Position: TPosition);
    var ch: char;
    begin
      if not Map.TryGetValue(Position, ch) or Seen.Contains(Position) then
        Exit;

      Seen.Add(Position);

      if ch <> '.' then
        Inc(Counter);

      _Run(Counter, Position.Clone.ApplyDirection(Up));
      _Run(Counter, Position.Clone.ApplyDirection(Down));
      _Run(Counter, Position.Clone.ApplyDirection(Left));
      _Run(Counter, Position.Clone.ApplyDirection(Right));
    end;

  begin
    Seen := TList<TPosition>.Create;
    Result := 0;
    _Run(Result, StartPosition);
    Seen.Free;
  end;

var x, y: Integer;
    Position, StartPosition: TPosition;
    TempChar: Char;
begin
  Map := TDictionary<TPosition, Char>.Create;
  for y := 0 to FInput.Count-1 do
    for x := 1 to Length(FInput[y]) do
    begin
      Position.SetIt(x-1, y);
      TempChar := FInput[y][x];

      if TempChar = '#' then //Dont add wall's to the map
        Continue;

      if TempChar = '@' then
      begin
        StartPosition := Position;
        TempChar := '.'
      end;

      if TempChar <> LowerCase(TempChar) then  //Dont add door's to the map, assume we can reach all keys always
        TempChar := '.';

      Map.Add(Position, TempChar);
    end;

  //Adjust map
  Map.Remove(StartPosition);
  Map.Remove(StartPosition.Clone.ApplyDirection(Up));
  Map.Remove(StartPosition.Clone.ApplyDirection(Down));
  Map.Remove(StartPosition.Clone.ApplyDirection(Left));
  Map.Remove(StartPosition.Clone.ApplyDirection(Right));

  Result := ExploreMap(Map, StartPosition.Clone.AddDelta(1, 1), CountKeys(StartPosition.Clone.AddDelta(1, 1)))
          + ExploreMap(Map, startPosition.Clone.AddDelta(-1, -1), CountKeys(StartPosition.Clone.AddDelta(-1, -1)))
          + ExploreMap(Map, StartPosition.Clone.AddDelta(1, -1), CountKeys(StartPosition.Clone.AddDelta(1, -1)))
          + ExploreMap(Map, StartPosition.Clone.AddDelta(-1, 1), CountKeys(StartPosition.Clone.AddDelta(-1, 1)));
  Map.Free; //1660
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay19'}
procedure TAdventOfCodeDay19.BeforeSolve;
begin
  Fprogram := TBasicIntComputer.ParseIntput(FInput[0]);
end;

procedure TAdventOfCodeDay19.AfterSolve;
begin
  Fprogram.Free;
end;

function TAdventOfCodeDay19.IsTractorBeam(Const aX, aY: Integer): Boolean;
var Computer: TBasicIntComputer;
begin
  Computer := TBasicIntComputer.Create(Fprogram);
  Computer.StopOnOutPut := True;
  Computer.QueueInput(aX);
  Computer.QueueInput(aY);
  Result := Computer.Run = 1;
  Computer.Free;
end;

function TAdventOfCodeDay19.SolveA: Variant;
var x, y: Integer;
begin
  result := 0;
  for x := 0 to 49 do
    for y := 0 to 49 do
      if IsTractorBeam(x, y) then
        Inc(Result); //162
end;

function TAdventOfCodeDay19.SolveB: Variant;
var x, y: Integer;
begin
  Result := 0;
  while True do
  begin
    for x := 1300 to 1500 do
    begin
      y := 0;
      while not IsTractorBeam(x, y) do
        Inc(y);

      while IsTractorBeam(x, y+99) do
      begin
        if IsTractorBeam(x,y) and IsTractorBeam(x, y+99) and IsTractorBeam(x+99, y) and IsTractorBeam(x+99, y+99) then
          Exit(10000*x + y); //13021056

        Inc(y);
      end;
    end;
  end;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay20'}
procedure TAdventOfCodeDay20.BeforeSolve;

  procedure _BuidPortalList;
  var TempPosition: TPosition;
      Char1, Char2: Char;
  begin
    for TempPosition in Map.Keys do
    begin
      Char1 := Map[TempPosition];

      if Char1 = '.' then
        Continue;

      if Map.TryGetValue(TempPosition.Clone.AddDelta(0, 1), Char2) then //Search down
        if Char2 <> '.' then //its the other char of the portal code
          if Map.ContainsKey(TempPosition.Clone.AddDelta(0, 2)) then
            Portals.Add(TempPosition.Clone.AddDelta(0, 2), Char1+Char2)
          else
            Portals.Add(TempPosition.Clone.AddDelta(0, -1), Char1+Char2);

      if Map.TryGetValue(TempPosition.Clone.AddDelta(1, 0), Char2) then  //Search left
        if Char2 <> '.' then
          if Map.ContainsKey(TempPosition.Clone.AddDelta(2, 0)) then
            Portals.Add(TempPosition.Clone.AddDelta(2, 0), Char1+Char2)
          else
            Portals.Add(TempPosition.Clone.AddDelta(-1, 0), Char1+Char2);
    end;
  end;

var x, y: Integer;
    Position: TPosition;
    TempChar: Char;
begin
  Map := TDictionary<TPosition, Char>.Create;
  for y := 0 to FInput.Count-1 do
    for x := 1 to Length(FInput[y]) do
    begin
      Position.SetIt(x-1, y);
      TempChar := FInput[y][x];

      if (TempChar = '#') or (TempChar = ' ') then  //Don't add walls or empty spaces
        Continue;

      Map.Add(Position, TempChar);
    end;

  Portals := TDictionary<TPosition, String>.Create;
  _BuidPortalList;

  Position.SetIt(-1, -1); //Dummy
  StartPortal := GetPortal('AA', Position);
  FinalPortal := GetPortal('ZZ', Position);

  Portals.Remove(StartPortal);
  Portals.Remove(FinalPortal);

  MaxY := FInput.Count-1;
  MaxX := Length(FInput[0]);
end;

procedure TAdventOfCodeDay20.AfterSolve;
begin
  Map.Free;
  Portals.Free;
end;

function TAdventOfCodeDay20.GetPortal(const PortalCode: string; Const CurrentPortal: TPosition): TPosition;
var TempPosition: TPosition;
begin
  for TempPosition in Portals.Keys do
  begin
    if Portals[TempPosition] = PortalCode then
      if not TempPosition.Equals(CurrentPortal) then
      begin
        Result := TempPosition;
        Exit;
      end;
  end;
end;

function TAdventOfCodeDay20.SolveDonutMaze(Const UseRecursiveSpaces: boolean): Integer;
var ExplorationQueue: TQueue<TExploreData>;
    SeenStates: TDictionary<String, Boolean>;
    Explore: TExploreData;
    TempPosition: TPosition;
    StateKey, PortalKey: string;
    IsOuterWall: Boolean;
begin
  Result := MaxInt;
  ExplorationQueue := TQueue<TExploreData>.Create;
  SeenStates := TDictionary<String, Boolean>.Create;
  try
    Explore.Position := StartPortal;
    Explore.Level := 0;
    Explore.StepsTaken := 0;
    ExplorationQueue.Enqueue(Explore);

    while ExplorationQueue.Count > 0 do
    begin
      Explore := ExplorationQueue.Dequeue;

      StateKey := Format('%d#%d#%d', [Explore.Position.X, Explore.Position.Y, Explore.Level]);
      if SeenStates.ContainsKey(StateKey) OR Not Map.ContainsKey(Explore.Position) then //State already seen or not on map
        Continue;

      SeenStates.Add(StateKey, True);

      if Explore.Position.Equals(FinalPortal) and (not UseRecursiveSpaces or (Explore.Level = 0)) then
        Exit(Explore.StepsTaken);//Found solution

      if Portals.TryGetValue(Explore.Position, PortalKey)  then
      begin
        IsOuterWall := ((Explore.Position.x < 4) or (Explore.Position.x >  MaxX -4)) or
                       ((Explore.Position.y < 4) or (Explore.Position.y >  MaxY -4));

        if ((Explore.Level = 0) and UseRecursiveSpaces) and IsOuterWall then
//        Do nothing
        else
        begin
          Explore.Position := GetPortal(PortalKey, Explore.Position);
          Inc(Explore.StepsTaken);
          if IsOuterWall then
            Dec(Explore.Level)
          else
            Inc(Explore.Level);
        end;
      end;

      Inc(Explore.StepsTaken);
      TempPosition := Explore.Position;
      ExplorationQueue.Enqueue(Explore.SetPosition(TempPosition.Clone.ApplyDirection(Up)));
      ExplorationQueue.Enqueue(Explore.SetPosition(TempPosition.Clone.ApplyDirection(Down)));
      ExplorationQueue.Enqueue(Explore.SetPosition(TempPosition.Clone.ApplyDirection(Left)));
      ExplorationQueue.Enqueue(Explore.SetPosition(TempPosition.Clone.ApplyDirection(Right)));
    end;
  finally
    SeenStates.Free;
    ExplorationQueue.Free;
  end;
end;

function TAdventOfCodeDay20.SolveA: Variant;
begin
 Result := SolveDonutMaze(False); //620
end;

function TAdventOfCodeDay20.SolveB: Variant;
begin
  Result := SolveDonutMaze(True); //7366
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay21'}
procedure TAdventOfCodeDay21.RunProgram(aComputer: TBasicIntComputer);
const Debug: Boolean = True; //Set to true to visualize the output of the computer
var OutPut: Integer;
    Line: string;
begin
  if Not Debug then
    aComputer.Run; //Just run the program;

  aComputer.StopOnOutPut := True;
  while not aComputer.IsStopped do
  begin
    Output := aComputer.Run;
    if Output = 10 then //NewLine
    begin
      WriteLn(Line);
      Line := '';
    end
    else
      Line := Line + char(Output);
  end;
end;

function TAdventOfCodeDay21.SolveA: Variant;
var Computer: TBasicIntComputer;
begin
  Computer := TBasicIntComputer.Create(FInput[0]);

  Computer.QueueASCIICode('OR A J'); // Set Jump to True is A is a groud
  Computer.QueueASCIICode('AND B J'); //Set Jump to True is B is a groud
  Computer.QueueASCIICode('AND C J'); //Set Jump to True is C is a groud
  Computer.QueueASCIICode('NOT J J'); //Inverse jump, command, if its false it means there is a hole ahead
  Computer.QueueASCIICode('AND D J'); //Check if the landingzone is safe, if not, don't jump
  Computer.QueueASCIICode('WALK');    //And walk

  RunProgram(Computer);
  Result := Computer.LastOutput; //19347868
  Computer.Free;
end;

function TAdventOfCodeDay21.SolveB: Variant;
var Computer: TBasicIntComputer;
begin
  Computer := TBasicIntComputer.Create(FInput[0]);

  Computer.QueueASCIICode('OR A J');  //Set Jump to True is A is a groud
  Computer.QueueASCIICode('AND B J'); //Set Jump to True is B is a groud
  Computer.QueueASCIICode('AND C J'); //Set Jump to True is C is a groud
  Computer.QueueASCIICode('NOT J J'); //Inverse jump, command, if its false it means there is a hole ahead
  Computer.QueueASCIICode('AND D J'); //Check if the landingzone is safe, if not, don't jump
  Computer.QueueASCIICode('OR E T');  //Check if there is ground one spot ahead of the landingzone, since we cant jump in the landingzone of a previous jump
  Computer.QueueASCIICode('OR H T');  //check if we can jump from position E and land safe
  Computer.QueueASCIICode('AND T J');  //Combine both cases
  Computer.QueueASCIICode('RUN'); //And run

  RunProgram(Computer);
  Result := Computer.LastOutput; //1142479667
  Computer.Free;
end;
{$ENDREGION}
{$Region 'TAdventOfCodeDay22'}
function TAdventOfCodeDay22.SolveA: Variant;
Const StackSize: Integer = 10007;
var CardsToCut: Integer;
    Line: TStringList;
    CurrentLine: String;
begin
  Line := TStringList.Create;
  Line.Delimiter := ' ';
  Result := 2019; //We only need to keep track of where card 2019 is ending up, so only calculate the position of this card

  for CurrentLine in FInput do
  begin
    Line.DelimitedText := CurrentLine;

    if CurrentLine.StartsWith('deal into new stack') then
      Result := StackSize -1 - Result
    else if CurrentLine.StartsWith('deal with increment') then
      Result := StrToInt(Line[3])*Result mod StackSize
    else if CurrentLine.StartsWith('cut') then
    begin
      CardsToCut := StrToInt(Line[1]);

      if Result <= CardsToCut -1 then
        Result := StackSize - CardsToCut + Result
      else
        Result := Result - CardsToCut;
    end
  end;
  Line.Free; //6526
end;

function TAdventOfCodeDay22.SolveB: Variant;
Const StackSize: int64 = 119315717514047;
      ShuffleRounds: Int64 = 101741582076661;
      TargetCardIndex: int64 = 2020;

  //https://stackoverflow.com/questions/12168348/ways-to-do-modulo-multiplication-with-primitive-types
  function RussianPeasantMultiplication(ValueA, ValueB, aMod: Int64): int64;
  begin
    Result := 0;
    while ValueA <> 0 do
    begin
      if Odd(ValueA) then
        result := (result + ValueB) mod aMod;
      ValueA := ValueA shr 1;
      ValueB := (ValueB shl 1) mod aMod;
    end;
  end;

  //https://rosettacode.org/wiki/Modular_inverse
  function modInv(e, t : int64) : int64;
  var
    d : int64;
    bal, count, step : int64;
  begin
    d := 0;
    if e < t then
      begin
        count := 1;
        bal := e;
        repeat
          step := ((t-bal) DIV e)+1;
          bal := bal + step * e;
          count := count + step;
          bal := bal - t;
        until bal = 1;
        d := count;
      end;
    modInv := d;
  end;

  function Modular(aBase, aExp, aMod: Int64): int64;
  var i, power: Int64;
  begin
    Result := 1;
    power := aBase mod aMod;
    for i := 0 to SizeOf(int64)*8-1 do
    begin
      if Odd(aExp shr i) then
        Result := RussianPeasantMultiplication(Result, power, aMod);
      power := RussianPeasantMultiplication(power, Power, aMod);
    end;
  end;

  procedure _Simplify(Var aValue: Int64);
  begin
    aValue := aValue mod StackSize;
    if aValue < 0 then
      Inc(aValue, StackSize);
  end;

var i: Integer;
    Line: TStringList;
    One, Two, Three, four, temp, a, b: Int64;
begin
  Line := TStringList.Create;
  Line.Delimiter := ' ';

  a := 1;
  b := 0;
  //Each itteration can be reversed with the formula (ax + b) mod m, first find a and b to undo one shuffle
  for i := FInput.Count-1 downto 0 do
  begin
    Line.DelimitedText := FInput[i];

    if FInput[i].StartsWith('deal into new stack') then
    begin
      b := -(b + 1);
      a := -a;;
    end
    else if FInput[i].StartsWith('cut') then //cut N cards
      Inc(b, StrToInt64(Line[1]))
    else if FInput[i].StartsWith('deal with increment')then //deal with increment N
    begin //https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
      temp := modInv(StrToInt64(Line[3]), StackSize);
      a := RussianPeasantMultiplication(a, temp, stacksize);
      b := RussianPeasantMultiplication(b, temp, stacksize);
    end;

    _Simplify(a);
    _Simplify(b);
  end;
  Line.Free;

  //https://en.wikipedia.org/wiki/Geometric_series
  one := RussianPeasantMultiplication(Modular(a, ShuffleRounds, StackSize), TargetCardIndex, StackSize);
  two := (Modular(a, ShuffleRounds, StackSize) + StackSize -1) mod StackSize;
  Three := RussianPeasantMultiplication(b, Two, StackSize);
  Four := Modular(a-1, StackSize-2, StackSize);
  Result := (One + RussianPeasantMultiplication(Three, four, StackSize)) mod stacksize; //79855812422607
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay23'}
procedure TAdventOfCodeDay23.HandleNoInputValue(var Input: Int64; Var Stop: Boolean);
begin
  Input := -1;
  Stop := True;
end;

function TAdventOfCodeDay23.BuildAndRunIntCluster(Const ReturnFirstPackage: Boolean): Int64;
var IntCluster: TList<TBasicIntComputer>;
    Computer: TBasicIntComputer;
    x, y, DestinationAddress, NatX, NatY: int64;
    IdleComputers, i: Integer;
    SeenY: TList<Int64>;
begin
  IntCluster := TList<TBasicIntComputer>.Create;
  SeenY := TList<Int64>.Create;

  for i := 0 to 49 do
  begin
    Computer := TBasicIntComputer.Create(FProgram);
    Computer.StopOnOutPut := True;
    Computer.QueueInput(i);
    Computer.OnNoInputValue := HandleNoInputValue;
    Computer.Run;
    IntCluster.Add(Computer);
  end;

  try
    NatX := 0;
    NatY := 0;

    while true do
    begin
      IdleComputers := 0;

      for Computer in IntCluster do
      begin
        Computer.LastOutput := -1;
        DestinationAddress := Computer.Run;

        if DestinationAddress < 0 then
        begin
          Inc(IdleComputers);
          Continue;
        end;

        x := Computer.Run;
        y := Computer.Run;

        if DestinationAddress = 255 then
        begin
          NatX := x;
          NatY := y;
          if ReturnFirstPackage then
            Exit(NatY);
        end
        else
        begin
          IntCluster[DestinationAddress].QueueInput(X);
          IntCluster[DestinationAddress].QueueInput(y);
        end;
      end;

      if IdleComputers = IntCluster.Count then
      begin
        if SeenY.Contains(NatY) then
          Exit(NatY);

        SeenY.Add(NatY);
        IntCluster[0].QueueInput(NatX);
        IntCluster[0].QueueInput(NatY);
        NatX := 0;
        NatY := 0;
      end;
    end;
  finally
    for Computer in IntCluster do
      Computer.Free;

    IntCluster.Free;
    SeenY.Free;
  end;
end;

procedure TAdventOfCodeDay23.BeforeSolve;
begin
  Fprogram := TBasicIntComputer.ParseIntput(FInput[0]);
end;

procedure TAdventOfCodeDay23.Aftersolve;
begin
  Fprogram.Free;
end;

function TAdventOfCodeDay23.SolveA: Variant;
begin
  Result := BuildAndRunIntCluster(True); //20367
end;

function TAdventOfCodeDay23.SolveB: Variant;
begin
  Result := BuildAndRunIntCluster(False); //15080
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay24'}
function TAdventOfCodeDay24.SolveA: Variant;
var Grid: TDictionary<TPosition, Boolean>;

  function _CountBugs(aPosition: TPosition): Integer;
  var Temp: Boolean;
  begin
    Result := 0;
    if Grid.TryGetValue(aPosition.Clone.ApplyDirection(Up), Temp) and Temp then inc(Result);
    if Grid.TryGetValue(aPosition.Clone.ApplyDirection(Down), Temp) and Temp then inc(Result);
    if Grid.TryGetValue(aPosition.Clone.ApplyDirection(Left), Temp) and Temp then inc(Result);
    if Grid.TryGetValue(aPosition.Clone.ApplyDirection(Right), Temp) and Temp then inc(Result);
  end;

var NewGrid: TDictionary<TPosition, Boolean>;
    Position: TPosition;
    x, y, BugCount, BioDiversityRating: Integer;
    Infested: Boolean;
    SeenBioDiversityRatings: TList<Integer>;
begin
  Grid := TDictionary<TPosition, Boolean>.Create;
  SeenBioDiversityRatings := TList<Integer>.Create;

  try
    for y := 0 to FInput.Count -1 do
      for x := 1 to Length(FInput[y]) do
        Grid.Add(Position.SetIt(x-1, y), FInput[y][x] = '#');

    while true do
    begin
      BioDiversityRating := 0;
      NewGrid := TDictionary<TPosition, Boolean>.Create;
      for Position in Grid.Keys do
      begin
        BugCount := _CountBugs(Position);
        Infested := Grid[Position];

        if Infested then
          BioDiversityRating := BioDiversityRating + Round(Power(2, Position.y*5 + Position.x));

        if Infested and (BugCount <> 1) then
          Infested := False
        else if (Not Infested) and (BugCount in [1,2]) then
          Infested := True;

        NewGrid.Add(Position, Infested);
      end;

      Grid.Free;
      Grid := TDictionary<TPosition, Boolean>.Create(NewGrid);
      NewGrid.Free;

      if SeenBioDiversityRatings.Contains(BioDiversityRating) then
        Exit(BioDiversityRating); //32511025
      SeenBioDiversityRatings.Add(BioDiversityRating);
    end;
  finally
    Grid.Free;
    SeenBioDiversityRatings.Free;
  end;
end;

function TAdventOfCodeDay24.SolveB: Variant;
var Grids: TAOCDictionary<Integer, TDictionary<TPosition, Boolean>>;
    EmptyGrid: TDictionary<TPosition, Boolean>;

  function _CountBugs(aPosition: TPosition; alevel: integer): Integer;
  var Temp: Boolean;
      RemoteMap: TDictionary<TPosition, Boolean>;
      TempPos: TPosition;

    function _CountRemoteRow(const aStartX, aDeltaX, aStartY, aDeltaY: integer): integer;
    var i: Integer;
        pos: TPosition;
    begin
      Result := 0;
      for i := 0 to 4 do
        if RemoteMap[pos.SetIt(aStartX, aStartY).AddDelta(aDeltaX*i, aDeltaY*i)] then
          Inc(Result);
    end;

  begin
      Result := 0;

      //This Grid
      if Grids[aLevel].TryGetValue(aPosition.Clone.ApplyDirection(Up), Temp) and Temp then inc(Result);
      if Grids[aLevel].TryGetValue(aPosition.Clone.ApplyDirection(Down), Temp) and Temp then inc(Result);
      if Grids[aLevel].TryGetValue(aPosition.Clone.ApplyDirection(Left), Temp) and Temp then inc(Result);
      if Grids[aLevel].TryGetValue(aPosition.Clone.ApplyDirection(Right), Temp) and Temp then inc(Result);

      //Level down
      if not Grids.TryGetValue(alevel - 1, RemoteMap) then
        RemoteMap := EmptyGrid;

      if (aPosition.x = 0) and RemoteMap[TempPos.SetIt(2,1)] then Inc(Result);
      if (aPosition.x = 4) and RemoteMap[TempPos.SetIt(2,3)] then Inc(Result);
      if (aPosition.y = 0) and RemoteMap[TempPos.SetIt(1,2)] then Inc(Result);
      if (aPosition.y = 4) and RemoteMap[TempPos.SetIt(3,2)] Then Inc(Result);

      //Level up;
      if not Grids.TryGetValue(alevel + 1, RemoteMap) then
        RemoteMap := EmptyGrid;

      if (aPosition.x = 2) and (aPosition.y=1) then //Case 8
        Inc(Result, _CountRemoteRow(0, 0, 0, 1));

      if (aPosition.x = 2) and (aPosition.y=3) then //Case 18
        Inc(Result, _CountRemoteRow(4, 0, 0, 1));

      if (aPosition.x = 1) and (aPosition.y=2) then //Case 12
        Inc(Result, _CountRemoteRow(0, 1, 0, 0));

      if (aPosition.x = 3) and (aPosition.y=2) then //Case 14
        Inc(Result, _CountRemoteRow(0, 1, 4, 0));
  end;

var NewGrids: TAOCDictionary<Integer, TDictionary<TPosition, Boolean>>;
    Grid, NewGrid: TDictionary<TPosition, Boolean>;
    Position: TPosition;
    x, y, Level, BugCount :Integer;
    Infested: Boolean;
begin
  Grids := TAOCDictionary<Integer, TDictionary<TPosition, Boolean>>.Create(GridNotify);

  //Load initial grid
  Grid := TDictionary<TPosition, Boolean>.Create;
  EmptyGrid := TDictionary<TPosition, Boolean>.Create;
  for y := 0 to 4 do
    for x := 0 to 4 do
    begin
      if (x=2) and (y=2)  then
        Continue;

      Position.SetIt(x, y);
      Grid.Add(Position, FInput[y][x+1] = '#');
      EmptyGrid.Add(Position, False);
    end;
  Grids.Add(0, Grid);

  //Add extra grids
  for Level := 1 to 100 do
  begin
    Grids.Add(Level, TDictionary<TPosition, Boolean>.Create(EmptyGrid));
    Grids.Add(-Level, TDictionary<TPosition, Boolean>.Create(EmptyGrid));
  end;

  for x := 0 to 199 do
  begin
    NewGrids := TAOCDictionary<Integer, TDictionary<TPosition, Boolean>>.Create(GridNotify);
    for Level in Grids.keys do
    begin
      NewGrid := TDictionary<TPosition, Boolean>.Create;

      for Position in Grids[Level].Keys do
      begin
        BugCount := _CountBugs(Position, Level);
        Infested := Grids[Level][Position];

        if Infested and (BugCount <> 1) then
          Infested := False
        else if (Not Infested) and (BugCount in [1,2]) then
          Infested := True;
        NewGrid.Add(Position, Infested);
      end;

      NewGrids.Add(Level, NewGrid); //Adding the newgrid to the dictionary ensures its cleand up once the Newgrids is freeed
    end;

    Grids.Free;
    Grids := TAOCDictionary<Integer, TDictionary<TPosition, Boolean>>.Create(GridNotify);
    for Level in NewGrids.Keys do
      Grids.Add(Level, TDictionary<TPosition, Boolean>.Create(NewGrids[Level]));

    NewGrids.Free;
  end;

  Result := 0;
  for Grid in Grids.Values do
    for Infested in Grid.Values do
      if Infested then
        Inc(Result);

  Grids.Free;
  EmptyGrid.Free;
end;

procedure TAdventOfCodeDay24.GridNotify(Sender: TObject; const Item: TDictionary<TPosition, Boolean>; Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    Item.Free;
end;
{$ENDREGION}
{$REGION 'TAdventOfCodeDay25'}
function TAdventOfCodeDay25.SolveA: Variant;
var Computer: TBasicIntComputer;
    InputQueue: TQueue<string>;
    Output, i: Integer;
    Line: string;
    Lines: TStringList;
    OutputMessages: TStack<String>;
begin
  InputQueue := TQueue<string>.Create;
  Lines := TStringList.Create;
  OutputMessages := TStack<String>.Create;;
  Computer := TBasicIntComputer.Create(FInput[0]);
  Computer.StopOnOutPut := True;

  try
    Lines.LoadFromFile(SaveFilePath);
    for i := 0 to Lines.Count-1 do
      InputQueue.Enqueue(Lines[i]);

    Line := '';
    while not Computer.IsStopped do
    begin
      if (Computer.InstructionQueueCount = 0) and (InputQueue.Count > 0) then
        Computer.QueueASCIICode(InputQueue.Dequeue);

      Output := Computer.Run;
      if Output = 10 then
      begin
        OutputMessages.Push(Line);
        Line := '';
      end
      else
        Line := Line + Char(Output);
    end;

    while True do
    begin
      Result := OutputMessages.Pop;
      if Result <> '' then
        Exit;
    end;
  finally
    Lines.Free;
    OutputMessages.Free;
    Computer.Free;
    InputQueue.Free;
  end;
end;

function TAdventOfCodeDay25.SolveB: Variant;
begin
  Result := 'Done!'
end;
{$ENDREGION}
initialization
  RegisterClasses([TAdventOfCodeDay1, TAdventOfCodeDay2, TAdventOfCodeDay3, TAdventOfCodeDay4, TAdventOfCodeDay5,
                   TAdventOfCodeDay6, TAdventOfCodeDay7, TAdventOfCodeDay8, TAdventOfCodeDay9, TAdventOfCodeDay10,
                   TAdventOfCodeDay11, TAdventOfCodeDay12, TAdventOfCodeDay13, TAdventOfCodeDay14, TAdventOfCodeDay15,
                   TAdventOfCodeDay16, TAdventOfCodeDay17, TAdventOfCodeDay18, TAdventOfCodeDay19, TAdventOfCodeDay20,
                   TAdventOfCodeDay21, TAdventOfCodeDay22, TAdventOfCodeDay23, TAdventOfCodeDay24, TAdventOfCodeDay25]);

end.

