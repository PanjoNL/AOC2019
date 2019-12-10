unit AOCSolutions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Defaults, System.Generics.Collections,
  system.Diagnostics, AOCBase, RegularExpressions, System.DateUtils, system.StrUtils,
  system.Math, IntComputers, uAOCUtils;

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
    Fprogram: TDictionary<Integer, int64>;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
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
    Fprogram: TDictionary<Integer, int64>;
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
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
    procedure BeforeSolve; override;
    procedure AfterSolve; override;
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
procedure TAdventOfCodeDay5.BeforeSolve;
begin
  Fprogram := TBasicIntComputer.ParseIntput(FInput[0]);
end;

procedure TAdventOfCodeDay5.AfterSolve;
begin
  Fprogram.Free;
end;

function TAdventOfCodeDay5.SolveA: Variant;
begin
  Result := TBasicIntComputer.RunProgram(Fprogram, 1);  //2845163
end;

function TAdventOfCodeDay5.SolveB: Variant;
begin
  Result := TBasicIntComputer.RunProgram(Fprogram, 5); //9436229
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
    while not  Amplifiers[4].Istopped do
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
procedure TAdventOfCodeDay9.BeforeSolve;
begin
  Fprogram := TBasicIntComputer.ParseIntput(FInput[0]);
end;

procedure TAdventOfCodeDay9.AfterSolve;
begin
  Fprogram.Free;
end;

function TAdventOfCodeDay9.SolveA: Variant;
begin
  Result := TBasicIntComputer.RunProgram(Fprogram, 1); //2789104029
end;

function TAdventOfCodeDay9.SolveB: Variant;
begin
  Result := TBasicIntComputer.RunProgram(Fprogram, 2); //32869
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
    i: Integer;
    angle: Extended;
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
procedure TAdventOfCodeDay11.BeforeSolve;
begin

end;

procedure TAdventOfCodeDay11.AfterSolve;
begin

end;

function TAdventOfCodeDay11.SolveA: Variant;
begin

end;

function TAdventOfCodeDay11.SolveB: Variant;
begin

end;
{$ENDREGION}

initialization
  RegisterClasses([TAdventOfCodeDay1, TAdventOfCodeDay2, TAdventOfCodeDay3, TAdventOfCodeDay4, TAdventOfCodeDay5,
    TAdventOfCodeDay6, TAdventOfCodeDay7, TAdventOfCodeDay8, TAdventOfCodeDay9, TAdventOfCodeDay10, TAdventOfCodeDay11
]);

end.

