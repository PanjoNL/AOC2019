unit IntComputers;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Defaults, System.Generics.Collections,
  system.Diagnostics, AOCBase, RegularExpressions, System.DateUtils, system.StrUtils,
  system.Math;

type TBasicIntComputer = class(TPersistent)
protected
  Fprogram: TDictionary<Integer, Integer>;
  FOriginalProgram: TDictionary<Integer, Integer>;
  MemPos: Integer;
  function GetParam(Const aIndex: Integer; Const aCommand: String): Integer;
private
  function CanRun: Boolean; virtual;
  procedure ProcessCommand(aCommand: String); virtual;
public
  LastOutput: Integer;
  constructor Create(Input: TDictionary<Integer, Integer>);
  destructor Destroy; override;

  function Run: Integer;
  function Istopped: Boolean;
  function GetMemory(Const MemoryIndex: Integer): Integer;
  procedure WriteMemory(const MemoryIndex, ValueToWrite: Integer);
  class function ParseIntput(const aProgram: String): TDictionary<Integer, Integer>;
end;

type TAmplifierController = class(TBasicIntComputer)
private
  FInternalStop: Boolean;
  FPhaseSettingQueue: TQueue<Integer>;
  FFeedbackMode: boolean;
protected
  procedure ProcessCommand(aCommand: String); override;
  function CanRun: Boolean; override;
public
  constructor Create(Input: TDictionary<Integer, Integer>; PhaseSetting: Integer; FeedBackMode: Boolean);
  destructor Destroy; override;
  function Run: integer; overload;
  procedure SetPhaseSetting(Const aSetting: Integer);
end;


implementation

////////////////////////////////// TBasicIntComputer //////////////////////////////////
class function TBasicIntComputer.ParseIntput(const aProgram: String): TDictionary<Integer, Integer>;
var Line: TStringList;
    i: Integer;
begin
  Result := TDictionary<Integer, Integer>.Create;
  Line := TStringList.Create;
  Line.Delimiter := ',';
  Line.DelimitedText := aProgram;

  for i := 0 to Line.Count - 1 do
    Result.Add(i, StrToInt(Line[i]));

  Line.Free;
end;

constructor TBasicIntComputer.Create(Input: TDictionary<Integer, Integer>);
begin
  FProgram := TDictionary<Integer, Integer>.Create(Input);
  MemPos := 0;
end;

destructor TBasicIntComputer.Destroy;
begin
  FProgram.Free;
end;

function TBasicIntComputer.Run: Integer;
var Command: string;
begin
 while CanRun do
  begin
    Command := RightStr('00000'+IntToStr(FProgram[MemPos]), 5); //104 -> 00104
    ProcessCommand(Command);
  end;

  Result := LastOutput;
end;

function TBasicIntComputer.Istopped: Boolean;
begin
   Result := (FProgram[MemPos] = 99);
end;

procedure TBasicIntComputer.WriteMemory(const MemoryIndex, ValueToWrite: Integer);
begin
  Fprogram[MemoryIndex] := ValueToWrite;
end;

function TBasicIntComputer.GetMemory(Const MemoryIndex: Integer): Integer;
begin
  Result := Fprogram[MemoryIndex]
end;

function TBasicIntComputer.CanRun: Boolean;
begin
  Result := Not Istopped;
end;

function TBasicIntComputer.GetParam(Const aIndex: Integer; Const aCommand: String): Integer;
begin
  if aCommand[Length(aCommand)-1-aIndex] = '1' then
    Result := FProgram[MemPos + aIndex]
  else
    Result := FProgram[FProgram[MemPos + aIndex]]
end;

procedure TBasicIntComputer.ProcessCommand(aCommand: String);

  function _GetParam(Const aIndex: Integer): Integer;
  begin
    Result := GetParam(aIndex, aCommand);
  end;

begin
  case StrToInt(RightStr(aCommand, 2)) of
    1: begin
        FProgram[FProgram[MemPos + 3]] := _GetParam(1) + _GetParam(2);
        MemPos := MemPos + 4;
       end;
    2: begin
        FProgram[FProgram[MemPos + 3]] := _GetParam(1) * _GetParam(2);
        MemPos := MemPos + 4;
       end;
    3: begin
        FProgram[FProgram[MemPos + 1]] := LastOutput;
        MemPos := MemPos + 2
       end;
    4: begin
        LastOutput := _GetParam(1);
        MemPos := MemPos + 2
       end;
    5: begin
        if _GetParam(1) <> 0 then
          MemPos := _GetParam(2)
        else
          MemPos := MemPos + 3
       end;
    6: begin
        if _GetParam(1) = 0 then
          MemPos := _GetParam(2)
        else
          MemPos := MemPos + 3;
       end;
    7: begin
        FProgram[FProgram[MemPos + 3]] := Integer(_GetParam(1) < _GetParam(2));
        MemPos := MemPos + 4;
       end;
    8: begin
        FProgram[FProgram[MemPos + 3]] := Integer(_GetParam(1) = _GetParam(2));
        MemPos := MemPos + 4;
        end;
  else
    raise Exception.CreateFmt('Unknown command: %s', [aCommand]);
  end;
end;


////////////////////////////////// TAmplifierController //////////////////////////////////
constructor TAmplifierController.Create(Input: TDictionary<Integer, Integer>; PhaseSetting: Integer; FeedBackMode: Boolean);
begin
  Inherited Create(Input);
  FPhaseSettingQueue := TQueue<Integer>.Create;
  FPhaseSettingQueue.Enqueue(PhaseSetting);
  FFeedbackMode := FeedBackMode;
end;

destructor TAmplifierController.Destroy;
begin
  FPhaseSettingQueue.Free;
  inherited;
end;

function TAmplifierController.CanRun: Boolean;
begin
  Result := Inherited and (Not FInternalStop);
end;

procedure TAmplifierController.ProcessCommand(aCommand: String);
begin
  case StrToInt(RightStr(aCommand, 2)) of
    3: begin
        if (FPhaseSettingQueue.Count > 0) then
          FProgram[FProgram[MemPos + 1]] := FPhaseSettingQueue.Dequeue
        else
          FProgram[FProgram[MemPos + 1]] := LastOutput;

        MemPos := MemPos + 2
       end;
    4: begin
        LastOutput := GetParam(1, aCommand);;
        MemPos := MemPos + 2;
        if FFeedbackMode then
          FInternalStop := True
       end;
  else
    inherited ProcessCommand(aCommand);
  end;
end;

function TAmplifierController.Run: Integer;
begin
  FInternalStop := False;
  Result := Inherited;
end;

procedure TAmplifierController.SetPhaseSetting(Const aSetting: Integer);
begin
  FPhaseSettingQueue.Enqueue(aSetting);
end;


end.
