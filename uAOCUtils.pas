unit uAOCUtils;

interface

uses
  inifiles, System.SysUtils, System.Generics.Collections, AOCBase, RTTI, System.Classes,
  System.Net.HttpClient, System.Net.urlclient, system.Generics.Defaults;

type AOCconfig = Record
  BaseUrl: string;
  BaseFilePath: string;
  SessionCookie: string;
  procedure LoadConfig;
End;

type TAdventOfCodeRef = class of TAdventOfCode;

type AOCUtils = class
  public
    class var Config: AOCConfig;
    class function GetAdventOfCode: TList<TAdventOfCodeRef>;
    class function DayIndexFromClassName(Const aClassName: String): String;
    class procedure DoAdventOfCode(aAdventOfCodeRef: TAdventOfCodeRef);
    class procedure DownLoadPuzzleInput(var InputList: TStrings; Const DayIndex: String);
end;

type TAOCDictionary<TKey,TValue> = class(TDictionary<TKey,TValue>)
  public
    procedure AddOrIgnoreValue(const Key: TKey; const Value: TValue);
end;

type
  TPosition = record
    x: integer;
    y: Integer;
    procedure SetIt(const aX, aY: integer);
    procedure AddDelta(const aX, aY: Integer);
    function Equals(Const Other: TPosition): Boolean;
  end;

function GCD(Number1, Number2: integer): integer;

implementation

procedure AOCconfig.LoadConfig;
const Config: string = 'Config';
var Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    BaseUrl := Ini.ReadString(Config, 'BaseUrl', '');
    BaseFilePath := Ini.ReadString(Config, 'BaseFilePath', '');
    SessionCookie := Ini.ReadString(Config, 'SessionCookie', '');
  finally
    Ini.Free;
  end;
end;

class function AOCUtils.GetAdventOfCode: TList<TAdventOfCodeRef>;
var
  ctx: TRttiContext;
  lType: TRttiType;
  AdventOfCode: TAdventOfCodeRef;
  Comparison: TComparison<TAdventOfCodeRef>;
begin
  result := TList<TAdventOfCodeRef>.Create;
  ctx := TRttiContext.Create;
  Writeln('Discovering advent of code');
  for lType in ctx.GetTypes do
    if (lType is TRttiInstanceType) and (TRttiInstanceType(lType).MetaclassType.InheritsFrom(TAdventOfCode))
    then
    begin
      AdventOfCode := TAdventOfCodeRef(TRttiInstanceType(lType).MetaclassType);
      Writeln('Found '+ AdventOfCode.ClassName);
      if AdventOfCode.ClassName <> TAdventOfCode.ClassName then
        Result.Add(adventOfCode);
    end;

  Comparison :=
    function(const Left, Right: TAdventOfCodeRef): Integer
    begin
      Result := StrToInt(AOCUtils.DayIndexFromClassName(Left.ClassName)) -
                StrToInt(AOCUtils.DayIndexFromClassName(Right.ClassName));
    end;
  Result.Sort(TComparer<TAdventOfCodeRef>.Construct(Comparison));
end;

class function AOCUtils.DayIndexFromClassName(Const aClassName: String): String;
var i: Integer;
begin
  i := Length('TAdventOfCodeDay');
  Result := Copy(aClassName, i + 1, Length(aClassName) - i);
end;

class procedure AOCUtils.DoAdventOfCode(aAdventOfCodeRef: TAdventOfCodeRef);
var AdventOfCode: TAdventOfCode;
begin
  AdventOfCode := aAdventOfCodeRef.Create;
  try
    AdventOfCode.Solve;
  finally
    AdventOfCode.Free;
  end;
end;

class procedure AOCUtils.DownLoadPuzzleInput(var InputList: TStrings; Const DayIndex: String);
var HttpClient: THttpClient;
    lHeader: TNetHeader;
    Headers: TNetHeaders;
    MemoryStream: TMemoryStream;
    Url: string;
begin
  Url := AOCUtils.Config.BaseUrl+'/day/'+DayIndex+'/input';
  WriteLn('Downloading puzzle data from ' + Url);

  HttpClient := THTTPClient.Create;
  lHeader := LHeader.Create('cookie', AOCUtils.Config.SessionCookie );
  SetLength(Headers, 1);
  Headers[0] := lHeader;
  MemoryStream := TMemoryStream.Create;
  try
    HttpClient.Get(Url, MemoryStream, Headers);
    InputList.LoadFromStream(MemoryStream);
  finally
    HttpClient.Free;
    MemoryStream.Free;
  end;
end;

procedure TAOCDictionary<TKey,TValue>.AddOrIgnoreValue(const Key: TKey; const Value: TValue);
begin
  if not Self.ContainsKey(Key) then
    Self.Add(Key, Value);
end;

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

function TPosition.Equals(Const Other: TPosition): Boolean;
begin
  Result := (x = Other.x) and (y = Other.y);
end;

function GCD(Number1, Number2: integer): integer;
var
  Temp: integer;
begin
  if Number1 < 0 then Number1 := -Number1;
  if Number2 < 0 then Number2 := -Number2;

  repeat
    if Number1 < Number2 then
      begin
        Temp := Number1;
        Number1 := Number2;
        Number2 := Temp;
      end;

    Number1 := Number1 mod Number2;
  until (Number1 = 0);

  result := Number2;
end;


end.
