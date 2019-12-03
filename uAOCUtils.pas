unit uAOCUtils;

interface

uses
  inifiles, System.SysUtils, System.Generics.Collections, AOCBase, RTTI, System.Classes,
  System.Net.HttpClient, System.Net.urlclient;

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

implementation

procedure AOCconfig.LoadConfig;
const Config: string = 'Config';
var Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), 'ini'));
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
var
  HttpClient: THttpClient;
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


end.
