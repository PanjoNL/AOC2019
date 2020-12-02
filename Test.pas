unit Test;

interface

uses
  System.Generics.Collections{, SBX509}, system.Classes{, SBHTTPSClient}, System.JSON, IdCoderMIME,   System.Net.HttpClient, System.Net.urlclient;

  type
  TSmAzureDevopsApi = Class
  private
    //Httpclient
    class function SetupHTTPClient: THttpClient;
    class procedure CleanUpHTTPClient(Const aClient: THttpClient);
//    class procedure TestHttpClientCertificateValidate(Sender: TObject; X509Certificate: TElX509Certificate; var Validate: Boolean);

//    //hulpfuncties
//    class procedure WriteResponseToDebug(Const aHttpClient: TElHTTPSClient; Const aHttpState: integer; Const aUrl: String);
    class function ReadStream(Const aStream: TStream): String;
    class function DoHttpGet(Const aUrl: String; Out aResponse: String): Boolean;
//
//    //Calls
//    class function GetSurveyId: string;
//    class function Internal_GetSurveyDetails: String;
//    class function Internal_PostResponse(Const aResponse: String): Boolean;
  public
//    Class function BestaatWorkItem(Const aTfsNummer: String): Boolean;
//    Class procedure OpenWorkItem(Const aTfsNummer: String);
    Class procedure PostComment(Const aTfsNummer, aComment: String);

    Class function GetCollector: String;
  End;

//TODO opnemen in appsettings ofzo
const access_token: String = 'afk2w62ycpzy5qzu6ay5adw65vqvd75yuxoc2azglizhurwucbvq';//'ah5bjvwg3motxzvmgxganua7emt6a7qzpqviqyebce4lyl7uhtjq';

//GOED 'OmFoNWJqdndnM21vdHh6dm1neGdhbnVhN2VtdDZhN3F6cHF2aXF5ZWJjZTRseWw3dWh0anE=';
//
// bASE 64 ZONDER PUNT IN ENCODING ;'YWg1Ymp2d2czbW90eHp2bWd4Z2FudWE3ZW10NmE3cXpwcXZpcXllYmNlNGx5bDd1aHRqcQ==';

// pat 'ah5bjvwg3motxzvmgxganua7emt6a7qzpqviqyebce4lyl7uhtjq';

//   '-u.LTH6vsBz6-NOCh1uwDdrOEVGgWOCiK1VDOLS45CAPMUKOedYyiT3uEv4Ur.1sUOIdXVENqaZF24W.nQ9ZlkhA8sRITOKjDIRwfb.bfrypdIDN5BeBH9BRGWUqSqqa';
// 'NOCh1uwDdrOEVGgWOCiK1VDOLS45CAPMUKOedYyiT3uEv4Ur.1sUOIdXVENqaZF24W.nQ9ZlkhA8sRITOKjDIRwfb.bfrypdIDN5BeBH9BRGWUqSqqa';

	  SurveyBaseUrl: String = 'https://api.surveymonkey.com/v3';
      SurveyTitle: String = 'Magister OOP Feedback';

implementation

uses
  system.StrUtils, System.SysUtils;

class function TSmAzureDevopsApi.ReadStream(Const aStream: TStream): String;
begin
  aStream.Position := 0;
  with TStreamReader.Create(aStream) do
  begin
    Result := ReadToEnd;
    Free;
  end;
end;

//class procedure TSmAzureDevopsApi.WriteResponseToDebug(Const aHttpClient: TElHTTPSClient; Const aHttpState: integer; Const aUrl: String);
//
//  Procedure DumpHeaders(Const aHeaderName: String; Const aHeaders: TStringList);
//  var i: Integer;
//  begin
//    WriteLn(#6, aHeaderName);
//    for i := 0 to aHeaders.Count - 1 do
//      Writeln(aHeaders[i]);
//  end;
//
//Var sFile: String;
//begin
//  if smConsts.DebugTool1 = nil then
//    Exit; //Geen debugscherm?? Dan ook niks processen
//
//  Writeln(#6, 'HttpState: ' + IntToStr(aHttpState));
//  WriteLn(#6, 'Url= ' + aUrl);
//  WriteLn(ReadStream(aHttpClient.OutputStream));
//
//  if DebugHook <> 0 then //En voor devs ook nog eens de hele handel opslaan in de logfolder
//  begin
//    DumpHeaders('Request headers:', aHttpClient.RequestHeaders);
//    DumpHeaders('Response headers:', aHttpClient.ResponseHeaders);
//    sfile := ExtractFilePath(ParamStr(0)) + 'log\' + FormatDateTime('yyyymmdd', now) + '\';
//    ForceDirectories(sfile);
//    (aHttpClient.OutputStream as TMemoryStream).SaveToFile(sfile + 'HttpState='+IntToStr(aHttpState) + '_' + FormatDateTime('hh-nn-ss.zzz', Now) + '.json');
//  end;
//end;

class function TSmAzureDevopsApi.SetupHTTPClient: THttpClient;
var encoder: TIdEncoderMIME;
    lHeader: TNetHeader;
    Headers: TNetHeaders;
begin
//  encoder := TIdEncoderMIME.Create;
//  Result := TElHTTPSClient.Create(nil);
//  Result.OnCertificateValidate := TestHttpClientCertificateValidate;
//  Result.RequestParameters.ContentType := 'application/json';
//  Result.RequestParameters.Accept := 'application/json';
//  Result.RequestParameters.Authorization := 'Basic ' + encoder.Encode(':'+access_token);
//  Result.OutputStream := TMemoryStream.Create;

//  encoder := TIdEncoderMIME.Create;
//  Result := THttpClient.Create(nil);
//
//
//  Result.ContentType := 'application/json';
//  Result.Accept := 'application/json';
//
//  lHeader := lHeader.Create('Authorization', 'Basic ' + encoder.Encode(':'+access_token));
//  SetLength(Headers, 1);
//  Headers[0] := lHeader;




end;

class procedure TSmAzureDevopsApi.CleanUpHTTPClient(Const aClient: THttpClient);
begin
//  aClient.Close;
  aClient.Free;
end;

//class procedure TSmAzureDevopsApi.TestHttpClientCertificateValidate(Sender: TObject; X509Certificate: TElX509Certificate; var Validate: Boolean);
//begin
//  Validate := True;
//end;

class function TSmAzureDevopsApi.DoHttpGet(Const aUrl: String; Out aResponse: String): Boolean;
var encoder: TIdEncoderMIME;
    lHeader: TNetHeader;
    Headers: TNetHeaders;
  ResultState: integer;
  HttpClient: THttpClient;
  Stream: TMemoryStream;
   Response: IHTTPResponse;
begin


   encoder := TIdEncoderMIME.Create;
//  Result := THttpClient.Create(nil);
//  Result.OnCertificateValidate := TestHttpClientCertificateValidate;


  HttpClient := THttpClient.Create;
  HttpClient.ContentType := 'application/json';
  HttpClient.Accept := 'application/json';

  lHeader := lHeader.Create('Authorization', 'Basic ' + encoder.Encode(':'+access_token));
  SetLength(Headers, 1);
  Headers[0] := lHeader;



  aResponse := '';
  Result := False;
  Stream := TMemoryStream.Create;
  try
    try
      Response:=  HttpClient.Get(aURL, Stream, Headers);
//      WriteResponseToDebug(HttpClient, ResultState, aURL);
      Writeln(Response.StatusText);

      Result := (ResultState = 200);

      WriteLn(readStream(Stream));

//      if Result then
//        aResponse := ReadStream(HttpClient.OutputStream);
    except
      on E:Exception do
      begin
        WriteLn(E.Message);
      end;
    end;
  finally
    Encoder.Free;
    HttpClient.Free;
    Stream.Free;
  end;
end;
//
//Class function TsmSurveyAPi.GetSurveyDetails: String;
//Var temp: TStringList;
//begin
//  Result := Internal_GetSurveyDetails;
//
//  if (Result = '') and FileExists('C:\Sources\Magister4\Main\Source\exe\SurveyTest.json') then //Temp om te testen
//  begin
//    temp := TStringList.Create;
//    Temp.LoadFromFile('C:\Sources\Magister4\Main\Source\exe\SurveyTest.json');
//    Result := Temp.Strings[0];
//    Temp.Free;
//  end;
//end;
//
Class function TSmAzureDevopsApi.GetCollector: String;
var
  HttpReponse: string;
  JsonValue: TJSONValue;
begin
//  if CollectorId <> '' then //TODO beslissen wat voor collector we terug willen geven, nu pakken we keihard de eerste
//    Exit(CollectorId);

  if  DoHttpGet('https://dev.azure.com/magister/Magister%20Suite/_apis/wit/workitems/226890/?api-version=5.1', HttpReponse) then
  begin
//    JsonValue := TJSonObject.ParseJSONValue(HttpReponse);
//    Result := JsonValue.GetValue<String>('data[0].id');
//    JsonValue.Free;
  end;



//  CollectorId := Result;
end;

Class procedure TSmAzureDevopsApi.PostComment(Const aTfsNummer, aComment: String);
var encoder: TIdEncoderMIME;
    lHeader, lHeader2: TNetHeader;
    Headers: TNetHeaders;
  ResultState: integer;
  HttpClient: THttpClient;
  Stream, Stream2: TMemoryStream;
  Url: string;
  Body: TStringList;
  Response: IHTTPResponse;
begin

    Url := 'https://dev.azure.com/magister/Magister%20Suite/_apis/wit/workitems/226890/comments?api-version=5.1-preview.3';
   encoder := TIdEncoderMIME.Create;
//  Result := THttpClient.Create(nil);
//  Result.OnCertificateValidate := TestHttpClientCertificateValidate;


  HttpClient := THttpClient.Create;
  HttpClient.ContentType := 'application/json';
  HttpClient.Accept := 'application/json';

  lHeader := lHeader.Create('Authorization', 'Basic ' + encoder.Encode(':'+access_token));
//  lHeader2 := lHeader.Create('Content-Type', 'application/json');

  SetLength(Headers, 1);
  Headers[0] := lHeader;
//  Headers[1] := lHeader2;

//  Body := TStringList.Create;
//  Body.Add('{"text": "Test via api"}');
  Stream2 := TStringStream.Create('{"text": "Test via api"}');


//  LHeaders := [TNetHeader.Create('Content-Type:', 'application/json; charset=' + GetEncodingMIMEName(LEncoding))] + AHeaders;  //

//   TNetHeader.Create('Content-Type:', 'application/x-www-form-urlencoded; Authorization: '

//  aResponse := '';
//  Result := False;
//  HttpClient := SetupHTTPClient;
  Stream := TMemoryStream.Create;
  try
    try
      HttpClient.Post(URL, Stream2, Stream, Headers);
//      WriteResponseToDebug(HttpClient, ResultState, aURL);
//      Result := (ResultState = 200);

      WriteLn(readStream(Stream));

//      if Result then
//        aResponse := ReadStream(HttpClient.OutputStream);
    except
      on E:Exception do
      begin
        WriteLn(E.Message);
      end;
    end;
  finally

  end;



end;

initialization

  //LET OP, code laten staan, is wel handig om te debuggen

  if DebugHook <> 0 then
  begin
    TThread.CreateAnonymousThread(
      procedure
      begin
        Sleep(0*1000); //Magister even de tijd geven om fatsoenlijk op te starten, Anders niet eens een dubugscherm.....
        TThread.Synchronize(nil,
        procedure
        var s: String;
        begin
          Try
            s := TSmAzureDevopsApi.GetCollector;

//            TSmAzureDevopsApi.PostComment('Test', 'Test')
          except
            on e: exception do WriteLn('Fout bij testen van de surveyApi: '+ e.Message);
          End
        end)
      end).Start;
  end;

end.
