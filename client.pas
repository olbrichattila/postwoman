unit client;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, CustomHelpers;

type
    { TTheadedClient }
    TOnResponse = procedure(Sender: TObject; const AResponse: String) of Object;
    TTheadedClient = class(TThread)
    private
        FOnResponse: TOnResponse;
        FResult, FUrl, FBody, FMethod, FHeaders, FResponseHeaders: String;
        procedure OnResult;
        procedure SetOnResponse(AOnResponse: TOnResponse);
        function GetResponseHeaders(AClient: TFPHTTPClient): String;
    protected
        procedure Execute; override;
    public
        constructor Create(Url, Method, Headers: String; AOnResponse: TOnResponse);
        constructor Create(Url, Method, Headers, Body: String; AOnResponse: TOnResponse);
        property OnResponse: TOnResponse read FOnResponse write SetOnResponse;
    end;

    { TClient }
    TClient = class(TObject)
      private
        FOnResponse: TOnResponse;
        FURL: String;
        FBody: String;
        FHeaders: String;
        procedure SetOnResponse(AOnResponse: TOnResponse);

      public
      // Standard HTTP request types
        procedure Request(Amethod: String);
        property URL: String read FURL write FURL;
        property Body: String read FBody write FBody;
        property Headers: String read FHeaders write FHeaders;
        property OnResponse: TOnResponse read FOnResponse write SetOnResponse;
    end;

implementation

{ TTheadedClient }

procedure TTheadedClient.OnResult;
begin
  if Assigned(FOnResponse) then
    FOnResponse(Self, '---Response headers---' + #13#10 + FResponseHeaders + #13#10 + '---Response body---' + #13#10 +FResult);
end;

procedure TTheadedClient.SetOnResponse(AOnResponse: TOnResponse);
begin
  FOnResponse:=AOnResponse;
end;

procedure TTheadedClient.Execute;
var
  Stream, ResponseStream:  TStringStream;
  HeaderList: TStringList;
  HTTPCilent: TFPHTTPClient;
  Splitted: TSplitted;
  i: Integer;
begin
  Stream := TStringStream.Create(FBody);
  ResponseStream := TStringStream.Create;
  HeaderList := TStringList.Create;
  HeaderList.Text := FHeaders;
  HTTPCilent := TFPHTTPClient.Create(nil);
  try
    // TODO this add header could be a separate function
    for i := 0 to HeaderList.Count - 1 do
    begin
         Splitted := SplitStringInTwo(HeaderList[i]);
         HTTPCilent.AddHeader(Splitted.Left, Splitted.Right);
    end;

    HTTPCilent.RequestBody := Stream;
    HTTPCilent.HTTPMethod(FMethod, FURL, ResponseStream, []);
    FResult:= ResponseStream.DataString;
    FResponseHeaders:= GetResponseHeaders(HTTPCilent);
    Synchronize(@OnResult);
  finally
    Stream.Free;
    ResponseStream.Free;
    HeaderList.Free;
    HTTPCilent.Free;
  end;
end;

constructor TTheadedClient.Create(Url, Method, Headers: String; AOnResponse: TOnResponse);
begin
  inherited Create(True);
  FreeOnTerminate := true;
  FOnResponse := AOnResponse;
  FUrl := Url;
  FMethod := Method;
  FHeaders:= Headers;
  Start;
end;

constructor TTheadedClient.Create(Url, Method, Headers, Body: String; AOnResponse: TOnResponse);
begin
  inherited Create(True);
  FreeOnTerminate:= true;
  FOnResponse:=AOnResponse;
  FUrl:=Url;
  FBody:=Body;
  FMethod := Method;
  FHeaders:= Headers;
  Start;
end;

function TTheadedClient.GetResponseHeaders(AClient: TFPHTTPClient): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to AClient.ResponseHeaders.Count - 1 do
  begin
    if i > 0 then Result := Result + #13#10;
    Result := Result + AClient.ResponseHeaders[i];
  end;
end;

{ TClient }
procedure TClient.SetOnResponse(AOnResponse: TOnResponse);
begin
  FOnResponse:=AOnResponse;
end;

procedure TClient.Request(Amethod: String);
begin
  if FBody <> '' then
    TTheadedClient.Create(FURL, Amethod, FHeaders, FBody, FOnResponse)
  else
    TTheadedClient.Create(FURL, Amethod, FHeaders, FOnResponse)
end;

end.

