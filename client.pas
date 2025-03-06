unit client;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient;

type
    { TTheadedClient }
    TOnResponse = procedure(Sender: TObject; const AResponse: String) of Object;
    TTheadedClient = class(TThread)
    private
        FOnResponse: TOnResponse;
        FResult, FUrl, FBody, FMethod: String;
        procedure OnResult;
        procedure SetOnResponse(AOnResponse: TOnResponse);
    protected
        procedure Execute; override;
    public
        constructor Create(Url, Method: String; AOnResponse: TOnResponse);
        constructor Create(Url, Method, Body: String; AOnResponse: TOnResponse);
        property OnResponse: TOnResponse read FOnResponse write SetOnResponse;
    end;

    { TClient }
    TClient = class(TObject)
      private
      FOnResponse: TOnResponse;
      FURL: String;
      FBody: String;
      procedure SetOnResponse(AOnResponse: TOnResponse);
      public
      // Standard HTTP request types
        procedure Request(Amethod: String);

        property URL: String read FURL write FURL;
        property Body: String read FBody write FBody;
        property OnResponse: TOnResponse read FOnResponse write SetOnResponse;
    end;

implementation

{ TTheadedClient }

procedure TTheadedClient.OnResult;
begin
  if Assigned(FOnResponse) then FOnResponse(Self, FResult);
end;

procedure TTheadedClient.SetOnResponse(AOnResponse: TOnResponse);
begin
  FOnResponse:=AOnResponse;
end;

procedure TTheadedClient.Execute;
var
  Stream, ResponseStream:  TStringStream;
  HTTPCilent: TFPHTTPClient;
begin
  Stream := TStringStream.Create(FBody);
  ResponseStream := TStringStream.Create(FBody);
  HTTPCilent := TFPHTTPClient.Create(nil);
  try
    HTTPCilent.RequestBody := Stream;
    HTTPCilent.HTTPMethod(FMethod, FURL, ResponseStream, []);
    FResult:= ResponseStream.DataString;
    Synchronize(@OnResult);
  finally
    Stream.Free;
    ResponseStream.Free;
    HTTPCilent.Free;
  end;
end;

constructor TTheadedClient.Create(Url, Method: String; AOnResponse: TOnResponse);
begin
  inherited Create(True);
  FreeOnTerminate := true;
  FOnResponse := AOnResponse;
  FUrl := Url;
  FMethod := Method;
  Start;
end;

constructor TTheadedClient.Create(Url, Method, Body: String; AOnResponse: TOnResponse);
begin
  inherited Create(True);
  FreeOnTerminate:= true;
  FOnResponse:=AOnResponse;
  FUrl:=Url;
  FBody:=Body;
  FMethod := Method;
  Start;
end;

procedure TClient.SetOnResponse(AOnResponse: TOnResponse);
begin
  FOnResponse:=AOnResponse;
end;

{ TClient }
procedure TClient.Request(Amethod: String);
begin
  if FBody <> '' then
    TTheadedClient.Create(FURL, Amethod, FBody, FOnResponse)
  else
    TTheadedClient.Create(FURL, Amethod, FOnResponse)
end;

end.

