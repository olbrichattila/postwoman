unit server;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, opensslsockets, stopSignalUnit;

type

  { TServer }
  TOnWebRequest = procedure(Sender: TObject; const AMessage: string) of object;
  TServer = class(TThread)
  private
    FServerStopper: TServerStopper;

    FOnWebRequest: TOnWebRequest;
    FResponseText: String;
    FPort: Integer;
    FRequestBody: String;
    FRunning: Boolean;
    FRunSignal: Boolean;
    procedure FPHttpServerRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
    procedure OnRequest;
    procedure TurnOnRunning;
    procedure TurnOffRunning;
    procedure TurnOnRunSignal;
    procedure TurnOffRunSignal;
  protected
      procedure Execute; override;
  public
    constructor Create();
    destructor Destroy; override;

    procedure StartServer;
    procedure StopServer;
    property OnWebRequest: TOnWebRequest read FOnWebRequest write FOnWebRequest;
    property ResponseText: String read FResponseText write FResponseText;
    property Port: Integer read FPort write FPort;
    property Running: Boolean read FRunning;
  end;


implementation

procedure TServer.FPHttpServerRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  FRequestBody := ARequest.URL + chr(13) + ARequest.Content;
  if ARequest.URL = '/stop-postwoman/server' then
     begin
       WriteLn('Deactivate');
       TFPHTTPServer(Sender).Active:= false;
       WriteLn('Deactivated');
       Synchronize(@TurnOffRunning);
       exit;
     end;

  AResponse.Content := FResponseText;
  AResponse.ContentLength := Length(AResponse.Content);
  AResponse.ContentType := 'text/plain';
  AResponse.SendResponse;

  Synchronize(@OnRequest);
end;

procedure TServer.OnRequest;
begin
  if Assigned(FOnWebRequest) then
     FOnWebRequest(self, FRequestBody);
end;

procedure TServer.TurnOnRunning;
begin
  FRunning:=true;
end;

procedure TServer.TurnOffRunning;
begin
  FRunning:=false;
end;

procedure TServer.TurnOnRunSignal;
begin
  FRunSignal:=true;
end;

procedure TServer.TurnOffRunSignal;
begin
  FRunSignal:=false;
end;

{ TServer }
procedure TServer.Execute;
var
  FHTTPServer: TFPHTTPServer;
begin
  Synchronize(@TurnOffRunSignal);
  Synchronize(@TurnOffRunning);
  FHTTPServer := TFPHTTPServer.Create(nil);
  FHTTPServer.KeepConnections := false;
  FHTTPServer.KeepConnectionTimeout := 1000;
  //FHTTPServer.Threaded:=true;
  FHTTPServer.OnRequest := @FPHttpServerRequest;

  try
    while not Terminated do
    begin
      if FRunSignal = true then
         begin
           Synchronize(@TurnOffRunSignal);
           Synchronize(@TurnOnRunning);
           FHTTPServer.Port := FPort;
           FHTTPServer.Active:=true;
         end;
      Sleep(100);
    end;
  finally
      FHTTPServer.Free;
  end;
end;

constructor TServer.Create();
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FServerStopper := TServerStopper.Create;
end;

destructor TServer.Destroy;
begin
  if FRunning then StopServer;

  FServerStopper.Terminate;
  FServerStopper.WaitFor;
  FServerStopper.Free;

  inherited Destroy;
end;

procedure TServer.StartServer;
begin
  TurnOnRunSignal;
end;

procedure TServer.StopServer;
begin
  FServerStopper.Send(FPort);
end;

end.

