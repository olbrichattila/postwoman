unit httpSocket;
{ This is a re-creation of FPHTTPServer
 as the free pascal HTTP server shutdown does not work and hangs the program on close, free or colse tab
 regardless of threaded version, then leaves the port binded, I was not able to make it work, regardless of putting into thread,
 trying different threaded modes, therefore here is a non blocking lower level implementation for this purpos
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, serverSocket;

type
  TOnWebRequest = procedure(Sender: TObject; const AMessage: string) of object;
  { THttpSocket }
  THttpSocket = class(TObject)
    private
      FBody: String;
      FOnWebRequest: TOnWebRequest;
      FPort, FResponseCode: Integer;
      FServerSocket: TServerSocket;
      FRunning: Boolean;
      procedure OnInternalWebRequest(Sender: TObject; const AMessage: string);
    public
      function StartServer: String;
      procedure StopServer;
      property Running: Boolean read FRunning;
      property Body: String read FBody write FBody;
      property OnWebRequest: TOnWebRequest read FOnWebRequest write FOnWebRequest;
      property Port: Integer read FPort write FPort;
      property ResponseCode: Integer read FResponseCode write FResponseCode;
  end;


implementation

{ THttpSocket }
procedure THttpSocket.OnInternalWebRequest(Sender: TObject;
  const AMessage: string);
begin
  if Assigned(FOnWebRequest) then
     begin
       FOnWebRequest(self, AMessage);
     end;
end;

function THttpSocket.StartServer: String;
var
  ServerStartError: String;
begin
  Result := '';
  FRunning:=true;
  FServerSocket:= TServerSocket.Create;
  FServerSocket.Body:=FBody;
  FServerSocket.OnWebRequest := @OnInternalWebRequest;
  Result := FServerSocket.StartServer(FPort, FResponseCode);
  if ServerStarterror <> '' then
  begin
      FRunning := false;
      FreeAndNil(FServerSocket);
  end;
end;

procedure THttpSocket.StopServer;
begin
  if Assigned(FServerSocket) then
     begin
     FServerSocket.Terminate;
     FServerSocket.WaitFor;
     FreeAndNil(FServerSocket);
     FRunning:=false;
     end;
end;

end.

