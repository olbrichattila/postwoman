unit serverSocket;
// Please see comment on HTTP socket, why it was done this way

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Sockets, BaseUnix;

type
  TOnWebRequest = procedure(Sender: TObject; const AMessage: string) of object;

  { TServerSocket }
  TServerSocket = class(TThread)
  private
    FPort, FResponseCode: Integer;
    FBody, FPayload, FHeaders: String;
    FMainSocket: Boolean;
    FClientSocket, FServerSocket: TSocket;
    FOnWebRequest: TOnWebRequest;
    function SplitHeaderLeft(S: String): String;
    function SplitHeaderRight(S: String): String;
    procedure NotifyHandledWebrequest;
  protected
    procedure Execute; override;
  public
    constructor Create;
    constructor Create(AClientSocket: TSocket; AResponseCode: Integer; Body, Headers : String);
    function StartServer(Aport, AResponseCode: Integer; AHeaders: String): String;
    procedure ExecuteMainSocket;
    procedure ExecuteClientSocket;
    property Body: String read FBody write FBody;
    property OnWebRequest: TOnWebRequest read FOnWebRequest write FOnWebRequest;
  end;


const
  F_GETFL = 3; // Get file status flags
  F_SETFL = 4; // Set file status flags
  O_NONBLOCK = 2048; // Non-blocking mode flag (may vary by platform)

implementation

{ TServerSocket }
function TServerSocket.SplitHeaderLeft(S: String): String;
var
  i: Integer;
begin
  Result := '';

  for i := 1 to length(S)  do
  begin
    if S[i] = ':' then
    begin
      Result := Trim(Result);
      Exit;
    end;
    Result := Result + S[i]
  end
end;

function TServerSocket.SplitHeaderRight(S: String): String;
var
  AppendString: Boolean;
  i: Integer;
begin
  Result := '';
  AppendString := false;
  for i := 1 to length(S)  do
  begin
    if (AppendString = false) and (S[i] = ':') then AppendString := true;
    if AppendString and (S[i] <> ':') then Result := Result + S[i];
  end;
  Result := Trim(Result);
end;

procedure TServerSocket.NotifyHandledWebrequest;
begin
  if Assigned(FOnWebRequest) then
    FOnWebRequest(self, FPayload);
end;

procedure TServerSocket.Execute;
begin
   if FMainSocket then
      ExecuteMainSocket
   else
      ExecuteClientSocket;
end;
constructor TServerSocket.Create;
begin
  FreeOnTerminate := False;
  FMainSocket := true;
  inherited Create(True);
end;

constructor TServerSocket.Create(AClientSocket: TSocket; AResponseCode: Integer; Body, Headers : String);
begin
  FBody := Body;
  FResponseCode := AResponseCode;
  FreeOnTerminate := True;
  FClientSocket := AClientSocket;
  FHeaders := Headers;
  FMainSocket := false;
  inherited Create(True);
  Start;
end;

function TServerSocket.StartServer(Aport, AResponseCode: Integer; AHeaders: String): String;
var
  ServerAddr: TInetSockAddr;
  Flags: LongInt;
begin
  Result := '';
  FPort := Aport;
  FResponseCode := AResponseCode;
  FHeaders := AHeaders;
  FMainSocket := true;

  // Create the server socket
  FServerSocket := fpSocket(AF_INET, SOCK_STREAM, 0);
  if FServerSocket = -1 then
  begin
    Result := 'Error creating server socket';
    Exit;
  end;

  // Set the socket to non-blocking mode
  Flags := fpfcntl(FServerSocket, F_GETFL, 0);
  fpfcntl(FServerSocket, F_SETFL, Flags or O_NONBLOCK);

  // Set up the server address
  ServerAddr.sin_family := AF_INET;
  ServerAddr.sin_port := htons(FPort);
  ServerAddr.sin_addr.s_addr := INADDR_ANY;

  // Bind the socket to the address
  if fpBind(FServerSocket, @ServerAddr, SizeOf(ServerAddr)) = -1 then
  begin
    Result := 'Error binding port ' + IntToStr(Aport);
    CloseSocket(FServerSocket);
    Exit;
  end;

  // Listen for incoming connections
  if fpListen(FServerSocket, 5) = -1 then
  begin
    Result := 'Error lisening on connection, Port:' + IntToStr(Aport);
    CloseSocket(FServerSocket);
    Exit;
  end;
  Start;
end;

procedure TServerSocket.ExecuteMainSocket;
var
  ClientAddr: TSockAddr;
  ClientAddrLen: TSockLen;
begin
  ClientAddrLen := SizeOf(ClientAddr);
  while not Terminated do
  begin
     FClientSocket:= fpAccept(FServerSocket, @ClientAddr, @ClientAddrLen);
     if FClientSocket = -1  then
     begin
       sleep(10);
       continue;
     end;
     TServerSocket.Create(FClientSocket, FResponseCode, FBody, FHeaders).OnWebRequest:=FOnWebRequest;
    sleep(10);
  end;

  fpShutdown(FClientSocket, SHUT_RDWR);
  CloseSocket(FServerSocket);
end;

procedure TServerSocket.ExecuteClientSocket;
var
  Buffer: array[0..1023] of Char;
  BufferPointer: Integer;
  Response, Line, Lines, Payload: String;
  BytesRead, ConentLength: Integer;
begin
  // TODO refactor this, to be more readable
    Line := '';
    Lines := '';
    FPayload := '';
    Payload := '';
    BufferPointer := 0;
    ConentLength := 0;
    BytesRead := 0;
     try
        repeat
          if BufferPointer = 0 then
          begin
            BytesRead := fpRecv(FClientSocket, @Buffer[0], SizeOf(Buffer), 0);
            if BytesRead <= 0 then
            begin
              // Client disconnected
               CloseSocket(FClientSocket);
               Exit;
            end;
          end;

          if Buffer[BufferPointer] = #13 then
          begin
            if Lines <> '' then
               Lines := Lines + #13+#10+ Line
            else
               Lines := Line;

            if SplitHeaderLeft(Line) = 'Content-Length' then ConentLength := StrToInt(SplitHeaderRight(Line));
            if Line = '' then
               break; // End of request, no content length
            Line := '';
          end;

          if (Buffer[BufferPointer] <> #10) and (Buffer[BufferPointer] <> #13) then
          begin
            Line := Line + Buffer[BufferPointer];
          end;

          inc(BufferPointer);
          if BufferPointer = BytesRead then BufferPointer := 0;
        until false;

      if ConentLength > 0 then
        begin
          repeat
            if BufferPointer = 0 then
            begin
              BytesRead := fpRecv(FClientSocket, @Buffer[0], SizeOf(Buffer), 0);
              if BytesRead <= 0 then
              begin
                 // Client disconnected
                 CloseSocket(FClientSocket);
                 Exit;
              end;
            end;

            Payload := Payload + Buffer[BufferPointer];
            inc(BufferPointer);
            if ConentLength = 0 then break;
            if BufferPointer = BytesRead then BufferPointer := 0;
            dec(ConentLength);
            until false;
          end;

        FPayload:= Lines + Payload;
        Response := 'HTTP/1.1 '+ IntToStr(FResponseCode) +' OK' + #13#10+
        FHeaders + #13#10+
        'Content-Length: ' + IntToStr(length(FBody)) + #13#10+
        'Date: ' + FormatDateTime('ddd, dd mmm yyyy hh:nn:ss', Now) + ' GMT' + #13#10+
        'Server: Apache/2.4.41 (Ubuntu)' + #13#10+
         #13#10+FBody;

        fpSend(FClientSocket, @PChar(Response)[0], length(Response), 0);
      Synchronize(@NotifyHandledWebrequest);
    finally
      CloseSocket(FClientSocket);
    end;
end;
end.

