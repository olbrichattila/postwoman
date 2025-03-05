unit stopSignalUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient;
type

  { TServerStopper }

  TServerStopper = class(TThread)
  private
    FSend: Boolean;
    FPort: Integer;
    procedure TurnOffSendSignal;
    procedure TurnOnSendSignal;

  protected
    procedure Execute; override;
  public
    constructor Create();
    procedure Send(APort: Integer);
end;

implementation

{ TServerStopper }

procedure TServerStopper.TurnOffSendSignal;
begin
  FSend:=false;
end;

procedure TServerStopper.TurnOnSendSignal;
begin
  FSend:=true;
end;

procedure TServerStopper.Execute;
var
  FHTTPCilent: TFPHTTPClient;
begin
  Synchronize(@TurnOffSendSignal);
  FHTTPCilent:= TFPHTTPClient.Create(nil);
  FHTTPCilent.ConnectTimeout:= 100;

   WriteLn('Server Stopper started');
  try
    while not Terminated do
    begin
      if FSend then
      begin
        Synchronize(@TurnOffSendSignal);
        try
          WriteLn('Send stop signal');
          FHTTPCilent.Get('http://localhost:'+IntToStr(FPort)+'/stop-postwoman/server');
          // Need sometimes to trigger a second server request so the server stops
//          FHTTPCilent.Get('http://localhost:'+IntToStr(FPort));
          WriteLn('Stop signal sent');
        except
            on E: Exception do
            begin
              WriteLn('could not close server, maybe not running')
            end;
        end;
      end;
      sleep(100);
      WriteLn('Sleep');
    end;
  finally
    FHTTPCilent.Free;
  end;
end;

constructor TServerStopper.Create();
begin
  inherited Create(False);
end;

procedure TServerStopper.Send(APort: Integer);
begin
  FPort:=APort;
  TurnOnSendSignal;
end;

end.

