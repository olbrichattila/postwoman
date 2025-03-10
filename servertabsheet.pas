unit ServerTabSheet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Menus, serverEnvironmentUnit, PostWomanTabSheet, httpSocket,
  keyValueEditor;

type TOnResponse = procedure(Sender: TObject; const Message: String) of Object;
type
  { TRequestTabSheet }

  { TServerTabSheet }

  TServerTabSheet = class(TPostWonamTabSheet)
    private
      FMemo: TMemo;
      FOnresponse: TOnResponse;
      FServerInfo: TServerInfo;
      FHttpSocket: THttpSocket;
      procedure Build;
      procedure ServerSendBtnClick(Sender: TObject);
      procedure OnWebResponse(Sender: TObject; const AMessage: string);
    protected
    public
      constructor Create(TheOwner: TComponent; const AServerInfo: TServerInfo);
      destructor Destroy; override;
      public function GetServerInfo: TServerInfo;
      property OnResponse: TOnResponse read FOnresponse write FOnresponse;
      property Memo: TMemo read FMemo write FMemo;
  end;


implementation

{ TServerTabSheet }

procedure TServerTabSheet.Build;
var
   Panel: TPanel;
   TabSheet, ResponseTabSheet, ResponseHeaderTabSheet: TTabSheet;
begin
  TabSheet := Self;
  Panel := TPanel.Create(TabSheet);
  with Panel do
  begin
    Parent := TabSheet;
    align := alTop;
    with TLabel.Create(TabSheet) do
    begin
       Parent := Panel;
       Top := 20;
       Width := 60;
       Caption := 'Port:'
    end;

    with TEdit.Create(TabSheet) do
    begin
       Parent := Panel;
       Tag := Ord(TComponentId.Port);
       Top := 10;
       Left := 60;
       Width := 100;
       NumbersOnly:=true;
       Text := IntToStr(FServerInfo.Port);
    end;

    with TLabel.Create(TabSheet) do
    begin
       Parent := Panel;
       Left := 160;
       Top := 20;
       Width := 100;
       Caption := 'Response code:'
    end;

    with TEdit.Create(TabSheet) do
    begin
       Parent := Panel;
       Tag := Ord(TComponentId.ResponseCode);
       Top := 10;
       Left := 270;
       Width := 80;
       NumbersOnly:=true;
       Text := IntToStr(FServerInfo.ResponseCode);
    end;

    with TButton.Create(TabSheet) do
    begin
       Parent := Panel;
       Tag := Ord(TComponentId.Button);
       Caption := 'Start';
       Top := 10;
       Left := 360;
       Width := 100;
       Height := 35;
       OnClick := @ServerSendBtnClick;
    end;
  end;
  with TPageControl.Create(Self) do
  begin
       Align := alClient;
       Parent := Self;
       // Add raw response contorl
       ResponseTabSheet:= AddTabSheet;
       ResponseTabSheet.Caption := 'Raw Response';

       // Add raw header control
       ResponseHeaderTabSheet:= AddTabSheet;
       ResponseHeaderTabSheet.Caption := 'Response header';
  end;

  with TMemo.Create(ResponseTabSheet) do
  begin
     Parent := ResponseTabSheet;
     Tag := Ord(TComponentId.Body);
     Text := FServerInfo.Body;
     Align:= alClient;
  end;

  with TKeyValueEditor.Create(ResponseHeaderTabSheet) do
  begin
    Tag := Ord(TComponentId.HeaderEditor);
     Values := FServerInfo.Headers
  end;
end;

procedure TServerTabSheet.ServerSendBtnClick(Sender: TObject);
var
   StartResult: String;
begin
   if not FHttpSocket.Running then
   begin
     FHttpSocket.Port := GetPort;
     FHttpSocket.Body:= GetBody;
     FHttpSocket.ResponseCode:= GetResponseCode;
     FHttpSocket.Headers:= Getheaders;
     StartResult := FHttpSocket.StartServer;
     if StartResult <> '' then
        ShowMessage('Cannot start server ' + StartResult)
     else
       TButton(Sender).Caption := 'Stop'
   end
   else
   begin
     FHttpSocket.StopServer;
     TButton(Sender).Caption := 'Start'
   end;
end;

procedure TServerTabSheet.OnWebResponse(Sender: TObject; const AMessage: string);
begin
  if Assigned(FOnresponse) then FOnresponse(Self, AMessage);
end;

constructor TServerTabSheet.Create(TheOwner: TComponent; const AServerInfo: TServerInfo);
begin
   inherited Create(TheOwner);
   FServerInfo:= AServerInfo;
   FHttpSocket := THttpSocket.Create;
   FHttpSocket.OnWebRequest:=@OnWebResponse;
   Caption := AServerInfo.Name;
   Build;
end;

destructor TServerTabSheet.Destroy;
begin
  if FHttpSocket.Running then FHttpSocket.StopServer;
  FHttpSocket.Free;
  inherited Destroy;
end;

function TServerTabSheet.GetServerInfo: TServerInfo;
begin
  Result.Name:= Caption;
  Result.Body:= GetBody;
  Result.Port:= GetPort;
  Result.Headers:= Getheaders;
  Result.ResponseCode:= GetResponseCode;
end;

end.

