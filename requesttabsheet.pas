unit RequestTabSheet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Menus, environmentUnit, PostWomanTabSheet, client;

type TOnResponse = procedure(Sender: TObject; const Message: String) of Object;
type
  { TRequestTabSheet }
  TRequestTabSheet = class(TPostWonamTabSheet)
    private
      FOnresponse: TOnResponse;
      FCLientInfo: TClientInfo;
      FCLient: TClient;
      procedure Build;
      procedure ClientSendBtnClick(Sender: TObject);
      procedure SetOnResponse(AOnresponse: TOnResponse);
    protected
    public
      constructor Create(TheOwner: TComponent; const AClientInfo: TClientInfo);
      destructor Destroy; override;
      function GetClientInfo: TClientInfo;
      property OnResponse: TOnResponse read FOnresponse write SetOnResponse;

  end;

implementation

{ TRequestTabSheet }

procedure TRequestTabSheet.Build;
var
  TabSheet: TTabSheet;
  Panel: TPanel;
  Index: Integer;
begin
  TabSheet := Self;
  Panel := TPanel.Create(TabSheet);
  with Panel do
  begin
    Parent := TabSheet;
    Align :=  alTop;
    with TComboBox.Create(TabSheet) do
    begin
       Parent := Panel;
       Align := alLeft;
       Tag := Ord(TComponentId.Method);
       Width := 130;
       Items.Add('GET');
       Items.Add('POST');
       Items.Add('PUT');
       Items.Add('PATCH');
       Items.Add('DELETE');
       Items.Add('HEAD');
       Items.Add('OPTIONS');
       Items.Add('TRACE');
       Items.Add('CONNECT');
       Index := Items.IndexOf(FClientInfo.Method);
       if Index <> -1 then
        ItemIndex := Index
       else
        ItemIndex := 0;
    end;

    with TEdit.Create(TabSheet) do
    begin
       Parent := Panel;
       Align := alClient;
       Tag := Ord(TComponentId.Url);
       Width := 230;
       Text := FClientInfo.URL;
       Anchors := [akLeft, akRight, akTop];
    end;

    with TButton.Create(TabSheet) do
    begin
       Parent := Panel;
       Align := alRight;
       Tag := Ord(TComponentId.Button);
       Caption := 'Send';
       Width := 100;
       Anchors := [akRight, akTop];
       OnClick := @ClientSendBtnClick;
    end;
  end;
  with TMemo.Create(TabSheet) do
  begin
     Parent := TabSheet;
     Tag := Ord(TComponentId.Body);
     Lines.Text := FClientInfo.Body;
     Align:= alClient;
  end;
end;

procedure TRequestTabSheet.ClientSendBtnClick(Sender: TObject);
var
   RequestType: String;
begin
  if not Assigned(FOnresponse) then exit;
   FCLient.URL := GetUrl;
   RequestType := GetMethod;
   case RequestType of
        'GET', 'HEAD', 'OPTIONS', 'TRACE', 'CONNECT': FClient.Body := ''
         else FClient.Body := GetBody;
   end;

   FCLient.Request(RequestType);
end;

procedure TRequestTabSheet.SetOnResponse(AOnresponse: TOnResponse);
begin
  WriteLn('Set on response');
  FCLient.OnResponse:=AOnresponse;
  FOnresponse:=AOnresponse;
end;

constructor TRequestTabSheet.Create(TheOwner: TComponent; const AClientInfo: TClientInfo);
begin
   inherited Create(TheOwner);
   FCLientInfo:= AClientInfo;
   FCLient := TClient.Create;
   FCLient.OnResponse:=FOnresponse;
   Caption := AClientInfo.Name;
   Build;
end;

destructor TRequestTabSheet.Destroy;
begin
  FCLient.Free;
  inherited Destroy;
end;

function TRequestTabSheet.GetClientInfo: TClientInfo;
begin
   Result.Body:= GetBody;
   Result.Method:= GetMethod;
   Result.URL:= GetUrl;
   Result.Name:= Caption;
end;

end.

