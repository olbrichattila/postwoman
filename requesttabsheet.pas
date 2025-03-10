unit RequestTabSheet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Menus, environmentUnit, PostWomanTabSheet, client,
  keyValueEditor, CustomHelpers, httpdefs;

type TOnResponse = procedure(Sender: TObject; const Message: String) of Object;
type
  { TRequestTabSheet }
  TRequestTabSheet = class(TPostWonamTabSheet)
    private
      FBodyPageControl: TPageControl;
      FOnresponse: TOnResponse;
      FCLientInfo: TClientInfo;
      FCLient: TClient;
      FMemo: TMemo;
      procedure Build;
      procedure ClientSendBtnClick(Sender: TObject);
      procedure SetOnResponse(AOnresponse: TOnResponse);
      procedure InternalResponse(Sender: TObject; const Message: String);
      function KeyValuesToParams(AKeyValues: String):String;
    protected
    public
      constructor Create(TheOwner: TComponent; const AClientInfo: TClientInfo);
      destructor Destroy; override;
      function GetClientInfo: TClientInfo;
      property OnResponse: TOnResponse read FOnresponse write SetOnResponse;
      property Memo: TMemo read FMemo write FMemo;
  end;

implementation

{ TRequestTabSheet }

procedure TRequestTabSheet.Build;
var
  TabSheet, ChildTabSheet, RequestTabSheet, FormDataTabSheet: TTabSheet;
  Panel: TPanel;
  ChildPageControl: TPageControl;
  Index: Integer;
begin
  TabSheet := Self;
  // Main panel
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

  // Editors
  ChildPageControl:= TPageControl.Create(TabSheet);
  with ChildPageControl do
  begin
    Align:= alClient;
    Parent := TabSheet;

    // Raw Request
    ChildTabSheet := AddTabSheet;
    ChildTabSheet.Caption := 'Request';
    FBodyPageControl := TPageControl.Create(ChildTabSheet);
    with FBodyPageControl do
    begin
      Align:= alClient;
      Parent := ChildTabSheet;
      // Create body tab sheets
      RequestTabSheet := AddTabSheet;
      RequestTabSheet.Caption:= 'Raw request';

      FormDataTabSheet := AddTabSheet;
      FormDataTabSheet.Caption := 'Form Request';
    end;

    with TMemo.Create(RequestTabSheet) do
    begin
       Parent := RequestTabSheet;
       Tag := Ord(TComponentId.Body);
       Lines.Text := FClientInfo.Body;
       Align:= alClient;
    end;

    with TKeyValueEditor.Create(FormDataTabSheet) do
    begin
       Parent := FormDataTabSheet;
       Tag := Ord(TComponentId.FormData);
       Values := FCLientInfo.FormData;
       Align:= alClient;
    end;

    // Header
    ChildTabSheet := AddTabSheet;
    ChildTabSheet.Caption := 'Headers';
    with TKeyValueEditor.Create(ChildTabSheet) do
    begin
      Tag := Ord(TComponentId.HeaderEditor);
      Values := FCLientInfo.Headers
    end;

  end;
end;

procedure TRequestTabSheet.ClientSendBtnClick(Sender: TObject);
var
   RequestType: String;
begin
  if not Assigned(FOnresponse) then exit;
   FCLient.URL := GetUrl;
   FClient.Headers := GetHeaders;
   RequestType := GetMethod;

   case RequestType of
        'GET', 'HEAD', 'OPTIONS', 'TRACE', 'CONNECT': FClient.Body := ''
         else
           begin
             if FBodyPageControl.ActivePageIndex = 0 then
                FClient.Body := GetBody
              else
                FClient.Body := KeyValuesToParams(GetFormData);
           end
   end;

   FCLient.Request(RequestType);
end;

procedure TRequestTabSheet.SetOnResponse(AOnresponse: TOnResponse);
begin
  FOnresponse:=AOnresponse;
end;

procedure TRequestTabSheet.InternalResponse(Sender: TObject;
  const Message: String);
begin
  if Assigned(FOnresponse) then FOnresponse(Self, Message);
end;

function TRequestTabSheet.KeyValuesToParams(AKeyValues: String): String;
var
   i: Integer;
   StringList: TStringList;
   Splitted: TSplitted;
begin
  Result := '';
  StringList:= TStringList.Create;

  try
     StringList.Text:= AKeyValues;
     for i := 0 to StringList.Count-1 do
     begin
          Splitted := SplitStringInTwo(StringList[0]);
          if i > 0 then Result := Result + '&';
          Result := Result + Splitted.Left + '=' + HTTPEncode(Splitted.Right);
     end;
  finally
    StringList.Free;
  end;
end;

constructor TRequestTabSheet.Create(TheOwner: TComponent; const AClientInfo: TClientInfo);
begin
   inherited Create(TheOwner);
   FCLientInfo:= AClientInfo;
   FCLient := TClient.Create;
   FCLient.OnResponse:=@InternalResponse;
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
   Result.Headers:=Getheaders;
   Result.URL:= GetUrl;
   Result.Name:= Caption;
   Result.FormData := GetFormData;
end;

end.

