unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Menus, environmentUnit, serverEnvironmentUnit,
  Types, RequestTabSheet, ServerTabSheet;

type
  { TMainForm }

  TMainForm = class(TForm)
    AggegatedServerResultPageControl: TTabSheet;
    AggregatedRequestResultSheet: TTabSheet;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    LoadTabsButton: TButton;
    AddRequestButton: TButton;
    AddServerButton: TButton;
    ServerResultMemo: TMemo;
    RequestResultPageControl: TPageControl;
    ResultMemo: TMemo;
    ResultPanel: TPanel;
    DeleteMenuOption: TMenuItem;
    DeleteServerMenuItem: TMenuItem;
    RenameServerInfoMenu: TMenuItem;
    SaveServerInfoMenu: TMenuItem;
    Rename: TMenuItem;
    Save: TMenuItem;
    PageControl1: TPageControl;
    ClientPageControl: TPageControl;
    ServerResultPageControl: TPageControl;
    ServerTabMenu: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TabMenu: TPopupMenu;
    ServerPageControl: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    ClientSheet: TTabSheet;
    ServerSheet: TTabSheet;
    procedure AddRequestButtonClick(Sender: TObject);
    procedure ClientPageControlContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure ClientPageControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DeleteMenuOptionClick(Sender: TObject);
    procedure DeleteServerMenuItemClick(Sender: TObject);
    procedure LoadTabsButtonClick(Sender: TObject);
    procedure AddServerButtonClick(Sender: TObject);
    procedure ClientPageControlCloseTabClicked(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ResultMemoChange(Sender: TObject);
    procedure RenameClick(Sender: TObject);
    procedure RenameServerInfoMenuClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure SaveServerInfoMenuClick(Sender: TObject);
    procedure ServerPageControlCloseTabClicked(Sender: TObject);
  private
    FEnv: TEnv;
    FServerEnv: TServerEnv;
    procedure OnWebRequestHandler(Sender: TObject; const Body: String);
    procedure ResetRequestTab;
    procedure ResetServerTab;
    procedure AddRequestTab(const AClientInfo: TClientInfo);
    procedure AddServerTab(const AServerInfo: TServerInfo);
    procedure RequestMemoAdd(Sender: TObject; const Message: String);
    procedure ServerMemoAdd(Sender: TObject; const Message: String);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.LoadTabsButtonClick(Sender: TObject);
var
  All: TClientInfoArray;
  AllServer: TServerInfoArray;
  i: Integer;
begin
  ResetRequestTab;
  all := FEnv.All();
  for i := 0 to High(all) do
    AddRequestTab(all[i]);

  ResetServerTab;
  allServer := FServerEnv.All();
    for i := 0 to High(allServer) do
    AddServerTab(allServer[i]);
end;

procedure TMainForm.DeleteMenuOptionClick(Sender: TObject);
begin
  if TabMenu.PopupComponent is TTabSheet then
   begin
      FEnv.Remove(TTabSheet(TabMenu.PopupComponent).Caption);
      ClientPageControlCloseTabClicked(TabMenu.PopupComponent);
   end;
end;

procedure TMainForm.ClientPageControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TabIndex: Integer;
begin
  if Button = mbRight then
  begin
    TabIndex := TPageControl(Sender).IndexOfTabAt(X, Y);
    if TabIndex <> -1 then
    begin
      TPageControl(Sender).ActivePageIndex := TabIndex;
      if TPageControl(Sender).Name = 'ClientPageControl' then
      begin
       TabMenu.PopupComponent := TPageControl(Sender).ActivePage;
       TabMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y)
      end
      else
      begin
        ServerTabMenu.PopupComponent := TPageControl(Sender).ActivePage;
        ServerTabMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
      end;
    end;
  end;
end;

procedure TMainForm.ClientPageControlContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
end;

procedure TMainForm.AddRequestButtonClick(Sender: TObject);
var
  CleintInfo: TClientInfo;
begin
  CleintInfo.Name := 'Request ' + IntToStr(ClientPageControl.PageCount + 1);
  AddRequestTab(CleintInfo);
end;

procedure TMainForm.DeleteServerMenuItemClick(Sender: TObject);
begin
    if ServerTabMenu.PopupComponent is TTabSheet then
     begin
      FServerEnv.Remove(TTabSheet(ServerTabMenu.PopupComponent).Caption);
      ServerPageControlCloseTabClicked(ServerTabMenu.PopupComponent);
     end;
end;

procedure TMainForm.AddServerButtonClick(Sender: TObject);
var
  ServerInfo: TServerInfo;
begin
  ServerInfo.Name := 'Server ' + IntToStr(ServerPageControl.PageCount + 1);
  ServerInfo.Port := 80;
  ServerInfo.ResponseCode:= 200;
  AddServerTab(ServerInfo);
end;

procedure TMainForm.ClientPageControlCloseTabClicked(Sender: TObject);
begin
  TTabSheet(Sender).Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FEnv := TEnv.Create;
  FServerEnv := TServerEnv.Create;
  LoadTabsButtonClick(nil);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FEnv.Free;
  FServerEnv.Free;
end;

procedure TMainForm.ResultMemoChange(Sender: TObject);
begin
     TMemo(Sender).SelStart := Length(TMemo(Sender).Text);
     TMemo(Sender).SelLength := 0;
end;

procedure TMainForm.RenameClick(Sender: TObject);
var
  OldTabName, NewTabName: String;
  i: Integer;
begin
  if TabMenu.PopupComponent is TTabSheet then
    begin
      OldTabName := TTabSheet(TabMenu.PopupComponent).Caption;
      NewTabName := InputBox('Rename Tab', 'Enter new tab name:', OldTabName);

      FEnv.Rename(TTabSheet(TabMenu.PopupComponent).Caption, NewTabName);
      TTabSheet(TabMenu.PopupComponent).Caption:= NewTabName;

      for i := 0 to RequestResultPageControl.PageCount -1 do
      begin
        if RequestResultPageControl.Page[i].Caption = OldTabName then
                RequestResultPageControl.Page[i].Caption := NewTabName;
      end;
    end;
end;

procedure TMainForm.RenameServerInfoMenuClick(Sender: TObject);
var
  OldTabName, NewTabName: String;
  i: Integer;
begin
    if ServerTabMenu.PopupComponent is TTabSheet then
    begin
      OldTabName := TTabSheet(ServerTabMenu.PopupComponent).Caption;
      NewTabName := InputBox('Rename Tab', 'Enter new tab name:', OldTabName);
      FServerEnv.Rename(TTabSheet(ServerTabMenu.PopupComponent).Caption, NewTabName);
      TTabSheet(ServerTabMenu.PopupComponent).Caption:= NewTabName;

      for i := 0 to ServerResultPageControl.PageCount -1 do
      begin
        if ServerResultPageControl.Page[i].Caption = OldTabName then
                ServerResultPageControl.Page[i].Caption := NewTabName;
      end;
    end;
end;

procedure TMainForm.SaveClick(Sender: TObject);
begin
  with TRequestTabSheet(TabMenu.PopupComponent) do
   begin
       FEnv.Add(Caption, GetClientInfo);
       OnResponse:=@RequestMemoAdd;
   end;
end;

procedure TMainForm.SaveServerInfoMenuClick(Sender: TObject);
begin
  with TServerTabSheet(ServerTabMenu.PopupComponent) do
   begin
       FServerEnv.Add(Caption, GetServerInfo);
   end;
end;

procedure TMainForm.ServerPageControlCloseTabClicked(Sender: TObject);
begin
  with TTabSheet(Sender) do
  begin
      while ComponentCount > 0 do components[ComponentCount -1].Free;
  end;
  TTabSheet(Sender).Free;
end;

procedure TMainForm.OnWebRequestHandler(Sender: TObject; const Body: String);
begin
  ResultMemo.Lines.Add('Called with: ' + #13+#10 + Body);
end;

procedure TMainForm.ResetRequestTab;
begin
  while ClientPageControl.PageCount > 0 do ClientPageControl.Page[ClientPageControl.PageCount -1].Free;
end;

procedure TMainForm.ResetServerTab;
begin
  while ServerPageControl.PageCount > 0 do ServerPageControl.Page[ServerPageControl.PageCount -1].Free;
end;

procedure TMainForm.AddRequestTab(const AClientInfo: TClientInfo);
var
  ResultTabSheet: TTabSheet;
  RequestResultMemo: TMemo;
begin
  with TRequestTabSheet.Create(ClientPageControl, AClientInfo) do
   begin
     PageControl := ClientPageControl;
     PopupMenu := TabMenu;

     ResultTabSheet := RequestResultPageControl.AddTabSheet;
     ResultTabSheet.Caption:=AClientInfo.Name;
     RequestResultMemo := TMemo.Create(ResultTabSheet);
     RequestResultMemo.Parent := ResultTabSheet;
     RequestResultMemo.Align:= alClient;
     RequestResultMemo.OnChange:= @ResultMemoChange;
     RequestResultMemo.ScrollBars:= ssAutoVertical;
     Memo := RequestResultMemo;
     OnResponse := @RequestMemoAdd;
   end;
end;

procedure TMainForm.AddServerTab(const AServerInfo: TServerInfo);
var
  ResultTabSheet: TTabSheet;
  RequestResultMemo: TMemo;
begin
   with TServerTabSheet.Create(ServerPageControl, AServerInfo) do
    begin
     PageControl:= ServerPageControl;
     PopupMenu := ServerTabMenu;

     ResultTabSheet := ServerResultPageControl.AddTabSheet;
     ResultTabSheet.Caption:=AServerInfo.Name;
     RequestResultMemo := TMemo.Create(ResultTabSheet);
     RequestResultMemo.Parent := ResultTabSheet;
     RequestResultMemo.Align:= alClient;
     RequestResultMemo.OnChange:= @ResultMemoChange;
     RequestResultMemo.ScrollBars:= ssAutoVertical;
     Memo := RequestResultMemo;
     OnResponse := @ServerMemoAdd;
    end;
end;

procedure TMainForm.RequestMemoAdd(Sender: TObject; const Message: String);
begin
  ResultMemo.Lines.Add(Message);
  ResultMemo.Lines.Add('---------------------');

  if (Sender is TRequestTabSheet) and Assigned(TRequestTabSheet(Sender).Memo) then
  begin
   TRequestTabSheet(Sender).Memo.Lines.Add(Message);
   TRequestTabSheet(Sender).Memo.Lines.Add('---------------------');
  end;
end;

procedure TMainForm.ServerMemoAdd(Sender: TObject; const Message: String);
begin
  ServerResultMemo.Lines.Add(Message);
  ServerResultMemo.Lines.Add('---------------------');

  if (Sender is TServerTabSheet) and Assigned(TServerTabSheet(Sender).Memo) then
  begin
   TServerTabSheet(Sender).Memo.Lines.Add(Message);
   TServerTabSheet(Sender).Memo.Lines.Add('---------------------');
  end;
end;

end.

