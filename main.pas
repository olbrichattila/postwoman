unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Menus, environmentUnit, serverEnvironmentUnit,
  Types, RequestTabSheet, ServerTabSheet;

type
  { TMainForm }

  TMainForm = class(TForm)
    LoadTabsButton: TButton;
    AddRequestButton: TButton;
    AddServerButton: TButton;
    ResultMemo: TMemo;
    DeleteMenuOption: TMenuItem;
    DeleteServerMenuItem: TMenuItem;
    RenameServerInfoMenu: TMenuItem;
    SaveServerInfoMenu: TMenuItem;
    Rename: TMenuItem;
    Save: TMenuItem;
    PageControl1: TPageControl;
    ClientPageControl: TPageControl;
    ServerTabMenu: TPopupMenu;
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
    procedure MemoAdd(Sender: TObject; const Message: String);
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
  NewTabName: String;
begin
  if TabMenu.PopupComponent is TTabSheet then
    begin
      NewTabName := InputBox('Rename Tab', 'Enter new tab name:', TTabSheet(TabMenu.PopupComponent).Caption);
      FEnv.Rename(TTabSheet(TabMenu.PopupComponent).Caption, NewTabName);
      TTabSheet(TabMenu.PopupComponent).Caption:= NewTabName;
    end;
end;

procedure TMainForm.RenameServerInfoMenuClick(Sender: TObject);
var
  NewTabName: String;
begin
    if ServerTabMenu.PopupComponent is TTabSheet then
    begin
      NewTabName := InputBox('Rename Tab', 'Enter new tab name:', TTabSheet(ServerTabMenu.PopupComponent).Caption);
      FServerEnv.Rename(TTabSheet(ServerTabMenu.PopupComponent).Caption, NewTabName);
      TTabSheet(ServerTabMenu.PopupComponent).Caption:= NewTabName;
    end;
end;

procedure TMainForm.SaveClick(Sender: TObject);
begin
  with TRequestTabSheet(TabMenu.PopupComponent) do
   begin
       FEnv.Add(Caption, GetClientInfo);
       OnResponse:=@MemoAdd;
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
begin
  with TRequestTabSheet.Create(ClientPageControl, AClientInfo) do
   begin
     PageControl := ClientPageControl;
     PopupMenu := TabMenu;
     OnResponse := @MemoAdd;
   end;
end;

procedure TMainForm.AddServerTab(const AServerInfo: TServerInfo);
begin
   with TServerTabSheet.Create(ServerPageControl, AServerInfo) do
    begin
         PageControl:= ServerPageControl;
         PopupMenu := ServerTabMenu;
         OnResponse := @MemoAdd;
    end;
end;

procedure TMainForm.MemoAdd(Sender: TObject; const Message: String);
begin
  ResultMemo.Lines.Add(Message);
end;

end.

