unit PostWomanTabSheet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ComCtrls, ExtCtrls;

type
  TComponentId = (Method = 1, Url = 2, Port = 3, Body = 4, Button = 5, ResponseCode = 6);

  { TPostWonamTabSheet }
  TPostWonamTabSheet = class(TTabSheet)
    private
    protected
      function GetComponentById(AComponent : TComponent; AComponentId: Integer): TComponent;
      function GetPort: Integer;
      function GetResponseCode: Integer;
      function GetBody: String;
      function GetMethod: String;
      function GetUrl: String;

    public
  end;


implementation

{ TPostWonamTabSheet }

function TPostWonamTabSheet.GetComponentById(AComponent: TComponent;
  AComponentId: Integer): TComponent;
var
   i: Integer;
begin
  Result := nil;
  if (Acomponent.Tag = AComponentId) and not (AComponent is TTabSheet) and not (AComponent is TPanel) then
  begin
    Result := AComponent;
    exit;
  end;

  for i:= 0 to AComponent.ComponentCount - 1 do
  begin
    Result := GetComponentById(AComponent.Components[i], AComponentId);
    if Assigned(Result) then exit;
  end;
end;

function TPostWonamTabSheet.GetPort: Integer;
var
   RComponent: TComponent;
begin
  RComponent := GetComponentById(Self, Ord(TComponentId.Port));
  if Assigned(RComponent) then
  begin
    Result := StrToInt(TEdit(RComponent).Text);
    Exit;
  end;

  raise Exception.Create('Port Edit cannot be found');
end;

function TPostWonamTabSheet.GetResponseCode: Integer;
var
   RComponent: TComponent;
begin
  RComponent := GetComponentById(Self, Ord(TComponentId.ResponseCode));
  if Assigned(RComponent) then
  begin
    Result := StrToInt(TEdit(RComponent).Text);
    Exit;
  end;

  raise Exception.Create('Response code Edit cannot be found');
end;

function TPostWonamTabSheet.GetBody: String;
var
   RComponent: TComponent;
begin
  RComponent := GetComponentById(Self, Ord(TComponentId.Body));
  if Assigned(RComponent) then
  begin
    Result := TMemo(RComponent).Lines.Text;
    Exit;
  end;

  raise Exception.Create('Body Memo cannot be found');

end;

function TPostWonamTabSheet.GetMethod: String;
var
   RComponent: TComponent;
begin
  RComponent := GetComponentById(Self, Ord(TComponentId.Method));
  if Assigned(RComponent) then
  begin
    Result := TComboBox(RComponent).Text;
    Exit;
  end;

  raise Exception.Create('Method ComboBox cannot be found');

end;

function TPostWonamTabSheet.GetUrl: String;
var
   RComponent: TComponent;
begin
  RComponent := GetComponentById(Self, Ord(TComponentId.Url));
  if Assigned(RComponent) then
  begin
    Result := TEdit(RComponent).Text;
    Exit;
  end;

  raise Exception.Create('URL Edit cannot be found');
end;

end.

