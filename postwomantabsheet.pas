unit PostWomanTabSheet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ComCtrls, ExtCtrls, keyValueEditor;

type
  TComponentId = (Method = 1000, Url = 2000, Port = 3000, Body = 4000, Button = 5000, ResponseCode = 6000, HeaderEditor = 7000, FormData = 8000);

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
      function Getheaders: String;
      function GetFormData: String;

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
  if (Acomponent.Tag = AComponentId) then
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

function TPostWonamTabSheet.Getheaders: String;
var
   RComponent: TComponent;
begin
  RComponent := GetComponentById(Self, Ord(TComponentId.HeaderEditor));
  if Assigned(RComponent) then
  begin
    Result := TKeyValueEditor(RComponent).Values;
    Exit;
  end;

  raise Exception.Create('Header Edit cannot be found');
end;

function TPostWonamTabSheet.GetFormData: String;
var
   RComponent: TComponent;
begin
  RComponent := GetComponentById(Self, Ord(TComponentId.FormData));
  if Assigned(RComponent) then
  begin
    Result := TKeyValueEditor(RComponent).Values;
    Exit;
  end;

  raise Exception.Create('Form data cannot be found');
end;

end.

