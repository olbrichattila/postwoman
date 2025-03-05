unit serverEnvironmentUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TServerInfo = record
    Name: String;
    Port: Integer;
    ResponseCode: Integer;
    Body: String;
  end;

  TServerInfoArray = array of TServerInfo;
type
  { TEnv }

  { TServerEnv }

  TServerEnv = class(TObject)
    private
    public
      procedure Add(const AName: string; AServerInfo: TServerInfo);
      procedure Rename(const AFromName, AToName: string);
      procedure Remove(const AName: string);
      function InternalByName(const AName: string): TServerInfo;
      function ByName(const AName: string): TServerInfo;
      function All(): TServerInfoArray;
  end;

const
  FFileExtension: String = '.se';

implementation

{ TEnv }

procedure TServerEnv.Add(const AName: string; AServerInfo: TServerInfo);
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  try
    StringList.Add(IntToStr(AServerInfo.Port));
    StringList.Add(IntToStr(AServerInfo.ResponseCode));
    StringList.Add(AServerInfo.Body);
    StringList.SaveToFile(AName + FFileExtension);
  finally
    StringList.Free;
  end;
end;

procedure TServerEnv.Rename(const AFromName, AToName: string);
begin
  RenameFile(AFromName + FFileExtension, AToName + FFileExtension);
end;

procedure TServerEnv.Remove(const AName: string);
begin
  DeleteFile(AName + FFileExtension);
end;

function TServerEnv.InternalByName(const AName: string): TServerInfo;
var
  StringList: TStringList;
  Body: string;
  i: Integer;
begin
  Result.Name := ChangeFileExt(AName, '');
  Body := '';
  StringList := TStringList.Create;
  try
    StringList.LoadFromFile(AName);
    Result.Port:= StrToInt(StringList[0]);
    for i := 2 to StringList.Count -1 do Body := Body + StringList[i] + chr(13);
    Result.Body := Body;
  finally
    StringList.Free;
  end;
end;

function TServerEnv.ByName(const AName: string): TServerInfo;
begin
     Result := InternalByName(AName + FFileExtension);
end;

function TServerEnv.All(): TServerInfoArray;
var
  SearchRec: TSearchRec;
  Count: Integer;
begin
  Count := 0;

  if FindFirst('*' + FFileExtension, faAnyFile, SearchRec) = 0 then
  begin
    repeat
      SetLength(Result, Count +1);
      Result[Count] := InternalByName(SearchRec.Name);
      Inc(Count);
    until FindNext(SearchRec) <> 0;

    FindClose(SearchRec);
  end;
end;

end.

