unit environmentUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;
type
  TClientInfo = record
    Name: String;
    Method: String;
    URL: String;
    Body: String;
  end;

  TClientInfoArray = array of TClientInfo;
type
  { TEnv }

  TEnv = class(TObject)
    private
    public
      procedure Add(const AName: string; AClientInfo: TClientInfo);
      procedure Rename(const AFromName, AToName: string);
      procedure Remove(const AName: string);
      function InternalByName(const AName: string): TClientInfo;
      function ByName(const AName: string): TClientInfo;
      function All(): TClientInfoArray;
  end;

const
  FFileExtension: String = '.cl';

implementation

{ TEnv }

procedure TEnv.Add(const AName: string; AClientInfo: TClientInfo);
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  try
    StringList.Add(AClientInfo.URL);
    StringList.Add(AClientInfo.Method);
    StringList.Add(AClientInfo.Body);
    StringList.SaveToFile(AName + FFileExtension);
  finally
    StringList.Free;
  end;
end;

procedure TEnv.Rename(const AFromName, AToName: string);
begin
  RenameFile(AFromName + FFileExtension, AToName + FFileExtension);
end;

procedure TEnv.Remove(const AName: string);
begin
  DeleteFile(AName + FFileExtension);
end;

function TEnv.InternalByName(const AName: string): TClientInfo;
var
  StringList: TStringList;
  Body: string;
  i: Integer;
begin
  Body := '';
  Result.Name := AName;
  StringList := TStringList.Create;
  try
    StringList.LoadFromFile(AName);
    Result.URL:= StringList[0];
    Result.Method:= StringList[1];
    for i := 2 to StringList.Count -1 do Body := Body + StringList[i] + chr(13);
    Result.Body := Body;
  finally
    StringList.Free;
  end;
end;

function TEnv.ByName(const AName: string): TClientInfo;
begin
     Result := InternalByName(AName + FFileExtension);
end;

function TEnv.All(): TClientInfoArray;
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

