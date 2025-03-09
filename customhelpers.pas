unit CustomHelpers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSplitted = record
    Left: String;
    Right: String;
  end;

function SplitStringInTwo(const AString: String): TSplitted;
function LinesToTabbedLine(const ASTring: String): String;
function TabbedLineToLines(const ASTring: String): String;

implementation

function SplitStringInTwo(const AString: String): TSplitted;
var
  i: Integer;
  isLeftSide: Boolean;
begin
  Result.Left := '';
  Result.Right := '';
  isLeftSide := true;
  for i:= 1 to length(AString) do
  begin
       if not isLeftSide then
       begin
          Result.Right:=Result.Right+AString[i];
          continue;
       end;

       if AString[i] = ':' then
       begin
          isLeftSide := false;
          continue;
       end;

       Result.Left := Result.Left+AString[i];
  end;
  Result.Left := Trim(Result.Left);
  Result.Right := Trim(Result.Right);
end;

function LinesToTabbedLine(const ASTring: String): String;
var
  i: integer;
  StringList: TStringList;
begin
  Result := '';
  StringList := TStringList.Create;
  try
    StringList.Text := ASTring;
    for i := 0 to StringList.Count -1  do
    begin
         if i > 0 then Result := Result + #9;
         Result := Result + StringList[i];
    end;
  finally
    StringList.Free;
  end;
  // FOR SOME REASON IT DOES NOT REPLACE replace any linux or windows line endings
  //result :=  StringReplace(ASTring, #13#10, #9, [rfReplaceAll]);
  //result :=  StringReplace(Result, #10#13, #9, [rfReplaceAll]);
  //result :=  StringReplace(Result, #13, #9, [rfReplaceAll]);
end;

function TabbedLineToLines(const ASTring: String): String;
begin
  result :=  StringReplace(ASTring, #9, #13#10, [rfReplaceAll]);
end;

end.

