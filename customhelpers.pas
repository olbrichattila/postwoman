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

end.

