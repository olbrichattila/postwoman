unit keyValueEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Grids, Dialogs, StrUtils,
  CustomHelpers;

type

  { TKeyValueEditor }

  TKeyValueEditor = class(TPanel)
  private
    FStringGrid: TStringGrid;
    procedure StringGridResize(Sender: TObject);
    procedure Build;
    procedure DeleteButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure SetValues(AValues: String);
    function GetValues: String;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    property Values: String read GetValues write SetValues;
end;

implementation

{ TKeyValueEditor }

procedure TKeyValueEditor.StringGridResize(Sender: TObject);
const
  FixedKeyColumnWidth = 150;
begin
  with TStringGrid(Sender) do
  begin
      ColWidths[0] := FixedKeyColumnWidth;
      ColWidths[1] := ClientWidth - FixedKeyColumnWidth;
  end;
end;

procedure TKeyValueEditor.Build;
var
  Panel: TPanel;
begin
  // Editor
  Panel := TPanel.Create(Self);
  with Panel do
  begin
    Parent:= Self;
    Align:= alClient;
    FStringGrid := TStringGrid.Create(Panel);
    with FStringGrid do
    begin
      Parent:= Panel;
      Align:= alClient;
      ColCount := 2;
      RowCount := 1;
      FixedCols := 0;
      FixedRows := 1;
      Cells[0, 0] := 'Key';
      Cells[1, 0] := 'Value';
      Options := Options + [goEditing];
      OnResize:=@StringGridResize;
      StringGridResize(FStringGrid);
    end;
  end;

  // Button Panel
  Panel := TPanel.Create(Self);
  with Panel do
  begin
    Parent:= Self;
    Height:= 30;
    Align:= alBottom;

    // Delete button
    with TButton.Create(Panel) do
    begin
      Parent := Panel;
      Caption := 'Delete';
      Align:= alLeft;
      OnClick := @DeleteButtonClick;
    end;

    // Add button
    with TButton.Create(Panel) do
    begin
      Parent := Panel;
      Caption := 'Add';
      Align:= alClient;
      OnClick := @AddButtonClick;
    end;
  end;
end;

procedure TKeyValueEditor.DeleteButtonClick(Sender: TObject);
begin
  if FStringGrid.Row > 0 then
    begin
        if MessageDlg('Are you sure you want to delete ' + FStringGrid.Cols[0][FStringGrid.Row] +  '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          FStringGrid.DeleteRow(FStringGrid.Row);
          FStringGrid.AutoSizeColumn(1);
        end;
    end;
end;

procedure TKeyValueEditor.AddButtonClick(Sender: TObject);
begin
  FStringGrid.RowCount := FStringGrid.RowCount + 1;
end;

procedure TKeyValueEditor.SetValues(AValues: String);
var
  i: Integer;
  Splitted: TSplitted;
  StringList: TStringList;
begin
  StringList:= TStringList.Create;
  StringList.Text := AValues;
  try

  FStringGrid.RowCount := +1;
  for i := 0 to StringList.Count-1 do
  begin
    Splitted := SplitStringInTwo(StringList[i]);
    if Splitted.Left <> '' then
    begin
      FStringGrid.RowCount := FStringGrid.RowCount + 1;
      FStringGrid.Cells[0, FStringGrid.RowCount-1] := Splitted.Left;
      FStringGrid.Cells[1, FStringGrid.RowCount-1] := Splitted.Right;
    end;
  end;
  finally
      StringList.Free;
  end;
end;

function TKeyValueEditor.GetValues: String;
var
  i: Integer;
  StringList: TStringList;
begin
  Result := '';
  try
    StringList := TStringList.Create;
    for i := 1 to FStringGrid.RowCount -1 do
    begin
      if Trim(FStringGrid.Cells[0, i]) <> '' then
        StringList.Add(FStringGrid.Cells[0, i] +': '+FStringGrid.Cells[1, i])
    end;

  finally
      Result := StringList.Text;
      StringList.Free;
  end
end;

constructor TKeyValueEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent:= TWinControl(AOwner);
  Align:= alClient;
  Build;
end;

end.

