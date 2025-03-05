unit newServerFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TNewServerForm }

  TNewServerForm = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    ResponseMemo: TMemo;
    PortEdit: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
  private

  public

  end;

var
  NewServerForm: TNewServerForm;

implementation

{$R *.lfm}

{ TNewServerForm }

end.

