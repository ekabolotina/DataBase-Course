unit cardinsert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DbCtrls, Connect, MetaUnit;

type

  { TCardInsertForm }

  TCardInsertForm = class(TForm)
    BtnSave: TButton;
    BtnCancel: TButton;
    CardInsertGroupBox: TGroupBox;
    procedure BtnCancelClick(Sender: TObject);
    public
      CurrentTbl: TTableInfo;
      EditFld: array of TEdit;
      procedure PopUpForm(ATable: TTableInfo);
      procedure BuildEditor(ATable: TTableInfo);
  end;

var
  CardInsertForm: array of TCardInsertForm;

implementation

procedure TCardInsertForm.BtnCancelClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TCardInsertForm.BuildEditor(ATable: TTableInfo);
var
  i, preTop: Integer;
begin
  with CardInsertForm[High(CardInsertForm)] do begin
    SetLength(EditFld, Length(ATable.FFields));
    preTop := 0;
    for i := 0 to High(ATable.FFields) do begin
      EditFld[i] := TEdit.Create(nil);
      with EditFld[i] do begin
        Width := 150;
        Height := 23;
        Top := preTop;
        Parent := CardInsertGroupBox;
        Text := ATable.FFields[i].FCaption;
      end;
      EditFld[i].Left := 5;
      ShowMessage(inttostr(EditFld[i].Left));
      preTop += 23;
    end;
  end;
end;

procedure TCardInsertForm.PopUpForm(ATable: TTableInfo);
var
  preTop, preLeft: Integer;
begin
  SetLength(CardInsertForm, Length(CardInsertForm)+1);
  CardInsertForm[High(CardInsertForm)] := TCardInsertForm.Create(nil);
  if High(CardInsertForm) <> 0 then begin
    preTop := CardInsertForm[High(CardInsertForm)-1].Top;
    preLeft := CardInsertForm[High(CardInsertForm)-1].Left;
  end;
  with CardInsertForm[High(CardInsertForm)] do begin
    Show;
    if High(CardInsertForm) <> 0 then begin
      Top := preTop + 15;
      Left := preLeft + 15;
    end;
    Caption := Format('Вставить в таблицу "%s"', [ATable.FCaption]);
  end;
  BuildEditor(ATable);
end;

{$R *.lfm}

end.

