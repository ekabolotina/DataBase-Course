unit cardinsert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db,FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DbCtrls, ExtCtrls, Connect, MetaUnit;

type

  TEditPanel = record
    Fld: TEdit;
    Lbl: TLabel;
    CmbBox: TDBLookupComboBox;
    DSrc: TDataSource;
    SQLQr: TSQLQuery;
  end;

  Tpr = procedure of Object;

  { TCardInsertForm }

  TCardInsertForm = class(TForm)
    BtnSave: TButton;
    BtnCancel: TButton;
    CardInsertGroupBox: TGroupBox;
    CardInsertSQLQuery: TSQLQuery;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    public
      CurrentTbl: TTableInfo;
      EditPanels: array of TEditPanel;
      CallB: procedure of Object;
      procedure PopUpForm(ATable: TTableInfo; ACallB: Tpr);
      procedure BuildEditor();
      function MakeQuery(): String;
      procedure BtnSaveEnDis(Sender: TObject);
  end;

var
  CardInsertForm: TCardInsertForm;

implementation

procedure TCardInsertForm.BtnCancelClick(Sender: TObject);
begin
  Self.Close;
end;

function TCardInsertForm.MakeQuery(): String;
var
  i: Integer;
  values: String;
begin
  for i := 1 to High(CurrentTbl.FFields) do
    values += Format(',:Val%d', [i]);
  delete(values, 1, 1);
  Result := Format('INSERT INTO %s VALUES(NEXT VALUE FOR %s,%s)', [CurrentTbl.FName, CurrentTbl.FSequence, values]);
end;

procedure TCardInsertForm.BtnSaveClick(Sender: TObject);
var
  i: Integer;
  empty: Boolean;
begin
  with CardInsertSQLQuery do begin
    Close;
    SQL.Clear;
    SQL.Text := MakeQuery;
    empty := False;
    for i := 1 to High(CurrentTbl.FFields) do
      if CurrentTbl.FFields[i].FForeignKeyTable <> '' then begin
        CardInsertSQLQuery.ParamByName(Format('Val%d', [i])).AsString :=
          EditPanels[i].CmbBox.ListSource.DataSet.FieldByName('id').AsString;
        if EditPanels[i].CmbBox.ItemIndex = -1 then
          empty := True;
      end
      else begin
        CardInsertSQLQuery.ParamByName(Format('Val%d', [i])).AsString :=
          EditPanels[i].Fld.Text;
        if EditPanels[i].Fld.Text = '' then
          empty := True;
      end;
    if empty then
      MessageDlg('Заполните все поля', mtCustom, [mbOK], 0)
    else begin
      ExecSQL;
      ConnectModule.SQLTransaction.Commit;
      BtnSave.Enabled := False;
      CallB;
    end;
  end;
end;

procedure TCardInsertForm.BuildEditor();
var
  i, preTop: Integer;
  refTbl: String;
begin
  SetLength(EditPanels, Length(CurrentTbl.FFields));
  preTop := 5;
  for i := 1 to High(CurrentTbl.FFields) do begin
    EditPanels[i].Lbl := TLabel.Create(nil);
    with EditPanels[i].Lbl do begin
      Width := 130;
      Height := 23;
      Top := preTop + 2;
      Left := 10;
      Parent := CardInsertGroupBox;
      Text := Format('%s:', [CurrentTbl.FFields[i].FCaption]);
    end;

    if CurrentTbl.FFields[i].FForeignKeyTable <> '' then begin
      EditPanels[i].SQLQr := TSQLQuery.Create(nil);
      with EditPanels[i].SQLQr do begin
        DataBase := ConnectModule.IBConnection;
        Transaction := ConnectModule.SQLTransaction;
      end;

      EditPanels[i].DSrc := TDataSource.Create(nil);
      EditPanels[i].DSrc.DataSet := EditPanels[i].SQLQr;
      with EditPanels[i].SQLQr do begin
        Close;
        SQL.Text := Format('SELECT ID, name FROM %s ORDER BY Name', [CurrentTbl.FFields[i].FForeignKeyTable]);
        Open;
      end;

      EditPanels[i].CmbBox := TDBLookupComboBox.Create(nil);
      with EditPanels[i].CmbBox do begin
        DataSource := EditPanels[i].DSrc;
        DataField := 'ID';
        ListSource := EditPanels[i].DSrc;
        ListField := 'name';
        KeyField := 'ID';
        Width := 170;
        Height := 23;
        Top := preTop;
        Left := 140;
        ItemIndex := -1;
        Style := csDropDownList;
        Parent := CardInsertGroupBox;
        OnChange := @BtnSaveEnDis;
      end;
    end else begin
      EditPanels[i].Fld := TEdit.Create(nil);
      with EditPanels[i].Fld do begin
        Width := 170;
        Height := 23;
        Top := preTop;
        Left := 140;
        Parent := CardInsertGroupBox;
        OnChange := @BtnSaveEnDis;
      end;
    end;
    preTop += 25;
  end;
end;

procedure TCardInsertForm.PopUpForm(ATable: TTableInfo; ACallB: Tpr);
begin
  Caption := Format('Вставить в таблицу "%s"', [ATable.FCaption]);
  CurrentTbl := ATable;
  CallB := ACallB;
  BuildEditor;
  ShowModal;
  BtnSave.Enabled := False;
end;

procedure TCardInsertForm.BtnSaveEnDis(Sender: TObject);
begin
  BtnSave.Enabled := True;
end;

{$R *.lfm}

end.


