unit CardEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db,FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DbCtrls, ExtCtrls, DBGrids, Connect, MetaUnit;

type

  TEditPanel = record
    Fld: TEdit;
    Lbl: TLabel;
    CmbBox: TDBLookupComboBox;
    DSrc: TDataSource;
    SQLQr: TSQLQuery;
  end;

  TProcedure = procedure of Object;
  TParamProcedure = procedure(AID: Integer) of Object;

  { TCardEditForm }

  TCardEditForm = class(TForm)
    BtnCancel: TButton;
    BtnSave: TButton;
    CardEditGroupBox: TGroupBox;
    CardEditDataSource: TDatasource;
    CardEditSQLQuery: TSQLQuery;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  public
    CurrentTbl: TTableInfo;
    CurrentID: Integer;
    CurrentFieldsVals: array of String;
    EditPanels: array of TEditPanel;
    CurrentIDs: array of Integer;
    CallBRefreshTbl: TProcedure;
    CallBClearTrash: TParamProcedure;
    procedure PopUpForm(
      ATable: TTableInfo; AID: Integer; AFieldsVals: array of String;
      ACallB: TProcedure; AClear: TParamProcedure);
    procedure BuildEditor;
    procedure CollectIDs;
    procedure BtnSaveEnDis(Sender: TObject);
    function MakeQuery(): String;
  end;

implementation

procedure TCardEditForm.BtnCancelClick(Sender: TObject);
begin
  CallBClearTrash(CurrentID);
  Close;
end;

procedure TCardEditForm.BtnSaveEnDis(Sender: TObject);
begin
  BtnSave.Enabled := True;
end;

function TCardEditForm.MakeQuery(): String;
var
  i: Integer;
  values: String;
begin
  for i := 1 to High(CurrentTbl.FFields) do
    values += Format(',%s = :Val%d', [CurrentTbl.FFields[i].FName, i]);
  delete(values, 1, 1);
  Result := Format('UPDATE %s SET %s WHERE ID = %d', [CurrentTbl.FName, values, CurrentID]);
end;

procedure TCardEditForm.CollectIDs;
var
  i: Integer;
begin
  with CardEditSQLQuery do begin
    Close;
    SQL.Clear;
    SQL.Text := Format('SELECT * FROM %s WHERE ID = %d', [CurrentTbl.FName, CurrentID]);
    Open;
  end;
  SetLength(CurrentIDs, Length(CurrentTbl.FFields));
  for i := 0 to High(CurrentIDs) do begin
    if CurrentTbl.FFields[i].FForeignKeyTable <> '' then
      CurrentIDs[i] := CardEditDataSource.DataSet.FieldByName(CurrentTbl.FFields[i].FName).AsInteger
    else
      CurrentIDs[i] := -1;
  end;
end;

procedure TCardEditForm.BuildEditor;
var
  i, preTop: Integer;
  refTbl: String;
begin
  CollectIDs;
  SetLength(EditPanels, Length(CurrentTbl.FFields));
  preTop := 5;
  for i := 0 to High(CurrentTbl.FFields) do begin
    EditPanels[i].Lbl := TLabel.Create(nil);
    with EditPanels[i].Lbl do begin
      Width := 130;
      Height := 23;
      Top := preTop + 2;
      Left := 10;
      Parent := CardEditGroupBox;
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
        KeyValue := CurrentIDs[i];
        Style := csDropDownList;
        Parent := CardEditGroupBox;
        OnChange := @BtnSaveEnDis;
      end;
    end else begin
      EditPanels[i].Fld := TEdit.Create(nil);
      with EditPanels[i].Fld do begin
        Width := 170;
        Height := 23;
        Top := preTop;
        Left := 140;
        Parent := CardEditGroupBox;
        OnChange := @BtnSaveEnDis;
        if i = 0 then begin
          Enabled := False;
          Text := IntToStr(CurrentID);
        end else
          Text := CurrentFieldsVals[i];
      end;
    end;
    preTop += 25;
  end;
end;

procedure TCardEditForm.BtnSaveClick(Sender: TObject);
var
  i: Integer;
  empty: Boolean;
begin
  //CardEditSQLQuery.Edit;
  //CardEditSQLQuery.Post;
  //CardEditSQLQuery.ApplyUpdates;
  with CardEditSQLQuery do begin
    Close;
    SQL.Clear;
    SQL.Text := MakeQuery;
    empty := False;
    for i := 1 to High(CurrentTbl.FFields) do
      if CurrentTbl.FFields[i].FForeignKeyTable <> '' then begin
        CardEditSQLQuery.ParamByName(Format('Val%d', [i])).AsString :=
          EditPanels[i].CmbBox.ListSource.DataSet.FieldByName('id').AsString;
        if EditPanels[i].CmbBox.ItemIndex = -1 then
          empty := True;
      end
      else begin
        CardEditSQLQuery.ParamByName(Format('Val%d', [i])).AsString :=
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
      CallBRefreshTbl;
    end;
  end;
end;

procedure TCardEditForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CallBClearTrash(CurrentID);
end;

procedure TCardEditForm.PopUpForm(ATable: TTableInfo; AID: Integer;
  AFieldsVals: array of String; ACallB: TProcedure; AClear: TParamProcedure);
var
  i: Integer;
begin
  Show;
  Caption := 'Редактирование записи';
  CurrentTbl := ATable;
  CurrentID := AID;
  CallBRefreshTbl := ACallB;
  CallBClearTrash := AClear;
  SetLength(CurrentFieldsVals, Length(AFieldsVals));
  for i := 1 to High(AFieldsVals) do
    CurrentFieldsVals[i] := AFieldsVals[i];
  BuildEditor;
  BtnSave.Enabled := False;
end;

{$R *.lfm}

end.

