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

  TRefreshTableProc = procedure of Object;

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
    CurrentisEditorEnabled: Boolean;
    EditPanels: array of TEditPanel;
    CurrentIDs: array of Integer;
    RefreshTableProc: TRefreshTableProc;
    procedure BuildEditor;
    procedure CollectIDs;
    procedure BtnSaveEnDis(Sender: TObject);
    function MakeQuery(): String;
    function GetFieldVal(AField: String): String;
    procedure ClearTrash(AID: Integer);
    class procedure ShowEditor(ATable: TTableInfo; AID: Integer; isEditorEnabled: Boolean;
      ARefreshTableProc: TRefreshTableProc);
  end;

  TEditFrms = record
    Frm: TCardEditForm;
    ID: Integer;
  end;

var
  CardEditForm: array of TEditFrms;

implementation

class procedure TCardEditForm.ShowEditor(ATable: TTableInfo; AID: Integer;
  isEditorEnabled: Boolean; ARefreshTableProc: TRefreshTableProc);
var
  i: Integer;
begin
  for i := 0 to High(CardEditForm) do
    if CardEditForm[i].ID = AID then begin
      CardEditForm[i].Frm.BringToFront;
      Exit;
    end;

  SetLength(CardEditForm, Length(CardEditForm)+1);
  with CardEditForm[High(CardEditForm)] do begin
    Frm := TCardEditForm.Create(nil);
    ID := AID;
  end;
  with CardEditForm[High(CardEditForm)].Frm do begin
    Show;
    if isEditorEnabled then
      Caption := 'Редактирование записи'
    else
      Caption := 'Вставка новой записи';
    CurrentTbl := ATable;
    CurrentID := AID;
    CurrentisEditorEnabled := isEditorEnabled;
    RefreshTableProc := ARefreshTableProc;
    BuildEditor;
  end;
end;

procedure TCardEditForm.ClearTrash(AID: Integer);
var
  i: Integer;
  shift: Boolean;
begin
  shift := False;
  for i := 0 to High(CardEditForm) do
    if shift then begin
      CardEditForm[i-1].Frm := CardEditForm[i].Frm;
      CardEditForm[i-1].ID := CardEditForm[i].ID;
    end else if CardEditForm[i].ID = AID then
      shift := True;
  if shift then
    SetLength(CardEditForm, High(CardEditForm));
end;

procedure TCardEditForm.BtnCancelClick(Sender: TObject);
begin
  if CurrentisEditorEnabled then
    ClearTrash(CurrentID);
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
  if CurrentisEditorEnabled then begin
    for i := 1 to High(CurrentTbl.FFields) do
      values += Format(',%s = :Val%d', [CurrentTbl.FFields[i].FName, i]);
    delete(values, 1, 1);
    Result := Format('UPDATE %s SET %s WHERE ID = %d', [CurrentTbl.FName, values, CurrentID]);
  end else begin
    for i := 1 to High(CurrentTbl.FFields) do
       values += Format(',:Val%d', [i]);
    delete(values, 1, 1);
    Result := Format('INSERT INTO %s VALUES(NEXT VALUE FOR %s,%s)', [CurrentTbl.FName, CurrentTbl.FSequence, values]);
  end;
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
      CurrentIDs[i] := CardEditSQLQuery.FieldByName(CurrentTbl.FFields[i].FName).AsInteger
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
        SQL.Text := Format('SELECT ID, Name FROM %s ORDER BY Name', [CurrentTbl.FFields[i].FForeignKeyTable]);
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
        if CurrentisEditorEnabled then KeyValue := CurrentIDs[i];
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
        end else if CurrentisEditorEnabled then
          Text := GetFieldVal(CurrentTbl.FFields[i].FName);
      end;
    end;
    preTop += 25;
  end;
end;

function TCardEditForm.GetFieldVal(AField: String): String;
begin
  with CardEditSQLQuery do begin
    Close;
    SQL.Clear;
    SQL.Text := Format('SELECT %s FROM %s WHERE ID = %d', [AField, CurrentTbl.FName, CurrentID]);
    Open;
    Result := FieldByName(AField).AsString;
  end;
end;

procedure TCardEditForm.BtnSaveClick(Sender: TObject);
var
  i: Integer;
  empty: Boolean;
begin
  with CardEditSQLQuery do begin
    Close;
    SQL.Clear;
    SQL.Text := MakeQuery;
    empty := False;
    for i := 1 to High(CurrentTbl.FFields) do
      if CurrentTbl.FFields[i].FForeignKeyTable <> '' then begin
        CardEditSQLQuery.ParamByName(Format('Val%d', [i])).AsString :=
          EditPanels[i].CmbBox.KeyValue;
        if EditPanels[i].CmbBox.KeyValue = -1 then
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
      ShowMessage(SQL.Text);
      ExecSQL;
      ConnectModule.SQLTransaction.Commit;
      BtnSave.Enabled := False;
      RefreshTableProc;
      BtnCancel.Click;
    end;
  end;
end;

procedure TCardEditForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  ClearTrash(CurrentID);
end;


{$R *.lfm}

end.

