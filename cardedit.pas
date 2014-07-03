unit CardEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db,FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DbCtrls, ExtCtrls, Connect, MetaUnit;

type

  TEditPanel = record
    Fld: TDBEdit;
    Lbl: TLabel;
    CmbBox: TDBLookupComboBox;
    DSrc: TDataSource;
    SQLQr: TSQLQuery;
  end;

  TPointArr = array of TPoint;

  TRefreshTableProc = procedure of Object;

  { TCardEditForm }

  TCardEditForm = class(TForm)
    BtnCancel: TButton;
    BtnSave: TButton;
    CardEditGroupBox: TGroupBox;
    CardEditDataSource: TDatasource;
    CardEditSQLQuery: TSQLQuery;
    ProcessSQL: TSQLQuery;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  public
    CurrentTbl: TTableInfo;
    CurrentID: Integer;
    CurrentisEditor: Boolean;
    EditPanels: array of TEditPanel;
    DefIDs: TPointArr;
    RefreshTableProc: TRefreshTableProc;
    procedure BuildEditor;
    function MakeQuery(): String;
    procedure ClearTrash(AID: Integer);
    class procedure ShowEditor(ATable: TTableInfo; AID: Integer; isEditor: Boolean;
      ARefreshTableProc: TRefreshTableProc; ADefIDs: TPointArr = nil);
  end;

  TEditFrms = record
    Frm: TCardEditForm;
    ID: Integer;
  end;

var
  CardEditForm: array of TEditFrms;

implementation

class procedure TCardEditForm.ShowEditor(ATable: TTableInfo; AID: Integer;
  isEditor: Boolean; ARefreshTableProc: TRefreshTableProc; ADefIDs: TPointArr = nil);
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
    if isEditor then
      Caption := 'Редактирование записи'
    else
      Caption := 'Вставка новой записи';
    CurrentTbl := ATable;
    CurrentID := AID;
    CurrentisEditor := isEditor;
    RefreshTableProc := ARefreshTableProc;
    SetLength(DefIDs, Length(ADefIDs));
    for i := 0 to High(ADefIDs) do
      DefIDs[i] := ADefIDs[i];
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
  if CurrentisEditor then
    ClearTrash(CurrentID);
  Close;
end;

function TCardEditForm.MakeQuery(): String;
var
  i: Integer;
  values: String = '';
begin
  if CurrentisEditor then begin
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

procedure TCardEditForm.BuildEditor;
var
  i, preTop: Integer;
begin
  with CardEditSQLQuery do begin
    Close;
    SQL.Clear;
    SQL.Text := Format('SELECT * FROM %s WHERE ID = %d', [CurrentTbl.FName, CurrentID]);
    Open;
  end;

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
      EditPanels[i].Fld := nil;
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
        if CurrentisEditor then begin
          DataSource := CardEditDataSource;
          DataField := CurrentTbl.FFields[i].FName;
        end;
        ListSource := EditPanels[i].DSrc;
        ListField := 'Name';
        KeyField := 'ID';
        if DefIDs <> nil then begin
          if DefIDs[0].x = i then KeyValue := DefIDs[0].y;
          if DefIDs[1].x = i then KeyValue := DefIDs[1].y;
        end;
        NullValueKey := -1;
        Width := 170;
        Height := 23;
        Top := preTop;
        Left := 140;
        Style := csDropDownList;
        Parent := CardEditGroupBox;
      end;
    end else begin
      EditPanels[i].CmbBox := nil;
      EditPanels[i].Fld := TDBEdit.Create(nil);
      with EditPanels[i].Fld do begin
        Width := 170;
        Height := 23;
        Top := preTop;
        Left := 140;
        Parent := CardEditGroupBox;
        DataSource := CardEditDataSource;
        DataField := CurrentTbl.FFields[i].FName;
        if i = 0 then begin
          Enabled := False;
          if not CurrentisEditor then
            Text := 'Автоинкремент';
        end;
      end;
    end;
    preTop += 25;
  end;
end;

procedure TCardEditForm.BtnSaveClick(Sender: TObject);
var
  i: Integer;
begin
  with ProcessSQL do begin
    Close;
    SQL.Clear;
    SQL.Text := MakeQuery;
    for i := 1 to High(CurrentTbl.FFields) do
      with EditPanels[i] do begin
        if ((CmbBox <> nil) and (CmbBox.ItemIndex = -1)) or((Fld <> nil) and (Fld.Text = '')) then begin
          MessageDlg('Заполните все поля', mtCustom, [mbOK], 0);
          Exit;
        end;
        if CurrentTbl.FFields[i].FForeignKeyTable <> '' then
          ParamByName(Format('Val%d', [i])).AsString :=
            CmbBox.KeyValue
        else
          ParamByName(Format('Val%d', [i])).AsString :=
            Fld.Text;
      end;
    ExecSQL;
    ConnectModule.SQLTransaction.Commit;
    RefreshTableProc;
    Self.Close;
  end;
end;

procedure TCardEditForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  ClearTrash(CurrentID);
end;


{$R *.lfm}

end.

