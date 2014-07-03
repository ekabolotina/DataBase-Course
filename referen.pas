unit Referen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, BufDataset, FileUtil, Forms, Controls, Graphics,
  Dialogs, DBGrids, StdCtrls, ExtCtrls, DbCtrls, Connect, MetaUnit, CardInsert,
  CardEdit, Filters;

type

  { TReferenForm }

  TReferenForm = class(TForm)
  type
    TFilterInfoArray = array of TFilterInfo;
  published
    BtnInsert: TButton;
    BtnEdit: TButton;
    BtnRemove: TButton;
    Datasource: TDatasource;
    DBGrid: TDBGrid;
    FilterGroup: TGroupBox;
    SQLQuery: TSQLQuery;
    procedure BtnEditClick(Sender: TObject);
    procedure BtnInsertClick(Sender: TObject);
    procedure BtnRemoveClick(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure PopupForm(ATable: TTableInfo; AFilterInfo: TFilterInfoArray = nil);
    procedure ShowTable;
  public
    ThisTable: TTableInfo;
    ThisQuery, DeleteConst: String;
    FilterType: TComboBox;
    SortStatus: array of (NONE, ASC, DESC);
    SortType, SortIcon: String;
    SortIndex: Integer;
    InsFrm: TCardInsertForm;
    F: TFilters;
    function MakeLocalQuery: String;
  end;

var
  ReferenForm: TReferenForm;

implementation

procedure TReferenForm.ShowTable;
var
  i: Integer;
  FSortIcon: String;
begin
  ThisQuery := TMeta.MakeQuery(ThisTable, MakeLocalQuery);
  SQLQuery.Close;
  SQLQuery.SQL.Text := ThisQuery;
  for i := 0 to High(F.FilterPanels) do begin
    with F.FilterPanels[i] do begin
      if not FCheckBox.Checked then Continue;
      SQLQuery.ParamByName(Format('FConst_Text%d', [i])).AsString := FConstraint.Text;
    end;
  end;
  SQLQuery.Open;
  for i := 0 to High(ThisTable.FFields) do begin
    if i = SortIndex then
      FSortIcon := ' ' + SortIcon
    else
      FSortIcon := '';
    DBGrid.Columns[i].Title.Caption := ThisTable.FFields[i].FCaption + FSortIcon;
    DBGrid.Columns[i].Width := ThisTable.FFields[i].FWidth * 10 + 5;
  end;
  DBGrid.Columns[0].Visible := False;
end;

procedure TReferenForm.PopupForm(ATable: TTableInfo; AFilterInfo: TFilterInfoArray);
var
  i: Integer;
begin
  ThisTable := ATable;
  DeleteConst := 'ID <> 0';
  SortIndex := -1;
  SetLength(SortStatus, Length(ATable.FFields));
  ReferenForm := TReferenForm.Create(nil);
  Caption := ATable.FCaption;
  DBGrid.Top := 40;
  F := TFilters.Create(ThisTable, FilterGroup, @ShowTable, 10, AFilterInfo);
  ShowTable;
  Show;
end;

function TReferenForm.MakeLocalQuery(): String;
var
  query, field: String;
begin
  query := '';
  if SortIndex <> -1 then begin
    with ThisTable.FFields[SortIndex] do begin
      if FForeignKeyTable <> '' then
        field := Format('%s.Name', [FForeignKeyTable])
      else
        field := Format('%s.%s', [ThisTable.FName, FName]);
    end;
    query += Format(' ORDER BY %s %s', [field, SortType]);
  end;
  query := Format('%s %s', [F.MakeQuery, query]);
  Result := query;
end;

procedure TReferenForm.DBGridTitleClick(Column: TColumn);
var
  M: TMeta;
begin
  if SortIndex <> Column.Index then
    SortStatus[SortIndex] := NONE;
  SortIndex := Column.Index;
  case SortStatus[SortIndex] of
    NONE: begin
      SortStatus[SortIndex] := DESC;
      SortType := 'ASC';
      SortIcon := '»';
    end;
    DESC: begin
      SortStatus[SortIndex] := ASC;
      SortType := 'DESC';
      SortIcon := '«';
    end;
    ASC: begin
      SortStatus[SortIndex] := NONE;
      SortType := '';
      SortIndex := -1;
      SortIcon := '';
    end;
  end;
  ShowTable;
  end;

procedure TReferenForm.BtnInsertClick(Sender: TObject);
begin
  TCardEditForm.ShowEditor(ThisTable, 0, False, @ShowTable);
end;

procedure TReferenForm.BtnEditClick(Sender: TObject);
var
  forEdit, i: Integer;
begin
  forEdit := Datasource.DataSet.FieldByName('ID').AsInteger;
  if forEdit = 0 then Exit;
  TCardEditForm.ShowEditor(ThisTable, forEdit, True, @ShowTable);
end;

procedure TReferenForm.BtnRemoveClick(Sender: TObject);
var
  forRemove, answer: Integer;
  M: TMeta;
begin
  forRemove := Datasource.DataSet.FieldByName('ID').AsInteger;
  if forRemove = 0 then Exit;
  answer := MessageDlg(Format('Вы действительно хотите удалить выбранную запись (ID %d)', [forRemove]), mtCustom, [mbYes, mbNo], 0);
  if answer = 6 then begin
    with SQLQuery do begin
      Close;
      SQL.Clear;
      SQL.Text := Format('DELETE FROM %s WHERE ID = %d', [ThisTable.FName, forRemove]);
      ExecSQL;
    end;
    ConnectModule.SQLTransaction.Commit;
    ShowTable;
  end;
end;

{$R *.lfm}

end.

