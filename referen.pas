unit Referen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, BufDataset, FileUtil, Forms, Controls, Graphics,
  Dialogs, DBGrids, StdCtrls, ExtCtrls, DbCtrls, Connect, MetaUnit, CardInsert, CardEdit;

type

  { TReferenForm }

  TFilters = record
    FPanel: TPanel;
    FFieldsList: TComboBox;
    FSignsList: TComboBox;
    FConstraint: TEdit;
    FRemove: TButton;
    FCheckBox: TCheckBox;
  end;

  TEditFrms = record
    Frm: TCardEditForm;
    ID: Integer;
  end;

  TSortType = (NONE, DESC, ASC);

  TReferenForm = class(TForm)
    BtnAddFilter: TButton;
    BtnInsert: TButton;
    BtnEdit: TButton;
    BtnRemove: TButton;
    FilterSubmit: TButton;
    Datasource: TDatasource;
    DBGrid: TDBGrid;
    FilterGroup: TGroupBox;
    ScrollBar: TScrollBar;
    SQLQuery: TSQLQuery;
    procedure BtnAddFilterClick(Sender: TObject);
    procedure BtnEditClick(Sender: TObject);
    procedure BtnInsertClick(Sender: TObject);
    procedure BtnRemoveClick(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure FilterSubmitClick(Sender: TObject);
    //procedure FormResize(Sender: TObject);
    procedure PopupForm(ATable: TTableInfo);
    procedure SwitchUpdateBtn(Sender: TObject);
    procedure RemoveFilter(Sender: TObject);
    procedure FilterEnDis(Sender: TObject);
  private
    procedure ShowTable(ATable: TTableInfo; ADBGrid: TDBGrid; AQuery: String);
  public
    ThisTable: TTableInfo;
    ThisQuery, DeleteConst: String;
    FilterType: TComboBox;
    SortStatus: array of TSortType;
    SortType, SortIcon: String;
    SortIndex: Integer;
    FilterStrList: array of String;
    FilterPanels: array of TFilters;
    InsFrm: TCardInsertForm;
    CardEditForm: array of TEditFrms;
    procedure MkPnl();
    function MakeLocalQuery(): String;
    procedure RefreshTable;
    procedure ClearTrash(AID: Integer);
  end;

var
  ReferenForm: TReferenForm;

implementation

procedure TReferenForm.ShowTable(ATable: TTableInfo; ADBGrid: TDBGrid; AQuery: String);
var
  i: Integer;
  FSortIcon: String;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := AQuery;
  for i := 0 to High(FilterPanels) do begin
    with FilterPanels[i] do begin
      if not FCheckBox.Checked then Continue;
      SQLQuery.ParamByName(Format('FConst_Text%d', [i])).AsString := FConstraint.Text;
    end;
  end;
  SQLQuery.Open;
  for i := 0 to High(ATable.FFields) do begin
    if i = SortIndex then
      FSortIcon := ' ' + SortIcon
    else
      FSortIcon := '';
    ADBGrid.Columns[i].Title.Caption := ATable.FFields[i].FCaption + FSortIcon;
    ADBGrid.Columns[i].Width := ATable.FFields[i].FWidth * 10 + 5;
  end;
  ADBGrid.Columns[0].Visible := False;
end;

procedure TReferenForm.RefreshTable();
var
  M: TMeta;
begin
  ShowTable(ThisTable, DBGrid, M.MakeQuery(ThisTable, MakeLocalQuery));
end;

procedure TReferenForm.ClearTrash(AID: Integer);
var
  i: Integer;
begin
  for i := 0 to High(CardEditForm) do
    if (CardEditForm[i].ID = AID) and (CardEditForm[i].Frm <> nil) then begin
      CardEditForm[i].Frm := nil;
      Break;
    end;
end;

procedure TReferenForm.SwitchUpdateBtn(Sender: TObject);
begin
  FilterSubmit.Enabled := True;
end;

procedure TReferenForm.FilterEnDis(Sender: TObject);
var
  FTag: Integer;
  FStatus: Boolean;
begin
  FTag := (Sender as TCheckBox).Tag;
  FStatus := not FilterPanels[FTag].FFieldsList.Enabled;
  with FilterPanels[FTag] do begin
    FFieldsList.Enabled := FStatus;
    FSignsList.Enabled := FStatus;
    FConstraint.Enabled := FStatus;
  end;
  SwitchUpdateBtn(Sender);
end;

procedure TReferenForm.RemoveFilter(Sender: TObject);
begin
  FilterPanels[High(FilterPanels)].FPanel.Free;
  SetLength(FilterPanels, Length(FilterPanels)-1);
  if Length(FilterPanels) <> 1 then
    FilterPanels[High(FilterPanels)].FRemove.Visible := True;
  FilterGroup.Height := FilterGroup.Height - 40;
  DBGrid.Top := DBGrid.Top - 40;
  SwitchUpdateBtn(Sender);
end;

procedure TReferenForm.MkPnl();
begin
  SetLength(FilterPanels, Length(FilterPanels)+1);

  with FilterPanels[High(FilterPanels)] do begin
    FPanel := TPanel.Create(nil);
    with FPanel do begin
      Width := 510;
      Height := 40;
      Top := High(FilterPanels) * 40;
      Left := 8;
      Parent := Self.FilterGroup;
    end;

    FCheckBox := TCheckBox.Create(nil);
    with FCheckBox do begin
      Top := 10;
      Left := 18;
      Checked := True;
      Tag := High(FilterPanels);
      OnChange := @FilterEnDis;
      Parent := FPanel;
    end;

    FFieldsList := TComboBox.Create(nil);
    with FFieldsList do begin
      Width := 100;
      Height := 23;
      Top := 8;
      Left := 50;
      Style := csDropDownList;
      Items.AddStrings(FilterStrList);
      Items.Delete(0);
      ItemIndex := 0;
      OnChange := @SwitchUpdateBtn;
      Parent := FPanel;
    end;

    FSignsList := TComboBox.Create(nil);
    with FSignsList do begin
      Width := 50;
      Height := 23;
      Top := 8;
      Left := 160;
      Style := csDropDownList;
      Items.AddStrings(['>', '<', '=', '>=', '<=', '<>']);
      ItemIndex := 0;
      OnChange := @SwitchUpdateBtn;
      Parent := FPanel;
    end;

    FConstraint := TEdit.Create(nil);
    with FConstraint do begin
      Width := 220;
      Height := 23;
      Top := 8;
      Left := 220;
      OnChange := @SwitchUpdateBtn;
      Parent := FPanel;
    end;

    if Length(FilterPanels) <> 1 then begin
      FRemove := TButton.Create(nil);
      with FRemove do begin
        Width := 23;
        Height := 23;
        Top := 8;
        Left := 462;
        Caption := '-';
        OnClick := @RemoveFilter;
        Parent := FPanel;
      end;
      if Length(FilterPanels) <> 2 then
        FilterPanels[High(FilterPanels)-1].FRemove.Visible := False;
    end else begin
      Self.FilterType := TComboBox.Create(nil);
      with Self.FilterType do begin
        Width := 50;
        Height := 23;
        Top := 8;
        Left := 450;
        Style := csDropDownList;
        Items.AddStrings(['AND', 'OR']);
        ItemIndex := 0;
        OnChange := @SwitchUpdateBtn;
        Parent := FPanel;
      end;
    end;
  end;

  FilterGroup.Height := FilterGroup.Height + 40;
  DBGrid.Top := DBGrid.Top + 40;
end;

procedure TReferenForm.PopupForm(ATable: TTableInfo);
var
  i: Integer;
  M: TMeta;
begin
  ThisTable := ATable;
  DeleteConst := 'ID <> 0';
  SortIndex := -1;
  SetLength(SortStatus, Length(ATable.FFields));
  ReferenForm := TReferenForm.Create(nil);
  ShowTable(ATable, DBGrid, TMeta.MakeQuery(ATable));
  SetLength(FilterStrList, Length(ATable.FFields));
  for i := 0 to High(FilterStrList) do
    FilterStrList[i] := ATable.FFields[i].FCaption;
  Caption := ATable.FCaption;
  FilterGroup.Height := 20;
  DBGrid.Top := 40;
  MkPnl();
  Show;
end;

//procedure TReferenForm.FormResize(Sender: TObject);
//begin
//  with DBGrid do begin
//    Width := Self.Width;
//    Height := Self.Height - 80;
//  end;
//end;

function TReferenForm.MakeLocalQuery(): String;
const
  SORT_SQL: array [TSortType] of String = ('', ' ASC', ' DESC');
var
  i, counter: Integer;
  query, field, ftype: String;
  M: TMeta;
begin
  counter := 0;
  query := '';
  for i := 0 to High(FilterPanels) do begin
    with FilterPanels[i] do begin
      if not FCheckBox.Checked then Continue;
      field := '';
      ftype := '';
      with ThisTable.FFields[FFieldsList.ItemIndex+1] do begin
        if FForeignKeyTable <> '' then
          field := Format('%s.Name', [FForeignKeyTable])
        else
          field := Format('%s.%s', [ThisTable.FName, FName]);
      end;
      if counter <> 0 then
        ftype := FilterType.Text;
      query += Format(' %s %s %s :FConst_Text%d', [ftype, field, FSignsList.Text, i]);
    end;
    inc(counter);
  end;
  if counter > 0 then
    query := 'WHERE ' + query;

  if SortIndex <> -1 then begin
    with ThisTable.FFields[SortIndex] do begin
      if FForeignKeyTable <> '' then
        field := Format('%s.Name', [FForeignKeyTable])
      else
        field := Format('%s.%s', [ThisTable.FName, FName]);
    end;
    query += Format(' ORDER BY %s %s', [field, SORT_SQL[SortStatus[SortIndex]]]);
  end;
  Result := query;
end;

procedure TReferenForm.FilterSubmitClick(Sender: TObject);
begin
  ShowTable(ThisTable, DBGrid, TMeta.MakeQuery(ThisTable, MakeLocalQuery));
  FilterSubmit.Enabled := False;
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
  ShowTable(ThisTable, DBGrid, M.MakeQuery(ThisTable, MakeLocalQuery));
end;

procedure TReferenForm.BtnAddFilterClick(Sender: TObject);
begin
  MkPnl();
  SwitchUpdateBtn(Sender);
end;

procedure TReferenForm.BtnInsertClick(Sender: TObject);
begin
  CardInsertForm := TCardInsertForm.Create(nil);
  CardInsertForm.PopUpForm(ThisTable, @RefreshTable);
end;

procedure TReferenForm.BtnEditClick(Sender: TObject);
var
  forEdit, i: Integer;
  FieldsVals: array of String;
begin
  forEdit := Datasource.DataSet.FieldByName('ID').AsInteger;
  for i := 0 to High(CardEditForm) do
    if (CardEditForm[i].Frm.CurrentID = forEdit) and (CardEditForm[i].Frm <> nil) then begin
      CardEditForm[i].Frm.BringToFront;
      Exit;
    end;
  SetLength(FieldsVals, Length(ThisTable.FFields));
  for i := 1 to High(ThisTable.FFields) do
    FieldsVals[i] := Datasource.DataSet.Fields[i].AsString;
  SetLength(CardEditForm, Length(CardEditForm)+1);
  with CardEditForm[High(CardEditForm)] do begin
    Frm := TCardEditForm.Create(nil);
    //ID := forEdit;
    Frm.PopUpForm(ThisTable, forEdit, FieldsVals, @RefreshTable, @ClearTrash);
  end;
end;

procedure TReferenForm.BtnRemoveClick(Sender: TObject);
var
  forRemove, answer: Integer;
  M: TMeta;
begin
  forRemove := Datasource.DataSet.FieldByName('ID').AsInteger;
  answer := MessageDlg(Format('Вы действительно хотите удалить выбранную запись (ID %d)', [forRemove]), mtCustom, [mbYes, mbNo], 0);
  if answer = 6 then begin
    with SQLQuery do begin
      Close;
      SQL.Clear;
      SQL.Text := Format('DELETE FROM %s WHERE ID = %d', [ThisTable.FName, forRemove]);
      ExecSQL;
    end;
    ConnectModule.SQLTransaction.Commit;
    RefreshTable;
  end;
end;


{$R *.lfm}

end.

