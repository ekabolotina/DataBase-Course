unit Referen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, BufDataset, FileUtil, Forms, Controls, Graphics,
  Dialogs, DBGrids, StdCtrls, ExtCtrls, Connect, MetaUnit;

type

  { TReferenForm }

  TReferenForm = class(TForm)
    BtnAddFilter: TButton;
    FilterSubmit: TButton;
    Datasource: TDatasource;
    DBGrid: TDBGrid;
    FilterGroup: TGroupBox;
    SQLQuery: TSQLQuery;
    procedure BtnAddFilterClick(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure FilterSubmitClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PopupForm(ATable: TTableInfo);
    procedure ShowTable(ATable: TTableInfo; ADBGrid: TDBGrid; AQuery: String);
    procedure SwitchUpdateBtn(Sender: TObject);
    procedure RemoveFilter(Sender: TObject);
    procedure FilterEnDis(Sender: TObject);
  public
    ThisTable: TTableInfo;
    ThisQuery: String;
    FilterPanels: array of TPanel;
    FilterFieldsLists: array of TComboBox;
    FilterSignsLists: array of TComboBox;
    FilterConstraints: array of TEdit;
    FilterRemove: array of TButton;
    FilterType: TComboBox;
    FilterStrList: array of String;
    FilterCheckBoxes: array of TCheckBox;
    SortStatus: array of Integer;
    procedure MkPnl();
  end;

var
  ReferenForm: TReferenForm;

implementation

procedure TReferenForm.ShowTable(ATable: TTableInfo; ADBGrid: TDBGrid; AQuery: String);
var
  i: Integer;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := AQuery;
  SQLQuery.Open;
  for i := 0 to High(ATable.FFields) do begin
    ADBGrid.Columns[i].Title.Caption := ATable.FFields[i].FCaption;
    ADBGrid.Columns[i].Width := ATable.FFields[i].FWidth * 10;
  end;
end;

procedure TReferenForm.SwitchUpdateBtn(Sender: TObject);
begin
  FilterSubmit.Enabled := True;
end;

procedure TReferenForm.FilterEnDis(Sender: TObject);
var
  FTag: Integer;
begin
  FTag := (Sender as TCheckBox).Tag;
  FilterFieldsLists[FTag].Enabled :=
    not(FilterFieldsLists[FTag].Enabled);
  FilterSignsLists[FTag].Enabled :=
    not(FilterSignsLists[FTag].Enabled);
  FilterConstraints[FTag].Enabled :=
    not(FilterConstraints[FTag].Enabled);
  SwitchUpdateBtn(Sender);
end;

procedure TReferenForm.RemoveFilter(Sender: TObject);
begin
  FilterPanels[High(FilterPanels)].Free;
  SetLength(FilterPanels, High(FilterPanels));
  SetLength(FilterFieldsLists, Length(FilterPanels));
  SetLength(FilterSignsLists, Length(FilterPanels));
  SetLength(FilterConstraints, Length(FilterPanels));
  SetLength(FilterRemove, Length(FilterPanels));
  SetLength(FilterCheckBoxes, Length(FilterPanels));
  if Length(FilterPanels) <> 1 then
    FilterRemove[High(FilterRemove)].Visible := True;
  FilterGroup.Height := FilterGroup.Height - 40;
  DBGrid.Top := DBGrid.Top - 40;
  SwitchUpdateBtn(Sender);
end;

procedure TReferenForm.MkPnl();
begin
  SetLength(FilterPanels, Length(FilterPanels)+1);
  FilterPanels[High(FilterPanels)] := TPanel.Create(nil);
  with FilterPanels[High(FilterPanels)] do begin
    Width := 510;
    Height := 40;
    Top := High(FilterPanels) * 40;
    Left := 8;
    Parent := FilterGroup;
  end;

  SetLength(FilterCheckBoxes, Length(FilterCheckBoxes)+1);
  FilterCheckBoxes[High(FilterCheckBoxes)] := TCheckBox.Create(nil);
  with FilterCheckBoxes[High(FilterCheckBoxes)] do begin
    Top := 10;
    Left := 18;
    Checked := True;
    Tag := High(FilterCheckBoxes);
    OnChange := @FilterEnDis;
    Parent := FilterPanels[High(FilterPanels)];
  end;

  SetLength(FilterFieldsLists, Length(FilterFieldsLists)+1);
  FilterFieldsLists[High(FilterFieldsLists)] := TComboBox.Create(nil);
  with FilterFieldsLists[High(FilterFieldsLists)] do begin
    Width := 100;
    Height := 23;
    Top := 8;
    Left := 50;
    ReadOnly := True;
    Items.AddStrings(FilterStrList);
    ItemIndex := 0;
    OnChange := @SwitchUpdateBtn;
    Parent := FilterPanels[High(FilterPanels)];
  end;

  SetLength(FilterSignsLists, Length(FilterSignsLists)+1);
  FilterSignsLists[High(FilterSignsLists)] := TComboBox.Create(nil);
  with FilterSignsLists[High(FilterSignsLists)] do begin
    Width := 50;
    Height := 23;
    Top := 8;
    Left := 160;
    ReadOnly := True;
    Items.AddStrings(['>', '<', '=', '>=', '<=', '<>']);
    ItemIndex := 0;
    OnChange := @SwitchUpdateBtn;
    Parent := FilterPanels[High(FilterPanels)];
  end;

  SetLength(FilterConstraints, Length(FilterConstraints)+1);
  FilterConstraints[High(FilterConstraints)] := TEdit.Create(nil);
  with FilterConstraints[High(FilterConstraints)] do begin
    Width := 220;
    Height := 23;
    Top := 8;
    Left := 220;
    OnChange := @SwitchUpdateBtn;
    Parent := FilterPanels[High(FilterPanels)];
  end;

  SetLength(FilterRemove, Length(FilterRemove)+1);
  if Length(FilterPanels) <> 1 then begin
    FilterRemove[High(FilterRemove)] := TButton.Create(nil);
    with FilterRemove[High(FilterRemove)] do begin
      Width := 23;
      Height := 23;
      Top := 8;
      Left := 462;
      Caption := '-';
      OnClick := @RemoveFilter;
      Parent := FilterPanels[High(FilterPanels)];
    end;
    if Length(FilterPanels) <> 2 then
      FilterRemove[High(FilterRemove)-1].Visible := False;
  end else begin
    FilterType := TComboBox.Create(nil);
    with FilterType do begin
      Width := 50;
      Height := 23;
      Top := 8;
      Left := 450;
      ReadOnly := True;
      Items.AddStrings(['AND', 'OR']);
      ItemIndex := 0;
      OnChange := @SwitchUpdateBtn;
      Parent := FilterPanels[High(FilterPanels)];
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
  SetLength(SortStatus, Length(ATable.FFields));
  ReferenForm := TReferenForm.Create(nil);
  ShowTable(ATable, DBGrid, M.MakeQuery(ATable));
  SetLength(FilterStrList, Length(ATable.FFields));
  for i := 0 to High(ATable.FFields) do
    FilterStrList[i] := ATable.FFields[i].FCaption;
  Caption := ATable.FCaption;
  FilterGroup.Height := 20;
  DBGrid.Top := 40;
  MkPnl();
  Show;
end;

procedure TReferenForm.FormResize(Sender: TObject);
begin
  with DBGrid do begin
    Width := Self.Width;
    Height := Self.Height - 80;
  end;
end;

procedure TReferenForm.FilterSubmitClick(Sender: TObject);
var
  i, FiltersEnabled, counter: Integer;
  query, field, ftype, ending, beginning: String;
  M: TMeta;
begin
  for i := 0 to High(FilterPanels) do inc(FiltersEnabled);
  counter := -1;
  for i := 0 to High(FilterPanels) do begin
    if not(FilterCheckBoxes[i].Checked) then Continue;
    inc(counter);
    field := '';
    ftype := '';
    beginning := '';
    ending := '';
    with ThisTable.FFields[FilterFieldsLists[i].ItemIndex] do begin
      if FForeignKeyTable <> '' then
        field := FForeignKeyTable + '.Name'
      else
        field := FName;
    end;
    if counter <> 0 then
      ftype := FilterType.Text;
    if FiltersEnabled <> 1 then begin
      beginning := '(';
      ending := ')';
    end;
    query += ftype + beginning + field + FilterSignsLists[i].Text + chr(39) + FilterConstraints[i].Text + chr(39) + ending;
  end;
  if counter >= 0 then begin
    query := 'WHERE ' + query;
    ShowTable(ThisTable, DBGrid, M.MakeQuery(ThisTable, query));
    ThisQuery := query;
  end else begin
    ShowTable(ThisTable, DBGrid, M.MakeQuery(ThisTable, ''));
    ThisQuery := '';
  end;
  FilterSubmit.Enabled := False;
end;

procedure TReferenForm.BtnAddFilterClick(Sender: TObject);
begin
  MkPnl();
  SwitchUpdateBtn(Sender);
end;

procedure TReferenForm.DBGridTitleClick(Column: TColumn);
var
  field, query, sorttype: String;
  M: TMeta;
  pick: String;
  index: Integer;
begin
  index := Column.Index;
  with ThisTable.FFields[index] do begin
    if FForeignKeyTable <> '' then
      field := FForeignKeyTable + '.Name'
    else
      field := FName;
  end;
  sorttype := '';
  case SortStatus[index] of
    0, 2: begin
      SortStatus[index] := 1;
      sorttype := 'DESC';
      pick := '«';
    end;
    1: begin
      SortStatus[index] := 2;
      sorttype := 'ASC';
      pick := '»';
    end;
  end;
  query := ThisQuery + ' ORDER BY ' + field + ' ' + sorttype;
  ShowTable(ThisTable, DBGrid, M.MakeQuery(ThisTable, query));
  DBGrid.Columns[index].Title.Caption := ThisTable.FFields[index].FCaption + ' ' + pick;
end;


{$R *.lfm}

end.

