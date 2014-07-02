unit Referen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, BufDataset, FileUtil, Forms, Controls, Graphics,
  Dialogs, DBGrids, StdCtrls, ExtCtrls, DbCtrls, Connect, MetaUnit, CardInsert;

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

  TReferenForm = class(TForm)
    BtnAddFilter: TButton;
    BtnInsert: TButton;
    BtnEdit: TButton;
    BtnRemove: TButton;
    FilterSubmit: TButton;
    Datasource: TDatasource;
    DBGrid: TDBGrid;
    FilterGroup: TGroupBox;
    SQLQuery: TSQLQuery;
    procedure BtnAddFilterClick(Sender: TObject);
    procedure BtnInsertClick(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure FilterSubmitClick(Sender: TObject);
    //procedure FormResize(Sender: TObject);
    procedure PopupForm(ATable: TTableInfo);
    procedure ShowTable(ATable: TTableInfo; ADBGrid: TDBGrid; AQuery: String);
    procedure SwitchUpdateBtn(Sender: TObject);
    procedure RemoveFilter(Sender: TObject);
    procedure FilterEnDis(Sender: TObject);
  public
    ThisTable: TTableInfo;
    ThisQuery: String;
    FilterType: TComboBox;
    SortStatus: array of (NONE, DESC, ASC);
    SortType, SortIcon: String;
    SortIndex: Integer;
    FilterStrList: array of String;
    FilterPanels: array of TFilters;
    InsFrm: TCardInsertForm;
    procedure MkPnl();
    function MakeLocalQuery(): String;
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
      SQLQuery.ParamByName('FConst_Text' + IntToStr(i)).AsString := FConstraint.Text;
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
      ReadOnly := True;
      Items.AddStrings(FilterStrList);
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
      ReadOnly := True;
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
        ReadOnly := True;
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
  SortIndex := -1;
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

//procedure TReferenForm.FormResize(Sender: TObject);
//begin
//  with DBGrid do begin
//    Width := Self.Width;
//    Height := Self.Height - 80;
//  end;
//end;

function TReferenForm.MakeLocalQuery(): String;
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
      with ThisTable.FFields[FFieldsList.ItemIndex] do begin
        if FForeignKeyTable <> '' then
          field := FForeignKeyTable + '.Name'
        else
          field := FName;
      end;
      if counter <> 0 then
        ftype := FilterType.Text;
      query += Format(' %s %s %s :FConst_Text%d', [ftype, field, FSignsList.Text, i]);
    end;
    inc(counter);
  end;
  if counter > 0 then
    query := 'WHERE ' + query;

  if SortIndex <> -1 then begin;
    with ThisTable.FFields[SortIndex] do begin
      if FForeignKeyTable <> '' then
        field := FForeignKeyTable + '.Name'
      else
        field := FName;
    end;
    query += Format(' ORDER BY %s %s', [field, SortType]);
  end;
  Result := query;
end;

procedure TReferenForm.FilterSubmitClick(Sender: TObject);
var
  M: TMeta;
begin
  ShowTable(ThisTable, DBGrid, M.MakeQuery(ThisTable, MakeLocalQuery));
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
  InsFrm.PopUpForm(ThisTable);
end;


{$R *.lfm}

end.

