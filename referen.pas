unit Referen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, BufDataset, FileUtil, Forms, Controls, Graphics,
  Dialogs, DBGrids, StdCtrls, Connect, MetaUnit;

type

  { TReferenForm }

  TReferenForm = class(TForm)
    FilterSubmit: TButton;
    FilterConstraint: TEdit;
    FilterSignsList: TComboBox;
    FilterFieldsList: TComboBox;
    Datasource: TDatasource;
    DBGrid: TDBGrid;
    SQLQuery: TSQLQuery;
    procedure FilterSubmitClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PopupForm(ATable: TTableInfo);
  public
    ThisTable: TTableInfo;
  end;

var
  ReferenForm: TReferenForm;

implementation

procedure TReferenForm.PopupForm(ATable: TTableInfo);
var
  i: Integer;
  M: TMeta;
begin
  ThisTable := ATable;
  ReferenForm := TReferenForm.Create(nil);
  SQLQuery.SQL.Text := M.MakeQuery(ATable);
  SQLQuery.Open;
  for i := 0 to High(ATable.FFields) do begin
    DBGrid.Columns[i].Title.Caption := ATable.FFields[i].FCaption;
    DBGrid.Columns[i].Width := ATable.FFields[i].FWidth * 10;
    FilterFieldsList.Items.Add(ATable.FFields[i].FCaption);
  end;
  FilterFieldsList.ItemIndex := 0;
  Caption := ATable.FCaption;
  Show;
end;

procedure TReferenForm.FormResize(Sender: TObject);
begin
  with DBGrid do begin
    Width := Self.Width;
    Height := Self.Height - 80;
    Top := 80;
  end;
end;

procedure TReferenForm.FilterSubmitClick(Sender: TObject);
var
  query, field: String;
  M: TMeta;
  i: Integer;
begin
  with ThisTable.FFields[FilterFieldsList.ItemIndex] do begin
    if FForeignKeyTable <> '' then
      field := FForeignKeyTable + '.Name'
    else
      field := FName;
  end;
  query := field + FilterSignsList.Text + chr(39) + FilterConstraint.Text + chr(39);
  SQLQuery.Close;
  SQLQuery.SQL.Text := M.MakeQuery(ThisTable, query);
  SQLQuery.Open;
  for i := 0 to High(ThisTable.FFields) do begin
    DBGrid.Columns[i].Title.Caption := ThisTable.FFields[i].FCaption;
    DBGrid.Columns[i].Width := ThisTable.FFields[i].FWidth * 10;
  end;
end;

{$R *.lfm}

end.

