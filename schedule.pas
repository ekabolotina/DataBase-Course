unit Schedule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Grids, MetaUnit, Connect;

type

  THorV = (H, V);

  { TFormSchedule }

  TFormSchedule = class(TForm)
    BtnSubmit: TButton;
    ComboBoxH: TComboBox;
    ComboBoxV: TComboBox;
    ScheduleDatasource: TDatasource;
    GroupBox: TGroupBox;
    LabelH: TLabel;
    LabelV: TLabel;
    ScheduleSQLQuery: TSQLQuery;
    StringGrid: TStringGrid;
    procedure BtnSubmitClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    ScheduleTable: TTableInfo;
    TablesList: array of String;
    HVals, VVals: array of String;
    procedure PopUpForm;
    procedure FillHeader(ATable: TTableInfo; HorV: THorV);
    procedure FillItems(AFieldH, AFieldV: Integer);
  end;

var
  FormSchedule: TFormSchedule;

implementation

procedure TFormSchedule.FormShow(Sender: TObject);
var i:integer;
begin
  for i := 0 to High(Tables) do begin
    ComboBoxH.Items.Add(Tables[i].FCaption);
    ComboBoxV.Items.Add(Tables[i].FCaption);
  end;
  ComboBoxH.ItemIndex := 0;
  ComboBoxV.ItemIndex := 0;
  ScheduleTable := TMeta.GetTableByName('Schedule_Items');
  StringGrid.Canvas.TextRect(StringGrid.CellRect(0,0), 0, 0, 'simpletext');
end;

procedure TFormSchedule.FillHeader(ATable: TTableInfo; HorV: THorV);
var
  i: Integer;
begin
  i := 1;
  with ScheduleSQLQuery do begin
    Close;
    SQL.Clear;
    SQL.Text := Format('SELECT Name FROM %s ORDER BY %s', [ATable.FName, ATable.FSortField]);
    Open;
    while not EOF do begin
      if HorV = H then begin
        StringGrid.ColCount := i+1;
        SetLength(HVals, i+1);
        StringGrid.Cells[i, 0] := FieldByName('Name').AsString;
        HVals[i] := FieldByName('Name').AsString;
      end else begin
        StringGrid.RowCount := i+1;
        SetLength(VVals, i+1);
        StringGrid.Cells[0, i] := FieldByName('Name').AsString;
        VVals[i] := FieldByName('Name').AsString;
      end;
      Inc(i);
      Next;
    end;
  end;
end;

procedure TFormSchedule.FillItems(AFieldH, AFieldV: Integer);
var
  i, j, X, Y: Integer;
begin
  X := StringGrid.ColCount - 1;
  Y := StringGrid.RowCount - 1;
  with ScheduleSQLQuery do begin
    Close;
    SQL.Clear;
    SQL.Text := TMeta.MakeQuery(ScheduleTable);
    Open;
    while not EOF do begin
      for i := 1 to X do
        for j := 1 to Y do
          if (Fields[AFieldH].AsString = HVals[i]) and (Fields[AFieldV].AsString = VVals[j]) then
            StringGrid.Cells[i, j] := Format('%s %d', [StringGrid.Cells[i, j], Fields[0].AsInteger]);
      Next;
    end;
  end;
end;

procedure TFormSchedule.BtnSubmitClick(Sender: TObject);
var
  i, j, FieldH, FieldV: Integer;
  TableH, TableV: TTableInfo;
begin
  TableH := Tables[ComboBoxH.ItemIndex];
  TableV := Tables[ComboBoxV.ItemIndex];
  FillHeader(TableH, H);
  FillHeader(TableV, V);
  StringGrid.RowHeights[0] := 23;

  for i := 1 to High(ScheduleTable.FFields) do
    with ScheduleTable.FFields[i] do begin
      if FForeignKeyTable = TableH.FName then
        FieldH := i;
      if FForeignKeyTable = TableV.FName then
        FieldV := i;
    end;

  FillItems(FieldH, FieldV);
end;

procedure TFormSchedule.PopUpForm;
var
  i: Integer;
begin
  FormSchedule := TFormSchedule.Create(nil);
  FormSchedule.ShowModal;
end;

{$R *.lfm}

end.

