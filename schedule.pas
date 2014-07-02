unit Schedule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Grids, ExtCtrls, Buttons, MetaUnit, Connect, Filters, windows;

type

  THorV = (H, V);

  { TFormSchedule }

  TFormSchedule = class(TForm)
    BtnSubmit: TButton;
    BtnSwitch: TSpeedButton;
    CheckGroup: TCheckGroup;
    ComboBoxH: TComboBox;
    ComboBoxV: TComboBox;
    FilterGroupBox: TGroupBox;
    ScheduleDatasource: TDatasource;
    CutGroupBox: TGroupBox;
    LabelH: TLabel;
    LabelV: TLabel;
    ScheduleSQLQuery: TSQLQuery;
    StringGrid: TStringGrid;
    procedure BtnSubmitClick(Sender: TObject);
    procedure BtnSwitchClick(Sender: TObject);
    procedure CheckGroupItemClick(Sender: TObject; Index: integer);
    procedure FormShow(Sender: TObject);
  public
    ScheduleTable: TTableInfo;
    TablesList: array of String;
    HVals, VVals: array of String;
    F: TFilters;
    FillStatus: array of array of Boolean;
    procedure PopUpForm;
    procedure FillHeader(ATable: TTableInfo; HorV: THorV);
    procedure FillItems(AFieldH, AFieldV: Integer);
    procedure FillCheckBoxGroup;
    procedure LoadGrid;
    procedure FillCell(X, Y: Integer);
  end;

var
  FormSchedule: TFormSchedule;

implementation

procedure TFormSchedule.FillCheckBoxGroup;
var
  i: Integer;
begin
  for i := 0 to High(ScheduleTable.FFields) do begin
    CheckGroup.Items.Add(ScheduleTable.FFields[i].FCaption);
    CheckGroup.Checked[i] := True;
  end;
end;

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
  F := TFilters.Create(ScheduleTable, FilterGroupBox, @LoadGrid, 10);
  F.MkPnl;
  FillCheckBoxGroup;
  LoadGrid;
end;


procedure TFormSchedule.FillCell(X, Y: Integer);
var
  i: Integer;
  Rect: TRect;
  s: String;
begin
  if not FillStatus[X, Y] then begin
    FillStatus[X, Y] := True;
    Rect := StringGrid.CellRect(X, Y);
    for i := 0 to High(ScheduleTable.FFields) do begin
      s := StringGrid.Cells[X, Y] + ScheduleSQLQuery.Fields[i].AsString + ' ';
      if CheckGroup.Checked[i] then
        //DrawText(StringGrid.Canvas.Handle, PChar(Utf8ToAnsi(s)), length(s), Rect, DT_WORDBREAK);
        StringGrid.Cells[X, Y] := s;
    end;
  end;
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

  SetLength(FillStatus, X+1, Y+1);
  for i := 0 to X do
    for j := 0 to Y do
      FillStatus[i, j] := False;

  with ScheduleSQLQuery do begin
    Close;
    SQL.Clear;
    SQL.Text := TMeta.MakeQuery(ScheduleTable, F.MakeQuery);
    for i := 0 to High(F.FilterPanels) do begin
      with F.FilterPanels[i] do begin
        if not FCheckBox.Checked then Continue;
        ParamByName(Format('FConst_Text%d', [i])).AsString := FConstraint.Text;
      end;
    end;
    Open;
    First;
    while not EOF do begin
      for i := 1 to X do
        for j := 1 to Y do
          if (Fields[AFieldH].AsString = HVals[i]) and (Fields[AFieldV].AsString = VVals[j]) then
            FillCell(i, j);
        Next;
    end;
  end;

end;

procedure TFormSchedule.LoadGrid;
var
  Rect: TRect;
var
  i, j, FieldH, FieldV: Integer;
  TableH, TableV: TTableInfo;
begin
  StringGrid.Clean;
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

procedure TFormSchedule.BtnSubmitClick(Sender: TObject);
begin
  LoadGrid;
end;

procedure TFormSchedule.BtnSwitchClick(Sender: TObject);
var
  tmp: Integer;
begin
  tmp := ComboBoxV.ItemIndex;
  ComboBoxV.ItemIndex := ComboBoxH.ItemIndex;
  ComboBoxH.ItemIndex := tmp;
  LoadGrid;
end;

procedure TFormSchedule.CheckGroupItemClick(Sender: TObject; Index: integer);
begin

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

