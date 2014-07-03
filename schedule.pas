unit Schedule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Grids, ExtCtrls, Buttons, MetaUnit, Connect, Filters;

type



  { TFormSchedule }

  TFormSchedule = class(TForm)
  const
    CellSize = 200;
  type
    THorV = (H, V);
    DinArr = array of String;
  procedure DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  published
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
    DrawGrid: TDrawGrid;
    CheckBoxShowTitles: TCheckBox;
    ScrollBox: TScrollBox;
    procedure UpdateGridOnClick(Sender: TObject);
    procedure BtnSwitchClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
  public
    ScheduleTable, TableH, TableV: TTableInfo;
    TablesList: array of String;
    HVals, VVals: array of String;
    F: TFilters;
    ScheduleItems: array of array of array of array of String;
    procedure PopUpForm;
    function FillHeader(ATable: TTableInfo; var Headers: DinArr): Integer;
    procedure FillItems(AFieldH, AFieldV: Integer);
    procedure FillCheckBoxGroup(H, V: Integer);
    procedure BuildCheckBoxGroup;
    procedure LoadGrid;
    procedure FillCell(X, Y: Integer);
    function PointIntoRect(Point: TPoint; Rect: TRect): Boolean;
  end;

var
  FormSchedule: TFormSchedule;

implementation

procedure TFormSchedule.BuildCheckBoxGroup;
var
  i: Integer;
begin
  for i := 0 to High(ScheduleTable.FFields) do begin
    CheckGroup.Items.Add(ScheduleTable.FFields[i].FCaption);
    CheckGroup.Checked[i] := True;
  end;
  CheckGroup.Checked[0] := False;
end;

procedure TFormSchedule.FillCheckBoxGroup(H, V: Integer);
var
  i: Integer;
begin
  for i := 0 to High(ScheduleTable.FFields) do begin
    CheckGroup.Checked[i] := True;
  end;
  CheckGroup.Checked[0] := False;
  CheckGroup.Checked[H] := False;
  CheckGroup.Checked[V] := False;
end;

procedure TFormSchedule.FormShow(Sender: TObject);
var i:integer;
begin
  for i := 0 to High(Tables) do
    if Tables[i].FIsReferen then begin
      ComboBoxH.Items.Add(Tables[i].FCaption);
      ComboBoxV.Items.Add(Tables[i].FCaption);
    end;
  ComboBoxH.ItemIndex := 0;
  ComboBoxV.ItemIndex := 0;
  ScheduleTable := TMeta.GetTableByName('Schedule_Items');
  F := TFilters.Create(ScheduleTable, ScrollBox, @LoadGrid, 100);
  F.MkPnl;
  BuildCheckBoxGroup;
  LoadGrid;
end;

procedure TFormSchedule.DrawGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  TextStyle: TTextStyle;
  i, j, marginX, marginY, insideMargin, outsideMargin: Integer;
begin
  TextStyle.Wordbreak := True;
  TextStyle.SingleLine := True;

  if (aRow = 0) or (aCol = 0) then
    DrawGrid.Canvas.Brush.Color := $f2f2f2
  else
    DrawGrid.Canvas.Brush.Color := $ffffff;

  DrawGrid.Canvas.Rectangle(aRect.Left-1, aRect.Top-1, aRect.Left + CellSize + 1, aRect.Top + CellSize + 1);

  if (aRow = 0) and (aCol = 0) then begin
    DrawGrid.Canvas.MoveTo(aRect.TopLeft);
    DrawGrid.Canvas.LineTo(aRect.BottomRight);
  end;

  marginY := aRect.Top;
  marginX := aRect.Left + (aRect.Right - aRect.Left - Canvas.TextHeight(HVals[aCol])) div 2;

  if (aRow = 0) and (aCol <> 0) then begin
    DrawGrid.Canvas.TextRect(aRect, marginX, marginY, HVals[aCol], TextStyle);
    Exit;
  end;
  if (aRow <> 0) and (aCol = 0)  then begin
    DrawGrid.Canvas.TextRect(aRect, marginX, marginY, VVals[aRow], TextStyle);
    Exit;
  end;

  if Length(ScheduleItems[aCol, aRow]) = 0 then Exit;
  insideMargin := 0;
  outsideMargin := 0;
  for i := 0 to High(ScheduleItems[aCol, aRow]) do begin
    insideMargin := 0;
    for j := 0 to High(ScheduleItems[aCol, aRow, i]) do begin
      DrawGrid.Canvas.TextRect(Rect(aRect.Left + 4, aRect.Top, aRect.Right, aRect.Bottom), marginX, marginY + insideMargin + outsideMargin, ScheduleItems[aCol, aRow, i, j], TextStyle);
      insideMargin += Canvas.TextHeight(ScheduleItems[aCol, aRow, i, j]);
    end;
    DrawGrid.Canvas.Brush.Color := $bbbbbb;
    outsideMargin += Canvas.TextHeight(ScheduleItems[aCol, aRow, i, j]) * Length(ScheduleItems[aCol, aRow, i]);
    DrawGrid.Canvas.Line(aRect.Left + 50, marginY + outsideMargin + 5, aRect.Right - 50, marginY + outsideMargin + 5);
    outsideMargin += 8;
  end;

  if Length(ScheduleItems[aCol, aRow]) > 1 then begin
    DrawGrid.Canvas.Brush.Color := $bbbbbb;
    DrawGrid.Canvas.Polygon([aRect.BottomRight, Point(aRect.Right, aRect.Bottom-20), Point(aRect.Right-20, aRect.Bottom)]);
  end;

end;


procedure TFormSchedule.FillCell(X, Y: Integer);
var
  i, GridW, GridH, ItemsCount, StrCount: Integer;
begin
  GridW := DrawGrid.ColCount;
  GridH := DrawGrid.RowCount;
  ItemsCount := Length(ScheduleItems[X, Y]);
  SetLength(ScheduleItems[X, Y], ItemsCount + 1);

  for i := 0 to High(ScheduleTable.FFields) do
    if CheckGroup.Checked[i] then begin
      StrCount := Length(ScheduleItems[X, Y, ItemsCount]);
      SetLength(ScheduleItems[X, Y, ItemsCount], StrCount + 1);
      if CheckBoxShowTitles.Checked then
        ScheduleItems[X, Y, ItemsCount, StrCount] +=  Format('%s: %s',
          [ScheduleTable.FFields[i].FCaption, ScheduleSQLQuery.Fields[i].AsString])
      else
        ScheduleItems[X, Y, ItemsCount, StrCount] +=  ScheduleSQLQuery.Fields[i].AsString
    end;

end;

function TFormSchedule.FillHeader(ATable: TTableInfo; var Headers: DinArr): Integer;
var
  i: Integer;
begin
  i := 1;
  SetLength(Headers, 1);
  with ScheduleSQLQuery do begin
    Close;
    SQL.Clear;
    SQL.Text := Format('SELECT Name FROM %s ORDER BY %s', [ATable.FName, ATable.FSortField]);
    Open;
    while not EOF do begin
      SetLength(Headers, Length(Headers)+1);
      Headers[i] := FieldByName('Name').AsString;
      Inc(i);
      Next;
    end;
  end;
  Result := i;
end;

procedure TFormSchedule.FillItems(AFieldH, AFieldV: Integer);
var
  i, j, X, Y: Integer;
begin
  X := DrawGrid.ColCount - 1;
  Y := DrawGrid.RowCount - 1;

  SetLength(ScheduleItems, X+1, Y+1, 0, 0);

  with ScheduleSQLQuery do begin
    Close;
    SQL.Clear;
    SQL.Text := TMeta.MakeQuery(ScheduleTable, F.MakeQuery) + ' ORDER BY ' + TableV.FName + '.' + TableV.FSortField + ',' + TableH.FName + '.' + TableH.FSortField;
    for i := 0 to High(F.FilterPanels) do begin
      with F.FilterPanels[i] do begin
        if not FCheckBox.Checked then Continue;
        ParamByName(Format('FConst_Text%d', [i])).AsString := FConstraint.Text;
      end;
    end;
    Open;
    First;
    i := 1;
    j := 1;
    while not EOF do begin
      if (i > X) and (j > Y) then Break;
      if (Fields[AFieldH].AsString = HVals[i]) and (Fields[AFieldV].AsString = VVals[j]) then begin
        FillCell(i, j);
        Next;
        Continue;
      end;
      if i < X then
        Inc(i)
      else begin
        Inc(j);
        i := 1;
      end;
    end;
  end;
end;

procedure TFormSchedule.LoadGrid;
var
  i, j, FieldH, FieldV: Integer;
begin
  DrawGrid.ColCount := 0;
  FillChar(ScheduleItems, SizeOf(ScheduleItems), 0);
  TableH := Tables[ComboBoxH.ItemIndex];
  TableV := Tables[ComboBoxV.ItemIndex];
  DrawGrid.ColCount := FillHeader(TableH, HVals);
  DrawGrid.RowCount := FillHeader(TableV, VVals);

  for i := 1 to High(ScheduleTable.FFields) do
    with ScheduleTable.FFields[i] do begin
      if FForeignKeyTable = TableH.FName then
        FieldH := i;
      if FForeignKeyTable = TableV.FName then
        FieldV := i;
    end;

  FillCheckBoxGroup(FieldH, FieldV);
  FillItems(FieldH, FieldV);
end;

function TFormSchedule.PointIntoRect(Point: TPoint; Rect: TRect): Boolean;
begin
  Result :=
    (Point.x >= Rect.Left) and
    (Point.x <= Rect.Right) and
    (Point.y >= Rect.Top) and
    (Point.y <= Rect.Bottom);
end;

procedure TFormSchedule.DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ClickPoint: TPoint;
  NeededRect, GridCellRect: TRect;
  Col, Row: Integer;
begin
  Col := DrawGrid.MouseCoord(X, Y).x;
  Row := DrawGrid.MouseCoord(X, Y).y;
  ClickPoint := Point(X, Y);
  GridCellRect := DrawGrid.CellRect(Col, Row);
  NeededRect := Rect(GridCellRect.Right-20, GridCellRect.Bottom-20, GridCellRect.Right, GridCellRect.Bottom);
  if PointIntoRect(ClickPoint, NeededRect) then begin
    ShowMessage('ok');
  end;
end;

procedure TFormSchedule.UpdateGridOnClick(Sender: TObject);
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

procedure TFormSchedule.PopUpForm;
var
  i: Integer;
begin
  FormSchedule := TFormSchedule.Create(nil);
  FormSchedule.ShowModal;
end;

{$R *.lfm}

end.

