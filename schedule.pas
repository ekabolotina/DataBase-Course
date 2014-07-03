unit Schedule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Grids, ExtCtrls, Buttons, Menus, MetaUnit, Connect, Filters,
  Referen, CardEdit;

type



  { TFormSchedule }

  TFormSchedule = class(TForm)
    ImageList: TImageList;
    PopUpMenuCellEdit: TMenuItem;
    PopUpMenuCellDelete: TMenuItem;
    PopupMenuCell: TPopupMenu;
  const
    CellSize = 200;
  type
    DinArr = array of String;
    TMouseCoord = record
      X, Y: Integer;
    end;
    TControlPanelFull = record
      FPanel: TPanel;
      FDeleteBtn, FMoreBtn, FInsertBtn: TButton;
    end;
    TScheduleItems = record
      Val, Name: String;
    end;
  procedure PopUpMenuCellDeleteClick(Sender: TObject);
  procedure PopUpMenuCellEditClick(Sender: TObject);
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
    procedure DrawGridDblClick(Sender: TObject);
    procedure DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RomoveClick(Sender: TObject);
  public
    ScheduleTable, TableH, TableV: TTableInfo;
    TablesList: array of String;
    HVals, VVals: array of String;
    FieldH, FieldV: Integer;
    F: TFilters;
    ControlPanelFull: TControlPanelFull;
    CurrentMouse: TMouseCoord;
    ControlPanelShowed: Boolean;
    DefaultFilters: array of TFilterInfo;
    ScheduleItems: array of array of array of array of TScheduleItems;
    IdtoRemoveOrEdit: String;
    procedure PopUpForm;
    function FillHeader(ATable: TTableInfo; var Headers: DinArr): Integer;
    procedure FillItems(AFieldH, AFieldV: Integer);
    procedure FillCheckBoxGroup(H, V: Integer);
    procedure BuildCheckBoxGroup;
    function CountFiledsToShow: Integer;
    procedure LoadGrid;
    procedure FillCell(X, Y: Integer);
    function PointIntoRect(Point: TPoint; Rect: TRect): Boolean;
    procedure DrawPanel(ARect: TRect; ACol, ARow: Integer; AFull: Boolean);
    procedure ShowControlPanel(ACol, ARow: Integer);
    procedure RemoveControlPanel(ACol, ARow: Integer);
    procedure UpdateMouse(ACol, ARow: Integer);
    procedure ShowPopUpMenu(AID, X, Y: Integer);
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

function TFormSchedule.CountFiledsToShow: Integer;
var
  i, count: Integer;
begin
  count := 0;
  for i := 0 to High(ScheduleTable.FFields) do begin
    if CheckGroup.Checked[i] then
      Inc(count);
  end;
  Result := count;
end;

procedure TFormSchedule.FormShow(Sender: TObject);
var
  i:integer;
begin
  for i := 0 to High(Tables) do
    if Tables[i].FIsReferen then begin
      ComboBoxH.Items.Add(Tables[i].FCaption);
      ComboBoxV.Items.Add(Tables[i].FCaption);
    end;
  ComboBoxH.ItemIndex := 0;
  ComboBoxV.ItemIndex := 1;
  ScheduleTable := TMeta.GetTableByName('Schedule_Items');
  F := TFilters.Create(ScheduleTable, ScrollBox, @LoadGrid, 100);
  BuildCheckBoxGroup;
  LoadGrid;
end;

procedure TFormSchedule.DrawGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  TextStyle: TTextStyle;
  i, j, marginX, marginY, insideMargin, outsideMargin: Integer;
  currentRect: TRect;
  currString: String;
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

  marginX := aRect.Left;
  marginY := aRect.Top;

  currentRect := Rect(aRect.Left + 4, aRect.Top, aRect.Right, aRect.Bottom);

  if (aRow = 0) and (aCol <> 0) then begin
    DrawGrid.Canvas.TextRect(currentRect, marginX, marginY, HVals[aCol], TextStyle);
    Exit;
  end;
  if (aRow <> 0) and (aCol = 0)  then begin
    DrawGrid.Canvas.TextRect(currentRect, marginX, marginY, VVals[aRow], TextStyle);
    Exit;
  end;

  if Length(ScheduleItems[aCol, aRow]) = 0 then Exit;
  insideMargin := 0;
  outsideMargin := 2;
  for i := 0 to High(ScheduleItems[aCol, aRow]) do begin
    if i = 2 then Break;
    insideMargin := 0;
    for j := 0 to High(ScheduleItems[aCol, aRow, i]) do begin
      if not CheckGroup.Checked[j] then Continue;
      if CheckBoxShowTitles.Checked then
        currString := Format('%s: %s', [ScheduleItems[aCol][aRow][i][j].Name, ScheduleItems[aCol][aRow][i][j].Val])
      else
        currString := ScheduleItems[aCol][aRow][i][j].Val;
      DrawGrid.Canvas.TextRect(currentRect, marginX, marginY + insideMargin + outsideMargin, currString, TextStyle);
      insideMargin += Canvas.TextHeight(ScheduleItems[aCol][aRow][i][j].Val);
    end;
    DrawGrid.Canvas.Brush.Color := $bbbbbb;
    outsideMargin += Canvas.TextHeight(ScheduleItems[aCol][aRow][i][j].Val) * CountFiledsToShow;
    DrawGrid.Canvas.Line(aRect.Left + 50, marginY + outsideMargin + 5, aRect.Right - 50, marginY + outsideMargin + 5);
    outsideMargin += 8;
  end;

  if Length(ScheduleItems[aCol, aRow]) > 1 then begin
    currentRect := Rect(aRect.Right - 15, aRect.Bottom - 15, aRect.Right, aRect.Bottom);
    DrawGrid.Canvas.Brush.Color := $bbbbbb;
    DrawGrid.Canvas.Polygon([aRect.BottomRight, Point(aRect.Right, aRect.Bottom-28), Point(aRect.Right-28, aRect.Bottom)]);
    DrawGrid.Canvas.TextRect(currentRect, currentRect.Left, currentRect.Top, IntToStr(Length(ScheduleItems[aCol, aRow])), TextStyle);
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

  for i := 0 to High(ScheduleTable.FFields) do begin
    StrCount := Length(ScheduleItems[X, Y, ItemsCount]);
    SetLength(ScheduleItems[X, Y, ItemsCount], StrCount + 1);
    ScheduleItems[X][Y][ItemsCount][StrCount].Name := ScheduleTable.FFields[i].FCaption;
    ScheduleItems[X][Y][ItemsCount][StrCount].Val := ScheduleSQLQuery.Fields[i].AsString;
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
  i, j: Integer;
begin
  RemoveControlPanel(CurrentMouse.X, CurrentMouse.Y);
  CurrentMouse.X := -1;
  CurrentMouse.Y := -1;
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

procedure TFormSchedule.UpdateMouse(ACol, ARow: Integer);
begin
  CurrentMouse.X := ACol;
  CurrentMouse.Y := ARow;
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
    //ReferenForm.PopupForm(ScheduleTable, DefaultFilters);
  end;
end;

procedure TFormSchedule.DrawGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  FormShift: TPoint;
  rowsHeight1, rowsHeight2, countItems, Col, Row: Integer;
begin
  Col := DrawGrid.MouseCoord(X, Y).x;
  Row := DrawGrid.MouseCoord(X, Y).y;
  UpdateMouse(Col, Row);

  FormShift := GetClientOrigin;
  if (Button = mbRight) and (CurrentMouse.X <> 0) and (CurrentMouse.Y <> 0) and (Length(ScheduleItems[Col, Row]) <> 0) then begin
    rowsHeight1 := Canvas.TextHeight(ScheduleItems[Col][Row][0][0].Val) * CountFiledsToShow + 8 + DrawGrid.CellRect(Col, Row).Top;
    if Length(ScheduleItems[Col, Row]) > 1 then
      rowsHeight2 := rowsHeight1 * 2 - DrawGrid.CellRect(Col, Row).Top - 8
    else
      rowsHeight2 := rowsHeight1;
    if Y < rowsHeight1 then
      ShowPopUpMenu(0, X + FormShift.x + DrawGrid.Left + 2, Y + FormShift.y + DrawGrid.Top + 2);
    if (Y > rowsHeight1) and (Y < rowsHeight2) then
      ShowPopUpMenu(1, X + FormShift.x + DrawGrid.Left + 2, Y + FormShift.y + DrawGrid.Top + 2);
  end;
end;

procedure TFormSchedule.ShowPopUpMenu(AID, X, Y: Integer);
begin
  IdtoRemoveOrEdit := ScheduleItems[CurrentMouse.X][CurrentMouse.Y][AID][0].Val;
  PopupMenuCell.PopUp(X, Y);
end;

procedure TFormSchedule.DrawGridMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  currRect: TRect;
  Col, Row: Integer;
begin
  Col := DrawGrid.MouseCoord(X, Y).x;
  Row := DrawGrid.MouseCoord(X, Y).y;
  if CurrentMouse.X = -1 then begin
    UpdateMouse(Col, Row);
    ShowControlPanel(Col, Row);
  end;
  if (Col <> CurrentMouse.X) or (Row <> CurrentMouse.Y) then begin
    RemoveControlPanel(CurrentMouse.X, CurrentMouse.Y);
    UpdateMouse(Col, Row);
    ShowControlPanel(Col, Row);
  end;

    F.CollectDefFilters(DefaultFilters);
    SetLength(DefaultFilters, Length(DefaultFilters)+2);
    DefaultFilters[High(DefaultFilters)-1] := TFilterInfo.Create(FieldH - 1, 2, HVals[Col], True);
    DefaultFilters[High(DefaultFilters)] := TFilterInfo.Create(FieldV - 1, 2, VVals[Row], True);

end;

procedure TFormSchedule.PopUpMenuCellEditClick(Sender: TObject);
begin
  TCardEditForm.ShowEditor(ScheduleTable, StrToInt(IdtoRemoveOrEdit), True, @LoadGrid);
end;

procedure TFormSchedule.PopUpMenuCellDeleteClick(Sender: TObject);
var
  answer, i: Integer;
  query: String;
begin
  answer := MessageDlg(Format('Вы действительно хотите удалить выбраннцю запись (ID %s)', [IdtoRemoveOrEdit]), mtCustom, [mbYes, mbNo], 0);
  if answer = 6 then begin
    query := Format('DELETE FROM %s WHERE ID = %s', [ScheduleTable.FName, IdtoRemoveOrEdit]);
    with ScheduleSQLQuery do begin
      Close;
      SQL.Clear;
      SQL.Text := query;
      ExecSQL;
    end;
    ConnectModule.SQLTransaction.Commit;
    LoadGrid;
  end;
end;

procedure TFormSchedule.RomoveClick(Sender: TObject);
var
  answer, i: Integer;
  query, tmpId : String;
begin
  answer := MessageDlg(Format('Вы действительно хотите удалить выбранные записи (%d штук)', [Length(ScheduleItems[CurrentMouse.X, CurrentMouse.Y])]), mtCustom, [mbYes, mbNo], 0);
  if answer = 6 then begin
    for i := 0 to High(ScheduleItems[CurrentMouse.X, CurrentMouse.Y]) do
      query += Format(',%s', [ScheduleItems[CurrentMouse.X][CurrentMouse.Y][i][0].Val]);
    Delete(query, 1, 1);
    query := Format('DELETE FROM %s WHERE ID IN (%s)', [ScheduleTable.FName, query]);
    with ScheduleSQLQuery do begin
      Close;
      SQL.Clear;
      SQL.Text := query;
      ExecSQL;
    end;
    ConnectModule.SQLTransaction.Commit;
    LoadGrid;
  end;
end;

procedure TFormSchedule.ShowControlPanel(ACol, ARow: Integer);
begin
  if (ACol <> 0) and (ARow <> 0) then begin
    if Length(ScheduleItems[ACol, ARow]) <> 0 then
      DrawPanel(DrawGrid.CellRect(ACol, ARow), ACol, ARow, True)
    else
      DrawPanel(DrawGrid.CellRect(ACol, ARow), ACol, ARow, False);
    ControlPanelShowed := True;
  end;
end;

procedure TFormSchedule.RemoveControlPanel(ACol, ARow: Integer);
begin
  if (ACol <> 0) and (ARow <> 0) and ControlPanelShowed then begin
    ControlPanelFull.FPanel.Free;
    ControlPanelShowed := False;
  end;
end;

procedure TFormSchedule.DrawPanel(ARect: TRect; ACol, ARow: Integer; AFull: Boolean);
begin
  with ControlPanelFull do begin
      FPanel := TPanel.Create(nil);
      with FPanel do begin
        Width := 175;
        Height := 28;
        Top := ARect.Bottom - 33;
        Left := ARect.Left + 2;
        BevelOuter := bvNone;
        Parent := DrawGrid;
      end;
      FInsertBtn := TButton.Create(nil);
      with FInsertBtn do begin
        Width := 55;
        Height := 23;
        Top := 3;
        if AFull then Left := 0 else Left := 70;
        Caption := 'Вставить';
        OnClick := @RomoveClick;
        Parent := FPanel;
      end;
      if not AFull then Exit;
      FDeleteBtn := TButton.Create(nil);
      with FDeleteBtn do begin
        Width := 55;
        Height := 23;
        Top := 3;
        Left := 60;
        Caption := 'Удалить';
        OnClick := @RomoveClick;
        Parent := FPanel;
      end;
      FMoreBtn := TButton.Create(nil);
      with FMoreBtn do begin
        Width := 55;
        Height := 23;
        Top := 3;
        Left := 120;
        Caption := 'Открыть';
        OnClick := @DrawGridDblClick;
        Parent := FPanel;
      end;
  end;
end;

procedure TFormSchedule.DrawGridDblClick(Sender: TObject);
begin
  if (CurrentMouse.X <> 0) and (CurrentMouse.Y <> 0) then
    ReferenForm.PopupForm(ScheduleTable, DefaultFilters);
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

