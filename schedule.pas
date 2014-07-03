unit Schedule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Grids, ExtCtrls, Buttons, Menus, comobj, MetaUnit, Connect, Filters,
  Referen, CardEdit;

type



  { TFormSchedule }

  TFormSchedule = class(TForm)
    BtnExportHTML: TButton;
    BtnExportExcel: TButton;
    ImageList: TImageList;
    PopUpMenuCellEdit: TMenuItem;
    PopUpMenuCellDelete: TMenuItem;
    PopupMenuCell: TPopupMenu;
    SaveDialog: TSaveDialog;
  const
    CellSize = 200;
  type
    TMouseCoord = record
      X, Y: Integer;
    end;
    TControlPanelFull = record
      FPanel: TPanel;
      FDeleteBtn, FMoreBtn, FInsertBtn: TButton;
    end;
    THeader = record
      Name: String;
      Id: Integer;
    end;
    DinArr = array of THeader;
  procedure BtnExportExcelClick(Sender: TObject);
  procedure BtnExportHTMLClick(Sender: TObject);
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
    procedure InsertClick(Sender: TObject);
  public
    ScheduleTable, TableH, TableV: TTableInfo;
    TablesList: array of String;
    HVals, VVals: array of THeader;
    FieldH, FieldV: Integer;
    F: TFilters;
    ControlPanelFull: TControlPanelFull;
    CurrentMouse: TMouseCoord;
    ControlPanelShowed: Boolean;
    DefaultFilters: array of TFilterInfo;
    ScheduleItems: array of array of array of array of String;
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
    procedure ExportScheduleToHTML(var fo: TextFile);
    procedure ExportScheduleToExcel(path: String);
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
    Exit;
  end;

  marginX := aRect.Left;
  marginY := aRect.Top;

  currentRect := Rect(aRect.Left + 4, aRect.Top, aRect.Right, aRect.Bottom);

  if (aRow = 0) and (aCol <> 0) then begin
    DrawGrid.Canvas.TextRect(currentRect, marginX, marginY, HVals[aCol].Name, TextStyle);
    Exit;
  end;
  if (aRow <> 0) and (aCol = 0)  then begin
    DrawGrid.Canvas.TextRect(currentRect, marginX, marginY, VVals[aRow].Name, TextStyle);
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
        currString := Format('%s: %s', [ScheduleTable.FFields[j].FCaption, ScheduleItems[aCol][aRow][i][j]])
      else
        currString := ScheduleItems[aCol][aRow][i][j];
      DrawGrid.Canvas.TextRect(currentRect, marginX, marginY + insideMargin + outsideMargin, currString, TextStyle);
      insideMargin += Canvas.TextHeight(ScheduleItems[aCol][aRow][i][j]);
    end;
    DrawGrid.Canvas.Brush.Color := $bbbbbb;
    outsideMargin += Canvas.TextHeight(ScheduleItems[aCol][aRow][i][j]) * CountFiledsToShow;
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
    ScheduleItems[X][Y][ItemsCount][StrCount] := ScheduleSQLQuery.Fields[i].AsString;
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
    SQL.Text := Format('SELECT Id, Name FROM %s ORDER BY %s', [ATable.FName, ATable.FSortField]);
    Open;
    while not EOF do begin
      SetLength(Headers, Length(Headers)+1);
      Headers[i].Name:= FieldByName('Name').AsString;
      Headers[i].Id := FieldByName('Id').AsInteger;
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
      if (Fields[AFieldH].AsString = HVals[i].Name) and (Fields[AFieldV].AsString = VVals[j].Name) then begin
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
    rowsHeight1 := Canvas.TextHeight(ScheduleItems[Col][Row][0][0]) * CountFiledsToShow + 8 + DrawGrid.CellRect(Col, Row).Top;
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
  IdtoRemoveOrEdit := ScheduleItems[CurrentMouse.X][CurrentMouse.Y][AID][0];
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
    DefaultFilters[High(DefaultFilters)-1] := TFilterInfo.Create(FieldH - 1, 2, HVals[Col].Name, True);
    DefaultFilters[High(DefaultFilters)] := TFilterInfo.Create(FieldV - 1, 2, VVals[Row].Name, True);

end;

procedure TFormSchedule.PopUpMenuCellEditClick(Sender: TObject);
var
  tmpArr: array of Integer;
begin
  TCardEditForm.ShowEditor(ScheduleTable, StrToInt(IdtoRemoveOrEdit), True, @LoadGrid, tmpArr);
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
      query += Format(',%s', [ScheduleItems[CurrentMouse.X][CurrentMouse.Y][i][0]]);
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

procedure TFormSchedule.InsertClick(Sender: TObject);
var
  tmpArr: array of Integer;
  i: Integer;
begin
  SetLength(tmpArr, Length(ScheduleTable.FFields));
  for i := 0 to High(tmpArr) do
    tmpArr[i] := -1;
  tmpArr[FieldH] := HVals[CurrentMouse.x].Id;
  tmpArr[FieldV] := VVals[CurrentMouse.y].Id;
  TCardEditForm.ShowEditor(ScheduleTable, 0, False, @LoadGrid, tmpArr);
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
        OnClick := @InsertClick;
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

procedure TFormSchedule.ExportScheduleToHTML(var fo: TextFile);
var
  X, Y, i, j, k, l: Integer;
  headerFile: TextFile;
  tmpStr: String;
begin
  AssignFile(headerFile, 'common\header.txt');
  Reset(headerFile);
  while not EOF(headerFile) do begin
    ReadLn(headerFile, tmpStr);
    WriteLn(fo, tmpStr);
  end;
  CloseFile(headerFile);

  WriteLn(fo, '<fieldset><legend><strong>Активные фильтры</strong></legend><ol>');
  WriteLn(fo, Format('<li><i>По горизонтали:</i> %s</li>', [TableH.FCaption]));
  WriteLn(fo, Format('<li><i>По вертикали:</i> %s</li>', [TableV.FCaption]));
  for i := 0 to High(F.FilterPanels) do
    with F.FilterPanels[i] do
      if FCheckBox.Checked then begin
        WriteLn(fo, Format('<li> <i>%s %s</i> %s </li>', [FFieldsList.Items[FFieldsList.ItemIndex], FSignsList.Items[FSignsList.ItemIndex], FConstraint.Text]));
        Inc(k);
      end;

  X := DrawGrid.ColCount - 1;
  Y := DrawGrid.RowCount - 1;

  WriteLn(fo, '</ol></fieldset><table border = "0" cellspacing = "0" cellpadding = "0">');
  for j := 0 to Y do begin
    Writeln(fo, '<tr valign = "top">');
    for i := 0 to X do begin
      if (i = 0) xor (j = 0) then
        Write(fo, '<td class = "h">')
      else
        Write(fo, '<td>');
      if (i = 0) and (j = 0) then begin
        Writeln(fo, '</td>');
        Continue;
      end;
      if j = 0 then begin
        Writeln(fo, HVals[i].Name + '</td>');
        Continue;
      end;
      if i = 0 then begin
        Writeln(fo, VVals[j].Name + '</td>');
        Continue;
      end;
      for k := 0 to High(ScheduleItems[i, j]) do begin
        for l := 1 to High(ScheduleItems[i, j, k]) do begin
          Write(fo, '<strong>' + ScheduleTable.FFields[l].FCaption + ':</strong> ' + ScheduleItems[i][j][k][l] + '<br />');
        end;
        if k <> High(ScheduleItems[i, j]) then
          WriteLn(fo, '<div class = "separator">&nbsp</div>');
      end;
      WriteLn(fo, '</td>');
    end;
    Writeln(fo, '</tr>');
  end;
  WriteLn(fo, '</table></body></html>');
end;

procedure TFormSchedule.BtnExportHTMLClick(Sender: TObject);
var
  out_p: TextFile;
  path: String;
begin
  with SaveDialog do begin
    FileName := 'Schedule.html';
    Filter := 'Html-документ | *.html';
    Title := 'Экспорт расписания в HTML';
  end;
  if SaveDialog.Execute then begin
    path := UTF8ToSys(SaveDialog.FileName);
    AssignFile(out_p, path);
    Rewrite(out_p);
    ExportScheduleToHTML(out_p);
    CloseFile(out_p);
  end;
end;

procedure TFormSchedule.ExportScheduleToExcel(path: String);
var
    ExcelApp: Variant;
    i, j, X, Y, k, l: Integer;
    s,t: String;
begin
  ExcelApp := CreateOleObject('Excel.Application');
  ExcelApp.Application.EnableEvents := False;
  ExcelApp.Workbooks.Add;
  ExcelApp.Worksheets[1].Name := WideString(UTF8ToSys('Расписание'));

  X := DrawGrid.ColCount - 1;
  Y := DrawGrid.RowCount - 1;

  for j := 0 to Y do begin
    for i := 0 to X do begin
      if (i = 0) and (j = 0) then Continue;
      if (i = 0) xor (j = 0) then begin
        ExcelApp.Cells[j + 1, i + 1].Font.Bold := True;
        ExcelApp.Cells[j + 1, i + 1].HorizontalAlignment := 3;
      end;
      ExcelApp.Cells[j + 1, i + 1].VerticalAlignment := 1;
      ExcelApp.Cells[j + 1, i + 1].ColumnWidth := 30;
      ExcelApp.Cells[j + 1, i + 1].WrapText := True;
      ExcelApp.Cells[j + 1, i + 1].Borders.LineStyle := 1;
      if j = 0 then begin
        ExcelApp.Cells[j + 1, i + 1] :=  WideString(UTF8ToSys(HVals[i].Name));
        Continue;
      end;
      if i = 0 then begin
        ExcelApp.Cells[j + 1, i + 1] :=  WideString(UTF8ToSys(VVals[j].Name));
        Continue;
      end;
      s := '';
      for k := 0 to High(ScheduleItems[i, j]) do begin
        for l := 1 to High(ScheduleItems[i, j, k]) do begin
          if not CheckGroup.Checked[l] then Continue;
          if CheckBoxShowTitles.Checked then
            s += Format('%s: %s' + PChar(#10), [ScheduleTable.FFields[l].FCaption ,ScheduleItems[i][j][k][l]])
          else
            s += Format('%s' + PChar(#10), [ScheduleItems[i][j][k][l]]);
        end;
        s += '-----------------------------------------' + PChar(#10);
      end;
      ExcelApp.Cells[j + 1, i + 1] :=  WideString(UTF8ToSys(s));
    end;
  end;

  ExcelApp.DisplayAlerts := False;
  ExcelApp.Worksheets[1].SaveAs(WideString(UTF8ToSys(path)));
  ExcelApp.Application.Quit;
  FreeAndNil(ExcelApp);
end;

procedure TFormSchedule.BtnExportExcelClick(Sender: TObject);
var
  path: String;
begin
  with SaveDialog do begin
    FileName := 'Schedule.xlsx';
    Filter := 'Документ Excel | *.xlsx';
    Title := 'Экспорт расписания в Excel';
  end;
  if SaveDialog.Execute then
    ExportScheduleToExcel(SaveDialog.FileName);
end;

{$R *.lfm}

end.

