unit ExportUnit;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Grids, ExtCtrls, Buttons, Menus, MetaUnit, ConflictsMeta, Filters, comobj;

type

  T4DArray = array of array of array of array of String;
  THeader = record
    Name: String;
    Id: Integer;
  end;


  TExport = class
    class procedure ExportScheduleToHTML(AHeaderH, AHeaderV: String;
      AHvals, AVvals: array of THeader; AScheduleTable: TTableInfo;
      AScheduleItems: T4DArray; AF: TFilters; ACheckGroup: TCheckGroup;
      AShowTitles: Boolean; APath: String);
    class procedure ExportScheduleToExcel(AHeaderH, AHeaderV: String;
      AHvals, AVvals: array of THeader; AScheduleTable: TTableInfo;
      AScheduleItems: T4DArray; AF: TFilters; ACheckGroup: TCheckGroup;
      AShowTitles: Boolean; APath: String);
  end;

implementation

class procedure TExport.ExportScheduleToHTML(AHeaderH, AHeaderV: String;
      AHvals, AVvals: array of THeader; AScheduleTable: TTableInfo;
      AScheduleItems: T4DArray; AF: TFilters; ACheckGroup: TCheckGroup;
      AShowTitles: Boolean; APath: String);
var
  X, Y, i, j, k, l: Integer;
  headerFile, fo: TextFile;
  tmpStr: String;
begin
  AssignFile(headerFile, 'common\header.txt');
  Reset(headerFile);
  AssignFile(fo, APath);
  Rewrite(fo);
  while not EOF(headerFile) do begin
    ReadLn(headerFile, tmpStr);
    WriteLn(fo, tmpStr);
  end;
  CloseFile(headerFile);
  k := 0;
  WriteLn(fo, '<fieldset><legend><strong>Активные фильтры</strong></legend><ol>');
  WriteLn(fo, Format('<li><i>По горизонтали:</i> %s</li>', [AHeaderH]));
  WriteLn(fo, Format('<li><i>По вертикали:</i> %s</li>', [AHeaderV]));
  for i := 0 to High(AF.FilterPanels) do
    with AF.FilterPanels[i] do
      if FCheckBox.Checked then begin
        WriteLn(fo, Format('<li> <i>%s %s</i> %s </li>', [FFieldsList.Items[FFieldsList.ItemIndex], FSignsList.Items[FSignsList.ItemIndex], FConstraint.Text]));
        Inc(k);
      end;

  X := Length(AHvals) - 1;
  Y := Length(AVvals) - 1;

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
        Writeln(fo, AHVals[i].Name + '</td>');
        Continue;
      end;
      if i = 0 then begin
        Writeln(fo, AVVals[j].Name + '</td>');
        Continue;
      end;
      for k := 0 to High(AScheduleItems[i, j]) do begin
        if ConflictsModule.IsInConflict(StrToInt(AScheduleItems[i][j][k][0])).x > -1 then
          Write(fo, '<div class = "conflict">');
        for l := 1 to High(AScheduleItems[i, j, k]) do begin
          Write(fo, '<strong>' + AScheduleTable.FFields[l].FCaption + ':</strong> ' + AScheduleItems[i][j][k][l] + '<br />');
        end;
        if k <> High(AScheduleItems[i, j]) then
          WriteLn(fo, '<div class = "separator">&nbsp</div>');
        if ConflictsModule.IsInConflict(StrToInt(AScheduleItems[i][j][k][0])).x > -1 then
          Write(fo, '</div>');
      end;
      WriteLn(fo, '</td>');
    end;
    Writeln(fo, '</tr>');
  end;
  WriteLn(fo, '</table></body></html>');
  CloseFile(fo);
end;

class procedure TExport.ExportScheduleToExcel(AHeaderH, AHeaderV: String;
      AHvals, AVvals: array of THeader; AScheduleTable: TTableInfo;
      AScheduleItems: T4DArray; AF: TFilters; ACheckGroup: TCheckGroup;
      AShowTitles: Boolean; APath: String);
var
    ExcelApp: Variant;
    i, j, X, Y, k, l: Integer;
    s: String;
begin
  ExcelApp := CreateOleObject('Excel.Application');
  ExcelApp.Application.EnableEvents := False;
  ExcelApp.Workbooks.Add;
  ExcelApp.Worksheets[1].Name := WideString(UTF8ToSys('Расписание'));

  X := Length(AHvals) - 1;
  Y := Length(AVvals) - 1;

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
        ExcelApp.Cells[j + 1, i + 1] :=  WideString(UTF8ToSys(AHVals[i].Name));
        Continue;
      end;
      if i = 0 then begin
        ExcelApp.Cells[j + 1, i + 1] :=  WideString(UTF8ToSys(AVVals[j].Name));
        Continue;
      end;
      s := '';
      for k := 0 to High(AScheduleItems[i, j]) do begin
        for l := 1 to High(AScheduleItems[i, j, k]) do begin
          if not ACheckGroup.Checked[l] then Continue;
          if AShowTitles then
            s += Format('%s: %s' + PChar(#10), [AScheduleTable.FFields[l].FCaption, AScheduleItems[i][j][k][l]])
          else
            s += Format('%s' + PChar(#10), [AScheduleItems[i][j][k][l]]);
        end;
        s += '-----------------------------------------' + PChar(#10);
      end;
      ExcelApp.Cells[j + 1, i + 1] :=  WideString(UTF8ToSys(s));
    end;
  end;

  ExcelApp.Cells[Y + 3, 1].Font.Bold := True;
  ExcelApp.Cells[Y + 3, 1].WrapText := True;
  ExcelApp.Cells[Y + 3, 1].HorizontalAlignment := 3;
  ExcelApp.Cells[Y + 3, 1] := WideString(UTF8ToSys('Активные фильтры'));
  ExcelApp.Cells[Y + 4, 1] := WideString(UTF8ToSys(Format('1. По горизонтали: %s', [AHeaderH])));
  ExcelApp.Cells[Y + 5, 1] := WideString(UTF8ToSys(Format('2. По вертикали: %s', [AHeaderV])));
  k := 3;
  for i := 0 to High(AF.FilterPanels) do
    with AF.FilterPanels[i] do
      if FCheckBox.Checked then begin
        ExcelApp.Cells[Y + 6 + i, 1] := WideString(UTF8ToSys(Format('%d. %s %s %s', [k, FFieldsList.Items[FFieldsList.ItemIndex], FSignsList.Items[FSignsList.ItemIndex], FConstraint.Text])));
        Inc(k);
      end;

  ExcelApp.DisplayAlerts := False;
  ExcelApp.Worksheets[1].SaveAs(WideString(UTF8ToSys(APath)));
  ExcelApp.Application.Quit;
end;

end.

