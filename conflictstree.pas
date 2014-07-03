unit ConflictsTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, MetaUnit, ConflictsMeta, Filters, Referen;

type

  { TConflictsTreeForm }

  TConflictsTreeForm = class(TForm)
    TreeView: TTreeView;
    procedure FormShow(Sender: TObject);
    procedure TreeViewDblClick(Sender: TObject);
  public
    ScheduleTable: TTableInfo;
    procedure BuildTree;
    procedure InsertConflicts(ANode: TTreeNode; AID:  Integer);
  end;

var
  ConflictsTreeForm: TConflictsTreeForm;

implementation

{ TConflictsTreeForm }

procedure TConflictsTreeForm.InsertConflicts(ANode: TTreeNode; AID:  Integer);
var
  i: Integer;
  Pairs: TConflictPairs;
  tmpNode: TTreeNode;
begin
  Pairs := ConflictsModule.ConflictPairs[AID];
  for i := 0 to High(Pairs) do begin
    tmpNode := TreeView.Items.AddChild(ANode, Format('Конфликт между %d и %d',
      [Pairs[i].x, Pairs[i].y]));
    tmpNode.Data := @ConflictsModule.ConflictPairs[AID][i];
  end;
end;

procedure TConflictsTreeForm.BuildTree;
var
  i: Integer;
  rootNode, currLvlNode: TTreeNode;
begin
  rootNode := TreeView.Items.Add(nil, 'Конфликты');
  for i := 0 to High(Conflicts) do begin
    currLvlNode := TreeView.Items.AddChild(rootNode,
      Format('%d. %s', [i + 1, Conflicts[i].FName]));
    InsertConflicts(currLvlNode, i);
  end;
  rootNode.Expanded := True;
end;

procedure TConflictsTreeForm.FormShow(Sender: TObject);
begin
  BuildTree;
end;

procedure TConflictsTreeForm.TreeViewDblClick(Sender: TObject);
var
  a, b: String;
  DefFilters: array of TFilterInfo;
begin
  if TreeView.Selected.Data = nil then Exit;
  with TPoint(TreeView.Selected.Data^) do begin
    a := IntToStr(x);
    b := IntToStr(y);
  end;
  SetLength(DefFilters, 2);
  DefFilters[0] := TFilterInfo.Create(0, 2, a, True);
  DefFilters[1] := TFilterInfo.Create(0, 2, b, True);
  ReferenForm.PopupForm(ConflictsModule.ScheduleTable, 1, DefFilters);
end;

{$R *.lfm}

end.

