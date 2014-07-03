unit ConflictsTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, MetaUnit, ConflictsMeta;

type

  { TConflictsTreeForm }

  TConflictsTreeForm = class(TForm)
    TreeView: TTreeView;
    procedure FormShow(Sender: TObject);
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
begin
  Pairs := ConflictsModule.ConflictPairs[AID];
  for i := 0 to High(Pairs) do
    TreeView.Items.AddChild(ANode, Format('Конфликт между %d и %d',
      [Pairs[i].x, Pairs[i].y]));
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

{$R *.lfm}

end.

