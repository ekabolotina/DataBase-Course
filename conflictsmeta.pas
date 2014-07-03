unit ConflictsMeta;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, FileUtil, MetaUnit, Connect;

type

  TConflictPairs = array of TPoint;

  TConflict = class
    FName: String;
    FEqual, FUnequal: array of Integer;
    constructor Create(AName: String; AEqual, AUnequal: array of Integer);
  end;

  { TConflictsModule }

  TConflictsModule = class(TDataModule)
    ConflictsSQLQuery: TSQLQuery;
  public
    ScheduleTable: TTableInfo;
    ConflictPairs: array of TConflictPairs;
    procedure AddConflict(AName: String; AEqual, AUnequal: array of Integer);
    function MakeQuery(AID: Integer): String;
    function GetConflictPairs(AID: Integer): TConflictPairs;
    function IsInConflict(AID: Integer): TPoint;
  end;

var
  ConflictsModule: TConflictsModule;
  Conflicts: array of TConflict;

implementation

constructor TConflict.Create(AName: String; AEqual, AUnequal: array of Integer);
var
  i: Integer;
begin
  FName := AName;
  SetLength(FEqual, Length(AEqual));
  SetLength(FUnequal, Length(AUnequal));
  for i := 0 to High(AEqual) do
    FEqual[i] := AEqual[i];
  for i := 0 to High(AUnequal) do
    FUnequal[i] := AUnequal[i];
end;

procedure TConflictsModule.AddConflict(AName: String; AEqual, AUnequal: array of Integer);
var
  i: Integer;
begin
  SetLength(Conflicts, Length(Conflicts) + 1);
  Conflicts[High(Conflicts)] := TConflict.Create(AName, AEqual, AUnequal);
  SetLength(ConflictPairs, Length(ConflictPairs)+1);
  ConflictPairs[High(ConflictPairs)] := GetConflictPairs(High(Conflicts));
end;

function TConflictsModule.MakeQuery(AID: Integer): String;
var
  i: Integer;
  q, pairs: String;
  currConf: TConflict;
begin
  ScheduleTable := TMeta.GetTableByName('Schedule_Items');
  currConf := Conflicts[AID];
  q := 'SELECT A.*, B.* FROM Schedule_Items A INNER JOIN Schedule_Items B ON ';
  pairs := '';

  for i := 0 to High(currConf.FEqual) do
    with ScheduleTable.FFields[currConf.FEqual[i]] do
      pairs += Format('AND A.%s = B.%s ', [FName, FName]);
  for i := 0 to High(currConf.FUnequal) do
    with ScheduleTable.FFields[currConf.FUnequal[i]] do
      pairs += Format('AND A.%s <> B.%s ', [FName, FName]);
  Delete(pairs, 1, 4);
  pairs += 'AND A.ID < B.ID';
  q += pairs;
  Result := q;
end;

function TConflictsModule.GetConflictPairs(AID: Integer): TConflictPairs;
var
  tmpPairs: TConflictPairs;
begin
  with ConflictsSQLQuery do begin
    Close;
    SQL.Clear;
    SQL.Text := MakeQuery(AID);
    Open;
    First;
    while not EOF do begin
      SetLength(tmpPairs, Length(tmpPairs)+1);
      tmpPairs[High(tmpPairs)].x := Fields[0].AsInteger;
      tmpPairs[High(tmpPairs)].y := Fields[Length(ScheduleTable.FFields)].AsInteger;
      Next;
    end;
  end;
  Result := tmpPairs;
end;

function TConflictsModule.IsInConflict(AID: Integer): TPoint;
var
  i, j: Integer;
begin
  for i := 0 to High(Conflicts) do
    for j := 0 to High(ConflictPairs[i]) do
      if (ConflictPairs[i][j].x = AID) or (ConflictPairs[i][j].y = AID) then
        Exit(Point(i, j));
  Exit(Point(-1, -1));
end;

end.
{$R *.lfm}

end.

