unit MetaUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TFieldInfo = class
  public
    FName, FCaption, FForeignKeyTable, FForeignKeyRow: String;
    FWidth: Integer;
    constructor Create(
      AName, ACaption: String; AWidth: Integer;
      AForeignKeyTable: String = '';
      AForeignKeyRow: String = '');
  end;

  TTableInfo = class
  public
    FName, FCaption, FSequence, FSortField: String;
    FFields: array of TFieldInfo;
    constructor Create(AName, ACaption, ASequence, ASortField: String; AFields: array of TFieldInfo);
  end;

  TMeta = class
  public
    procedure AddTable(AName, ACaption, ASequence, ASortField: String; AFields: array of TFieldInfo);
    procedure AddTable(ATable: TTableInfo);
    class function MakeQuery(ATable: TTableInfo; AConstrains: string = ''): String;
    class function GetTableByName(AName: String): TTableInfo;
  end;

function MkFld(AName, ACaption: String; AWidth: Integer;
    AForeignKeyTable: String = '';
    AForeignKeyRow: String = ''): TFieldInfo;

var
  Tables: array of TTableInfo;

implementation

function MkFld(AName, ACaption: String; AWidth: Integer;
    AForeignKeyTable: String = '';
    AForeignKeyRow: String = ''): TFieldInfo;
begin
  Result := TFieldInfo.Create(
    AName, ACaption, AWidth, AForeignKeyTable, AForeignKeyRow);
end;

constructor TFieldInfo.Create(
  AName, ACaption: String; AWidth: Integer;
  AForeignKeyTable, AForeignKeyRow: String);
begin
  FName := AName;
  FCaption := ACaption;
  FWidth := AWidth;
  FForeignKeyTable := AForeignKeyTable;
  FForeignKeyRow := AForeignKeyRow;
end;

constructor TTableInfo.Create(AName, ACaption, ASequence, ASortField: String; AFields: array of TFieldInfo);
var
  i: Integer;
begin
  FName := AName;
  FCaption := ACaption;
  FSequence := ASequence;
  FSortField := ASortField;
  SetLength(FFields, Length(AFields));
  for i := 0 to High(AFields) do
    FFields[i] := AFields[i];
end;

class function TMeta.MakeQuery(ATable: TTableInfo; AConstrains: string = ''): String;
var
  rows, innerjoins, consraints: String;
  i: Integer;
begin
  rows := '';
  innerjoins := '';
  for i := 0 to High(ATable.FFields) do
    with ATable.FFields[i] do
      if FForeignKeyTable <> '' then begin
        rows += Format(', %s.Name', [FForeignKeyTable]);
        innerjoins += Format('INNER JOIN %s ON %s.%s = %s.%s ', [FForeignKeyTable, ATable.FName, FName, FForeignKeyTable, FForeignKeyRow]);
      end else
        rows += Format(', %s.%s', [ATable.FName, ATable.FFields[i].FName]);
  Delete(rows, 1, 1);
  if AConstrains <> '' then
    consraints := Format(' %s', [AConstrains])
  else
    consraints := '';
  Result := Format('SELECT %s FROM %s %s %s', [rows, ATable.FName, innerjoins, consraints]);
end;

procedure TMeta.AddTable(AName, ACaption, ASequence, ASortField: String; AFields: array of TFieldInfo);
begin
  AddTable(TTableInfo.Create(AName, ACaption, ASequence, ASortField, AFields));
end;

procedure TMeta.AddTable(ATable: TTableInfo);
begin
  SetLength(Tables, Length(Tables)+1);
  Tables[High(Tables)] := ATable;
end;

class function TMeta.GetTableByName(AName: String): TTableInfo;
var
  i: Integer;
begin
  for i := 0 to High(Tables) do
    if Tables[i].FName = AName then
      Exit(Tables[i]);
end;

end.

