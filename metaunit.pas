unit MetaUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Connect, Referen;

type

  Tmeta = class
  public
    procedure AddTable(tn, tc: String; count: Integer; rows_n: array of String; rows_c: array of Integer; f_k: array of String);
  end;

  TFieldInto = class
    Name, Caption, ForeignKeyTable, ForeignKeyRow: String;
    Width: Integer;
    constructor Create(n, c: String; w: Integer; f_k_t, f_k_r: String);
  end;

  TTableInfo = class
    Fields: array of TFieldInto;
    Name, Caption: String;
    constructor Create(f: array of TFieldInto; n, c: String);
    procedure ShowTable();
  end;

var
  Tables: array of TTableInfo;

implementation

constructor TFieldInto.Create(n, c: String; w: Integer; f_k_t, f_k_r: String);
begin
  Name := n;
  Caption := c;
  Width := w;
  ForeignKeyTable := f_k_t;
  ForeignKeyRow := f_k_r;
end;

constructor TTableInfo.Create(f: array of TFieldInto; n, c: String);
var
  i: Integer;
begin
  for i := 0 to High(f) do begin
    SetLength(Fields, Length(Fields)+1);
    Fields[i] := f[i];
  end;
  Name := n;
  Caption := c;
end;

procedure TTableInfo.ShowTable();
var
  rows, innerjoins: String;
  i: Integer;
begin
  ReferenForm := TReferenForm.Create(nil);
  rows := '';
  innerjoins := '';
  for i := 0 to High(Fields) do begin
    if Fields[i].ForeignKeyTable <> '' then begin
      rows := rows + ', ' + Fields[i].ForeignKeyTable + '.Name';
      innerjoins := innerjoins + 'INNER JOIN ' + Fields[i].ForeignKeyTable + ' ON ' + Name + '.' + Fields[i].Name + ' = ' + Fields[i].ForeignKeyTable + '.' + Fields[i].ForeignKeyRow + ' ';
    end else
      rows := rows + ',' + Fields[i].Name;
  end;
  Delete(rows, 1, 1);
  ReferenForm.SQLQuery.SQL.Text := 'SELECT ' + rows + ' FROM ' + Name + ' ' + innerjoins;
  ReferenForm.SQLQuery.Open;
  for i := 0 to High(Fields) do begin
    ReferenForm.DBGrid.Columns[i].Title.Caption := Fields[i].Caption;
    ReferenForm.DBGrid.Columns[i].Width := Fields[i].Width * 10;
  end;
  ReferenForm.Caption := Caption;
  ReferenForm.Show;
end;

procedure Tmeta.AddTable(tn, tc: String; count: Integer; rows_n: array of String; rows_c: array of Integer; f_k: array of String);
var
  i, k: Integer;
  Fields: array of TFieldInto;
begin
  i := 0;
  k := 0;
  while i < 2 * count do begin
    SetLength(Fields, Length(Fields)+1);
    Fields[k] := TFieldInto.Create(rows_n[i], rows_n[i+1], rows_c[k], f_k[i], f_k[i+1]);
    i := i+2;
    inc(k);
  end;
  SetLength(Tables, Length(Tables)+1);
  Tables[High(Tables)] := TTableInfo.Create(Fields, tn, tc);
end;

end.

