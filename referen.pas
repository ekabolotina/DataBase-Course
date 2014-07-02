unit Referen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, BufDataset, FileUtil, Forms, Controls, Graphics,
  Dialogs, DBGrids, StdCtrls;

type

  { TReferenForm }

  TReferenForm = class(TForm)
    Datasource: TDatasource;
    DBGrid: TDBGrid;
    SQLQuery: TSQLQuery;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ReferenForm: TReferenForm;

implementation

{$R *.lfm}

end.

