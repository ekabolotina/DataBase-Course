unit Connect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil;

type

  { TConnectModule }

  TConnectModule = class(TDataModule)
    IBConnection: TIBConnection;
    SQLTransaction: TSQLTransaction;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ConnectModule: TConnectModule;

implementation

{$R *.lfm}

end.

