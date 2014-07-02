program SQLAdmin;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainUnit, Connect, Referen, MetaUnit, cardinsert;

{$R *.res}

begin
  Application.Title:='SQLAdmin';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TConnectModule, ConnectModule);
  Application.CreateForm(TReferenForm, ReferenForm);
  Application.Run;
end.

