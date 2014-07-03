program SQLAdmin;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainUnit, Connect, Referen, MetaUnit, CardEdit, Schedule, Filters;

{$R *.res}

begin
  Application.Title:='SQLAdmin';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TConnectModule, ConnectModule);
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TReferenForm, ReferenForm);
  //Application.CreateForm(TFormSchedule, FormSchedule);
  Application.Run;
end.

