unit Filters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, BufDataset, FileUtil, Forms, Controls, Graphics,
  Dialogs, DBGrids, StdCtrls, ExtCtrls, DbCtrls, Connect, MetaUnit;

type

  TShowTableProc = procedure of Object;

  TFilters = class
    type
    TFilterPanel = record
      FPanel: TPanel;
      FFieldsList: TComboBox;
      FSignsList: TComboBox;
      FConstraint: TEdit;
      FRemove: TButton;
      FCheckBox: TCheckBox;
    end;
    public
      FilterType: TComboBox;
      SubmitBtn, AddBtn: TButton;
      FilterPanels: array of TFilterPanel;
      FParent: TWinControl;
      FFilterStrList: array of String;
      FShowTableProc: TShowTableProc;
      FTable: TTableInfo;
      FMaxCount: Integer;
      procedure FilterEnDis(Sender: TObject);
      procedure RemoveFilter(Sender: TObject);
      procedure AddBthClick(Sender: TObject);
      procedure SubmitBtnClick(Sender: TObject);
      procedure SubmitBtnSwitch(Sender: TObject);
      procedure MkPnl;
      function MakeQuery(): String;
      constructor Create(ATable: TTableInfo; AParent: TWinControl;
        AShowTableProc: TShowTableProc; AMaxCount: Integer);
  end;

implementation

procedure TFilters.MkPnl;
var
  i: Integer;
begin
  SetLength(FFilterStrList, Length(FTable.FFields));
  for i := 0 to High(FFilterStrList) do
    FFilterStrList[i] := FTable.FFields[i].FCaption;

  SetLength(FilterPanels, Length(FilterPanels)+1);
    with FilterPanels[High(FilterPanels)] do begin
      FPanel := TPanel.Create(nil);
      with FPanel do begin
        Width := 510;
        Height := 40;
        Top := High(FilterPanels) * 40;
        Left := 8;
        Parent := FParent;
      end;

      FCheckBox := TCheckBox.Create(nil);
      with FCheckBox do begin
        Top := 10;
        Left := 18;
        Checked := True;
        Tag := High(FilterPanels);
        OnChange := @FilterEnDis;
        Parent := FPanel;
      end;

      FFieldsList := TComboBox.Create(nil);
      with FFieldsList do begin
        Width := 100;
        Height := 23;
        Top := 8;
        Left := 50;
        Style := csDropDownList;
        Items.AddStrings(FFilterStrList);
        Items.Delete(0);
        ItemIndex := 0;
        OnChange := @SubmitBtnSwitch;
        Parent := FPanel;
      end;

      FSignsList := TComboBox.Create(nil);
      with FSignsList do begin
        Width := 50;
        Height := 23;
        Top := 8;
        Left := 160;
        Style := csDropDownList;
        Items.AddStrings(['>', '<', '=', '>=', '<=', '<>']);
        ItemIndex := 0;
        OnChange := @SubmitBtnSwitch;
        Parent := FPanel;
      end;

      FConstraint := TEdit.Create(nil);
      with FConstraint do begin
        Width := 220;
        Height := 23;
        Top := 8;
        Left := 220;
        OnChange := @SubmitBtnSwitch;
        Parent := FPanel;
      end;

      if Length(FilterPanels) <> 1 then begin
        FRemove := TButton.Create(nil);
        with FRemove do begin
          Width := 23;
          Height := 23;
          Top := 8;
          Left := 462;
          Caption := '-';
          OnClick := @RemoveFilter;
          Parent := FPanel;
        end;
        if Length(FilterPanels) <> 2 then
          FilterPanels[High(FilterPanels)-1].FRemove.Visible := False;
      end else begin
        Self.FilterType := TComboBox.Create(nil);
        with Self.FilterType do begin
          Width := 50;
          Height := 23;
          Top := 8;
          Left := 450;
          Style := csDropDownList;
          Items.AddStrings(['AND', 'OR']);
          ItemIndex := 0;
          OnChange := @SubmitBtnSwitch;
          Parent := FPanel;
        end;
      end;
    end;
end;

constructor TFilters.Create(ATable: TTableInfo; AParent: TWinControl;
  AShowTableProc: TShowTableProc; AMaxCount: Integer);
var
  i: Integer;
begin
  FParent := AParent;
  FTable := ATable;
  FMaxCount := AMaxCount;
  FShowTableProc := AShowTableProc;
  AddBtn := TButton.Create(nil);
  with AddBtn do begin
    Width := 23;
    Height := 23;
    Left := 530;
    Top := 6;
    Caption := '+';
    OnClick := @AddBthClick;
    Parent := FParent;
  end;
  SubmitBtn := TButton.Create(nil);
  with SubmitBtn do begin
    Width := 46;
    Height := 23;
    Left := 560;
    Top := 6;
    Caption := 'Обно';
    OnClick := @SubmitBtnClick;
    Parent := FParent;
  end;
end;

procedure TFilters.AddBthClick(Sender: TObject);
begin
  if Length(FilterPanels) < FMaxCount then begin
    MkPnl;
    SubmitBtnSwitch(Sender);
  end;
end;

procedure TFilters.SubmitBtnSwitch(Sender: TObject);
begin
  SubmitBtn.Enabled := True;
end;

procedure TFilters.SubmitBtnClick(Sender: TObject);
begin
  FShowTableProc;
  SubmitBtn.Enabled := False;
end;

procedure TFilters.FilterEnDis(Sender: TObject);
var
  FTag: Integer;
  FStatus: Boolean;
begin
  FTag := (Sender as TCheckBox).Tag;
  FStatus := not FilterPanels[FTag].FFieldsList.Enabled;
  with FilterPanels[FTag] do begin
    FFieldsList.Enabled := FStatus;
    FSignsList.Enabled := FStatus;
    FConstraint.Enabled := FStatus;
  end;
  SubmitBtnSwitch(Sender);
end;

procedure TFilters.RemoveFilter(Sender: TObject);
begin
  FilterPanels[High(FilterPanels)].FPanel.Free;
  SetLength(FilterPanels, Length(FilterPanels)-1);
  if Length(FilterPanels) <> 1 then
    FilterPanels[High(FilterPanels)].FRemove.Visible := True;
  SubmitBtnSwitch(Sender);
end;

function TFilters.MakeQuery(): String;
var
  i, counter: Integer;
  query, field, ftype: String;
begin
  counter := 0;
  query := '';
  for i := 0 to High(FilterPanels) do begin
    with FilterPanels[i] do begin
      if not FCheckBox.Checked then Continue;
      field := '';
      ftype := '';
      with FTable.FFields[FFieldsList.ItemIndex+1] do begin
        if FForeignKeyTable <> '' then
          field := Format('%s.Name', [FForeignKeyTable])
        else
          field := Format('%s.%s', [FTable.FName, FName]);
      end;
      if counter <> 0 then
        ftype := FilterType.Text;
      query += Format(' %s %s %s :FConst_Text%d', [ftype, field, FSignsList.Text, i]);
    end;
    inc(counter);
  end;
  if counter > 0 then
    query := 'WHERE ' + query;
  Result := query;
end;

end.

