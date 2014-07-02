unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  Connect, Referen, MetaUnit, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuHelp: TMenuItem;
    MenuReferen: TMenuItem;
    MenuFile_Exit: TMenuItem;
    MenuHelp_About: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuFile_ExitClick(Sender: TObject);
    procedure MenuHelp_AboutClick(Sender: TObject);
    procedure MenuTable_Show(Sender: TObject);
    procedure MakeUpTableList();
  private
    { private declarations }
  public
    MenuItems: array of TMenuItem;
    AT: TMeta;
    b: TButton;
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.MakeUpTableList();
var
  i: Integer;
begin
  for i := 0 to High(Tables) do begin
    SetLength(MenuItems, Length(MenuItems)+1);
    MenuItems[High(MenuItems)] := TMenuItem.Create(MenuReferen);
    with MenuItems[High(MenuItems)] do begin
      Tag := i;
      Caption := Tables[i].FCaption;
      Name := 'MI_' + Tables[i].FName;
      OnClick := @MenuTable_Show;
    end;
  end;
  MenuReferen.Add(MenuItems);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  AT.AddTable('Subjects', 'Предметы', 'Subject_ID', [
    MkFld('ID', 'Идентификатор', 5),
    MkFld('name', 'Название', 20)
  ]);
  AT.AddTable('Subject_Types', 'Виды занятий', 'Type_ID', [
    MkFld('ID', 'Идентификатор', 5),
    MkFld('name', 'Вид', 10)
    ]);
  AT.AddTable('Professors', 'Преподаватели', 'Professor_ID', [
    MkFld('ID', 'Идентификатор', 5),
    MkFld('name', 'ФИО', 15)
    ]);
  AT.AddTable('Times', 'Пары', 'Time_ID',[
    MkFld('ID', 'Идентификатор', 5),
    MkFld('Name', 'Название', 5),
    MkFld('"Begin"', 'Начало', 10),
    MkFld('"End"', 'Окончание', 5)
  ]);
  AT.AddTable('Days', 'Дни', 'Day_Index', [
    MkFld('ID', 'Идентификатор', 5),
    MkFld('name', 'День', 15)
    ]);
  AT.AddTable('Groups', 'Группы', 'Group_ID', [
    MkFld('ID', 'Идентификатор', 5),
    MkFld('name', 'Номер', 7),
    MkFld('Group_Size', 'Размер', 5)
  ]);
  AT.AddTable('Rooms', 'Аудитории', 'Room_ID', [
    MkFld('ID', 'Идентификатор', 5),
    MkFld('name', 'Номер', 15),
    MkFld('"Size"', 'Вместимость', 10)
  ]);
  AT.AddTable('Professors_Subjects', 'Преподаватели - предметы', 'PS_ID', [
    MkFld('ID', 'Идентификатор', 5),
    MkFld('Professor_ID', 'Преподаватель', 10,'Professors', 'ID'),
    MkFld('Subject_ID', 'Предмет', 10,'Subjects', 'ID')
  ]);
  AT.AddTable('Subjects_Groups', 'Предметы - группы', 'SG_ID', [
    MkFld('ID', 'Идентификатор', 5),
    MkFld('Subject_ID', 'Предмет', 15, 'Subjects', 'ID'),
    MkFld('Group_ID', 'Группа', 15, 'Groups', 'ID')
  ]);
  AT.AddTable('Schedule_Items', 'Расписание', 'Item_ID', [
    MkFld('ID', 'Идентификатор', 5),
    MkFld('Subject_ID', 'Предмет', 20, 'Subjects', 'ID'),
    MkFld('Subject_Type_ID', 'Вид занятия', 3, 'Subject_Types', 'ID'),
    MkFld('Professor_ID', 'Преподаватель', 10, 'Professors', 'ID'),
    MkFld('Time_Index', 'Пара', 3, 'Times', 'ID'),
    MkFld('Day_Index', 'День недели', 7, 'Days', 'ID'),
    MkFld('Group_ID', 'Группа', 6, 'Groups', 'ID'),
    MkFld('Room_ID', 'Аудитория', 5,'Rooms', 'ID')
  ]);

  MakeUpTableList();
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
end;

procedure TMainForm.MenuTable_Show(Sender: TObject);
begin
  ReferenForm.PopupForm(Tables[(Sender as TMenuItem).Tag]);
end;

procedure TMainForm.MenuFile_ExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.MenuHelp_AboutClick(Sender: TObject);
begin
  ShowMessage('SQL. Болотин Иван, Б8103а');
end;

end.

