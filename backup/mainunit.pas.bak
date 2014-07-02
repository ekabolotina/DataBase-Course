unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  Connect, Referen, MetaUnit;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuHelp: TMenuItem;
    MenuReferen: TMenuItem;
    MenuFile_Exit: TMenuItem;
    MenuHelp_About: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure MenuFile_ExitClick(Sender: TObject);
    procedure MenuHelp_AboutClick(Sender: TObject);
    procedure MenuTable_Show(Sender: TObject);
    procedure MakeUpTableList();
  private
    { private declarations }
  public
    MenuItems: array of TMenuItem;
    AT: Tmeta;
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
      Caption := Tables[i].Caption;
      Name := 'MI_' + Tables[i].Name;
      OnClick := @MenuTable_Show;
    end;
  end;
  MenuReferen.Add(MenuItems);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  AT.AddTable('Subjects', 'Предметы', 1, ['name', 'Название'], [20], ['', '']);
  AT.AddTable('Subject_Types', 'Виды занятий', 1, ['name', 'Вид'], [10], ['', '']);
  AT.AddTable('Professors', 'Преподаватели', 1, ['name', 'ФИО'], [15], ['', '']);
  AT.AddTable('Times', 'Пары', 2, ['"Begin"', 'Начало', '"End"', 'Окончание'], [10, 5], ['', '', '', '']);
  AT.AddTable('Days', 'Дни', 1, ['name', 'День'], [15], ['', '']);
  AT.AddTable('Groups', 'Группы', 2, ['name', 'Номер', 'Group_Size', 'Размер'], [7, 5], ['', '', '', '']);
  AT.AddTable('Rooms', 'Аудитории', 2, ['name', 'Номер', '"Size"', 'Вместимость'], [15, 10], ['', '', '', '']);
  AT.AddTable('Professors_Subjects', 'Преподаватели - предметы', 2, ['Professor_ID', 'Преподаватель', 'Subject_ID', 'Предмет'], [15, 15], ['Professors', 'ID', 'Subjects', 'ID']);
  AT.AddTable('Subjects_Groups', 'Предметы - группы', 2, ['Subject_ID', 'Предмет', 'Group_ID', 'Группа'], [15, 15], ['Subjects', 'ID', 'Groups', 'ID']);
  AT.AddTable('Schedule_Items', 'Расписание', 8, ['Subject_ID', 'Предмет', 'Subject_Type_ID', 'Вид занятия', 'Professor_ID', 'Преподаватель', 'Time_Index', 'Пара', 'Day_Index', 'День недели', 'Group_ID', 'Группа', 'Room_ID', 'Аудитория', 'Week', 'Неделя (чет/нечет)'], [20, 3, 10, 3, 7, 6, 5, 3], ['Subjects', 'ID', 'Subject_Types', 'ID', 'Professors', 'ID', '', '', 'Days', '"Index"', 'Groups', 'ID', 'Rooms', 'ID', '', '']);

  MakeUpTableList();
end;

procedure TMainForm.MenuTable_Show(Sender: TObject);
begin
  Tables[(Sender as TMenuItem).Tag].ShowTable();
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

