unit More;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type
  TFormMore = class(TForm)
  public
    procedure PopUpForm;
  end;

var
  FormMore: TFormMore;

implementation

procedure TFormMore.PopUpForm;
begin
   FormMore := TFormMore.Create(nil);
   FormMore.Show;
end;

{$R *.lfm}

end.

