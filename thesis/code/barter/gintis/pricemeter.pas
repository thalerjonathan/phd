unit PriceMeter;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
  private
  public
    Procedure InitializePriceMemo;
    Procedure SetPrice(I : Integer;P : Real);
  end;

var
  Form2: TForm2;

implementation

uses Barterp;

{$R PriceMeter.dfm}

Procedure TForm2.InitializePriceMemo;
var
  I : Integer;
begin
  Memo1.Clear;
  for I := 0 to NumGoods+2 do Memo1.Lines.Add('');
  Memo1.Lines[0] := '  Actual      Equilibrium';
  Memo1.Lines[1] := '  Price             Price';
end;

Procedure TForm2.SetPrice(I : Integer;P : Real);
var
  S,T : String;
begin
  S := Format('  P' + IntToStr(I)+ ': %5.2f',[P]);
  while Length(S) < 14 do S := S + ' ';
  T := FloatToStrF(OX[NumGoods]/OX[I],ffGeneral,5,2);
  if Pos('.',T) = 0 then T := T + '.00'
  else while Length(T) < 4 do T := T + '0';
  S := S + '   P'+IntToStr(I)+': ' + T;
  Memo1.Lines[I+1] := S;
  Memo1.Lines[NumGoods+4] := ' ';
end;

end.
