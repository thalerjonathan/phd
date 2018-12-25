program Barter;

uses
  Forms,
  Barterp in 'Barterp.pas' {Form1},
  PriceMeter in 'PriceMeter.pas' {Form2},
  Pile in '..\..\..\..\..\Computer Languages\Delphi\repository\pile.pas',
  Query in '..\..\..\..\..\Computer Languages\Delphi\repository\Query.pas' {QueryForm},
  XYGraphu in '..\..\..\..\..\Computer Languages\Delphi\repository\xygraphu.pas' {XYGraph},
  Statisticsu in '..\..\..\..\..\Computer Languages\Delphi\repository\Statisticsu.pas',
  InputParameters in '..\..\..\..\..\Computer Languages\Delphi\repository\inputparameters.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TQueryForm, QueryForm);
  Application.CreateForm(TXYGraph, XYGraph);
  Application.Run;
end.     
