unit XYGraphu;

(*
  Creating a graph:
  if Assigned(MyGraph) then MyGraph.Destroy;
  MyGraph := TXYGraph.Create(Form1);
  InitializeMyGraph;

  Use AddPoint to add points to graph.

Procedure InitializeGraph;
const
	GraphHeight = 300;
  GraphWidth = 550;
begin
	with XYGraph do begin
    Caption := '';
  	Initialize(GraphWidth,GraphHeight,Form1.Left + Form1.Width + 200,10);
    ShowGraph;
    SetVLabel('');
    SetHLabel('Period');
    SetLegend(1,'');
    SetLegend(2,'');
    ShowLegends;
    SetXAfterDecimalPoint(0);
    SetYAfterDecimalPoint(2);
    SetXYLimits(1,TotalPeriods,0,1);
    SetAbsoluteYMax(2);
    EnablePopupMenu;
  end;
end; *)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus;
const
	MaxPlots = 10;
  DefaultXExpandFactor = 1000;
  DefaultYExpandFactor = 1;
type
  TPoints = Array of Array of Real;
  TValid = Array of Array of Boolean;
  PPoint = class(TObject)
    PlotNum : Byte;
    XNum,YNum : Real;
  end;
  TXYGraph = class(TForm)
    GraphImage: TImage;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    PopupMenu1: TPopupMenu;
    SavePlot1: TMenuItem;
    SaveDialog1: TSaveDialog;
    SetYMaximum1: TMenuItem;
    procedure SavePlot2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SetYMaximum1Click(Sender: TObject);
  private
    Procedure SavePlot;
    Procedure ReAddPoint(PlotNumber : Integer;X,Y : Real);
    Procedure ExpandXDirection;
    Procedure ExpandYDirection;
    Procedure SetXandY(X0,X1,Y0,Y1 : Real);
    Procedure ShowAxesNumbers;
  public
    AbsYMax,XMin,XMax,YMin,YMax,XExpandFactor,YExpandFactor : Real;
    XYHeight,XYWidth : Integer;
    PlotFName,PlotHeader,XLabel,YLabel : String;
    Legends : Array[1..MaxPlots] of String;
    XYLastPlotValue : Array[1..MaxPlots,1..2] of Integer;
    XYPlotIsEmpty : Array[1..MaxPlots] of Boolean;
    XOrigin,XWidth,YOrigin,YHeight: Integer;
    VLabelVOffset,VLabelHOffset,XLabelOffset,YValueOffset,XValueOffset,
    LeftOffset,BottomOffset,RightOffset,RightMargin,TopOffset,
    MajorTickSize,MinorTickSize,XAFterDecimal,YAFterDecimal,
      LegendOffset : Integer;
    Header,PlotPoints : TStringList;
    XYPoints : TPoints;
    XYValid : TValid;
    LegendsSet : Boolean;
    PRecord : Array of PPoint;
    PRecordPtr,PRecordSize : Integer;
    Constructor Initialize(WinWidth,WinHeight,WinLeft,WinTop : Integer); virtual;
    Procedure InitParameters;
    Procedure ShowGraph;
    Procedure SetVLabel(VLabel : String);
    Procedure SetHLabel(HLabel : String);
    Procedure SetPlotFileName(PlotName : String);
    Procedure SetLegend(PlotNumber : Integer;Legend : String);
    Procedure ShowLegends;
    Procedure SetXYLimits(X0,X1,Y0,Y1 : Real);
    Procedure SetAbsoluteYMax(Y1 : Real);
    Procedure SetHorGridLine(Y : Real;Color : Integer);
    Procedure SetVertGridLine(X : Real;Color : Integer);
  	Procedure SetXAfterDecimalPoint(Places : Integer);
  	Procedure SetYAfterDecimalPoint(Places : Integer);
  	Procedure AddPoint(PlotNumber : Integer;X,Y : Real);
    Procedure Clear;
    Procedure SetPlotHeader(PHeader : String);
    Procedure EnablePopupMenu;
    Procedure RedrawScreen;
  end;

var
  XYGraph : TXYGraph;
	XYPenColors : Array[1..MaxPlots] of TColor;

implementation

{$R *.DFM}


Procedure GetValues(S : String;var PNum : Integer;var X,Y : Real);
var
  T : String;
begin
  T := Copy(S,1,Pos(',',S)-1);
  PNum := StrToInt(T);
  Delete(S,1,Length(T)+1);
  T := Copy(S,1,Pos(',',S)-1);
  X := StrToFloat(T);
  Delete(S,1,Length(T)+1);
  Y := StrToFloat(S);
end;

Constructor TXYGraph.Initialize(WinWidth,WinHeight,WinLeft,WinTop : Integer);
var
	I : Integer;
begin
  InitParameters;
  ClientHeight := WinHeight;
  ClientWidth := WinWidth;
  XWidth := WinWidth - LeftOffset - RightOffset;
  YOrigin := WinHeight - BottomOffset;
  YHeight := WinHeight- BottomOffset - TopOffset;
	XOrigin := LeftOffset;
  Left := WinLeft;
	Top := WinTop;
  XYPenColors[1] := clRed;XYPenColors[2] := clBlue;
  XYPenColors[3] := clYellow;XYPenColors[4] := clWhite;
  XYPenColors[5] := clGreen;XYPenColors[6] := clFuchsia;
  XYPenColors[7] := clLime;XYPenColors[8] := clTeal;
  XYPenColors[9] := clMaroon;XYPenColors[10] := clBlack;
  for I := 1 to MaxPlots do begin
  	XYPlotIsEmpty[I] := True;
    Legends[I] := '';
  end;
  LegendsSet := False;
  PRecordSize := 1000;
  PRecordPtr := 1;
  SetLength(PRecord,1+PRecordSize);
end;

Procedure TXYGraph.InitParameters;
begin
  SavePlot1.Enabled := False;
  with PlotPoints do begin
    Duplicates := dupIgnore;
    Sorted := True;
    Clear;
  end;
  with Header do begin
    Duplicates := dupIgnore;
    Sorted := False;
    Clear;
  end;
  PlotFName := 'Plot.txt';
  PlotHeader := '';
  VLabelVOffset := 10;
	VLabelHOffset := 10;
  XLabelOffset := 5;
  YValueOffset := -10;
  XValueOffset := -23;
  LeftOffset := 50;
  BottomOffset := 40;
  RightOffset := 60;
  RightMargin := 10;
  TopOffset := 30;
  MajorTickSize := 7;
  MinorTickSize := 5;
  XAFterDecimal := 2;
  YAFterDecimal := 2;
  LegendOffset := 100;
  XExpandFactor := DefaultXExpandFactor;
  YExpandFactor := DefaultYExpandFactor;
  AbsYMax := MaxInt*1.0;
end;

Procedure TXYGraph.ShowGraph;
var
  I : Integer;
begin
	Show;
  with GraphImage.Canvas do begin
  	Brush.Color := clAqua;
  	Rectangle(0,0,ClientWidth,ClientHeight);
  	Pen.Color := clBlack;
    Pen.Width := 1;
  	Moveto(XOrigin,YOrigin);
    Lineto(XOrigin + XWidth,YOrigin);
		Moveto(XOrigin,YOrigin);
    Lineto(XOrigin,YOrigin - YHeight);
    MoveTo(XOrigin+XWidth,YOrigin);
    LineTo(XOrigin+XWidth,YOrigin-MajorTickSize);
    MoveTo(XOrigin,YOrigin-YHeight);
    LineTo(XOrigin+MajorTickSize,YOrigin-YHeight);
    for I := 1 to 10 do begin
      Pen.Color := clWhite;
      MoveTo(XOrigin+1,YOrigin-I*YHeight div 10);
      LineTo(XOrigin+XWidth,YOrigin-I*YHeight div 10);
      Pen.Color := clBlack;
      MoveTo(XOrigin + (I-1)*XWidth div 10,YOrigin);
      LineTo(XOrigin + (I-1)*XWidth div 10,YOrigin-MinorTickSize);
    end;
  end;
end;

Procedure TXYGraph.SetVLabel(VLabel : String);
var
  OldColor : Integer;
begin
  OldColor :=GraphImage.Canvas.Pen.Color;
  GraphImage.Canvas.Pen.Color := clBlack;
  GraphImage.Canvas.TextOut(VLabelHOffset,VLabelVOffset,VLabel);
  GraphImage.Canvas.Pen.Color := OldColor;
  YLabel := VLabel;
end;

Procedure TXYGraph.SetHLabel(HLabel : String);
begin
  with GraphImage.Canvas do
  	TextOut(XOrigin + XWidth-TextWidth(HLabel),
      	YOrigin+TextHeight(HLabel)+XLabelOffset,HLabel);
  XLabel := HLabel;
end;

Procedure TXYGraph.SetPlotFileName(PlotName : String);
begin
  PlotFName := PlotName;
end;

Procedure TXYGraph.SetLegend(PlotNumber : Integer;Legend : String);
begin
  Legends[PlotNumber] := Legend;
end;

Procedure TXYGraph.ShowLegends;
const
	LOff = 15;
var
	I,X0,Y0,OldWidth,OldColor : Integer;
begin
  for I := 1 to MaxPlots do
    if GraphImage.Canvas.TextWidth(Legends[I]) > LegendOffset then
      LegendOffset := GraphImage.Canvas.TextWidth(Legends[I]);
  for I := 1 to MaxPlots do
    if Legends[I] <> '' then begin
      X0 := XOrigin + XWidth + RightOffset-RightMargin-LegendOffset;
      Y0 := YOrigin+I*LOff-YHeight-30;
      GraphImage.Canvas.TextOut(X0,Y0,Legends[I]);
      GraphImage.Canvas.Pen.Color := XYPenColors[I];
      OldColor := GraphImage.Canvas.Pen.Color;
      OldWidth := GraphImage.Canvas.Pen.Width;
      GraphImage.Canvas.Pen.Width := 2;
      GraphImage.Canvas.MoveTo(X0-20,Y0 +
        GraphImage.Canvas.TextHeight('H') div 2);
      GraphImage.Canvas.LineTo(X0-4,Y0 +
        GraphImage.Canvas.TextHeight('H') div 2);
      GraphImage.Canvas.Pen.Width := OldWidth;
      GraphImage.Canvas.Pen.Color := OldColor;
    end;
  LegendsSet := True;
end;

Procedure TXYGraph.Clear;
var
	I : Integer;
begin
  for I := 1 to MaxPlots do XYPlotIsEmpty[I] := True;
  PlotPoints.Clear;
  PRecordPtr := 1;
  Header.Clear;
  ShowGraph;
  SetVLabel(YLabel);
  SetHLabel(XLabel);
  SetXYLimits(XMin,XMax,YMin,YMax);
end;

Procedure TXYGraph.SavePlot;
var
  I,J,II,JJ,K,PNum : Integer;
  XNum,YNum : Real;
  S,OutFName : String;
begin
  with SaveDialog1 do begin
    S := Application.ExeName;
    while (S <> '') and (S[Length(S)] <> '\') do Delete(S,Length(S),1);
    FileName := S + PlotFName;
    if Execute then begin
      OutFName := FileName;
      JJ := 1;
      for J := 1 to MaxPlots do
        if not XYPlotIsEmpty[J] then Inc(JJ);
      II := 0;
      J := 0;
      repeat
        I := 0;
        S := Copy(PlotPoints.Strings[J],1,Pos(',',PlotPoints.Strings[J]));
        while Pos(S,PlotPoints.Strings[J]) = 1 do begin
          Inc(J);
          Inc(I);
          if J >= PlotPoints.Count then Break;
        end;
        if I > II then II := I;
      until J >= PlotPoints.Count;
      SetLength(XYPoints,II,JJ);
      SetLength(XYValid,II,JJ);
      for I := 0 to II-1 do begin
        for J := 0 to JJ-1 do begin
          XYPoints[I,J] := 0.0;
          XYValid[I,J] := False;
        end;
      end;
      for K := 0 to PlotPoints.Count-1 do begin
        GetValues(PlotPoints.Strings[K],PNum,XNum,YNum);
        for I := 0 to II-1 do
          if (not XYValid[I,0]) or (XYPoints[I,0] = XNum) then Break;
        if not XYValid[I,0] then begin
          XYValid[I,0] := True;
          XYPoints[I,0] := XNum;
        end;
        XYValid[I,PNum] := True;
        XYPoints[I,PNum] := YNum;
      end;
      Clear;
      for I := 0 to II-1 do begin
        S := '';
        for J := 0 to JJ-1 do begin
          if XYValid[I,J] then
            S := S + FloatToStrF(XYPoints[I,J],ffGeneral,7,7);
          if J < JJ-1 then S := S + ',';
        end;
        PlotPoints.Add(IntToStr(Pos(',',S)) + ',' + S);
      end;
      PlotPoints.Sorted := True;
      PlotPoints.Sort;
      PlotPoints.Sorted := False;
      for I := 0 to PlotPoints.Count-1 do begin
        S := PlotPoints.Strings[I];
        System.Delete(S,1,Pos(',',S));
        PlotPoints.Strings[I] := S;
      end;
      if PlotHeader <> '' then Header.LoadFromFile(PlotHeader);
      for I := 0 to Header.Count-1 do
        PlotPoints.Insert(I,'//' + Header[I]);
      PlotPoints.SaveToFile(OutFName);
    end;
  end;
end;

Procedure TXYGraph.SetPlotHeader(PHeader : String);
begin
  PlotHeader := PHeader;
end;

Procedure TXYGraph.EnablePopupMenu;
begin
  SavePlot1.Enabled := True;
  SetPlotHeader('');
end;

Procedure TXYGraph.RedrawScreen;
var
  I : Integer;
begin
  ShowGraph;
  Application.ProcessMessages;
  ShowLegends;
  for I := 1 to MaxPlots do XYPlotIsEmpty[I] := True;
  for I := 1 to PRecordPtr-1 do
    ReAddPoint(PRecord[I].PlotNum,PRecord[I].XNum,PRecord[I].YNum);
  Application.ProcessMessages;
end;

Procedure TXYGraph.SetXandY(X0,X1,Y0,Y1 : Real);
begin
	XMin := X0;XMax := X1; YMin := Y0; YMax := Y1;
  if (XMax <= XMin) or (YMax <= YMin) then begin
  	ShowMessage('Illegal XY-Graph Limits!');
    Halt;
  end;
end;

Procedure TXYGraph.SetXYLimits(X0,X1,Y0,Y1 : Real);
begin
  SetXandY(X0,X1,Y0,Y1);
  ShowAxesNumbers;
end;

Procedure TXYGraph.SetAbsoluteYMax(Y1 : Real);
begin
  AbsYMax := Y1;
end;

Procedure TXYGraph.ShowAxesNumbers;
var
	S : String;
  I,HFigures : Integer;
begin
  with GraphImage.Canvas do begin
    S := Format('%*.*f',[4,XAfterDecimal,XMax]);
    if TextWidth(S) < XWidth div 10 then HFigures := 10
    else HFigures := 5;
    for I := 1 to HFigures+1 do begin
      S := Format('%*.*f',[4,XAfterDecimal,
        XMin+((XMax-XMin)/HFigures)*(I-1)]);
    	TextOut(XOrigin-(TextWidth(S) div 2)+(I-1)*(XWidth div HFigures),
        YOrigin+TextHeight('0') + YValueOffset,S);
    end;
    for I := 1 to 11 do begin
      S := Format('%*.*f',[8,YAfterDecimal,(I-1)*(YMax-YMin)/10+YMin]);
      TextOut(XOrigin-TextWidth(S+' '),Round(YOrigin-(I-1)*(YHeight)/10-5),S);
    end;
  end;
end;

Procedure TXYGraph.SetHorGridLine(Y : Real;Color : Integer);
var
	YInt,OldColor : Integer;
begin
  YInt := YOrigin - Round(YHeight*(Y-YMin)/(YMax-YMin));
  with GraphImage.Canvas do begin
    OldColor := Pen.Color;
  	Pen.Color := XYPenColors[Color];
  	MoveTo(XOrigin,YInt);
    LineTo(XOrigin+XWidth,YInt);
    Pen.Color := OldColor;
  end;
end;

Procedure TXYGraph.SetVertGridLine(X : Real;Color : Integer);
var
	XInt,OldColor : Integer;
begin
  XInt := XOrigin + Round(XWidth*((X-XMin)/(XMax-XMin)));
  with GraphImage.Canvas do begin
    OldColor := Pen.Color;
  	Pen.Color := XYPenColors[Color];
  	MoveTo(XInt,YOrigin);
    LineTo(XInt,YOrigin-YHeight);
    Pen.Color := OldColor;
  end;
end;

Procedure TXYGraph.SetXAfterDecimalPoint(Places : Integer);
begin
	XAfterDecimal := Places;
end;

Procedure TXYGraph.SetYAfterDecimalPoint(Places : Integer);
begin
	YAfterDecimal := Places;
end;

Procedure TXYGraph.AddPoint(PlotNumber : Integer;X,Y : Real);
var
	XInt,YInt,OldColor : Integer;
begin
  if not LegendsSet then ShowLegends;
  if X > XMax then ExpandXDirection;
  if Y > YMax then ExpandYDirection;
  XInt := XOrigin + Round(XWidth*((X-XMin)/(XMax-XMin)));
  YInt := YOrigin - Round(YHeight*(Y-YMin)/(YMax-YMin));
  PlotPoints.Add(IntToStr(PlotNumber)+ ',' +
    FloatToStrF(X,ffGeneral,7,3) + ',' + FloatToStrF(Y,ffGeneral,7,3));
  if not Assigned(PRecord[PRecordPtr]) then
    PRecord[PRecordPtr] := PPoint.Create;
  PRecord[PRecordPtr].PlotNum := PlotNumber;
  PRecord[PRecordPtr].XNum := X;
  PRecord[PRecordPtr].YNum := Y;
  Inc(PRecordPtr);
  if PRecordPtr >= PRecordSize then begin
    Inc(PRecordSize,1000);
    SetLength(PRecord,PRecordSize);
  end;
	if not XYPlotIsEmpty[PlotNumber] then with GraphImage.Canvas do begin
    OldColor := Pen.Color;
  	Pen.Color := XYPenColors[PlotNumber];
    Pen.Width := 3;
  	MoveTo(XYLastPlotValue[PlotNumber,1],XYLastPlotValue[PlotNumber,2]);
    LineTo(XInt,YInt);
    Pen.Color := OldColor;
  end;
  XYPLotIsEmpty[PlotNumber] := False;
  XYLastPlotValue[PlotNumber,1] := XInt;
  XYLastPlotValue[PlotNumber,2] := YInt;
end;

Procedure TXYGraph.ReAddPoint(PlotNumber : Integer;X,Y : Real);
var
	XInt,YInt,OldColor : Integer;
begin
  XInt := XOrigin + Round(XWidth*((X-XMin)/(XMax-XMin)));
  YInt := YOrigin - Round(YHeight*(Y-YMin)/(YMax-YMin));
	if not XYPlotIsEmpty[PlotNumber] then begin
    OldColor := GraphImage.Canvas.Pen.Color;
    GraphImage.Canvas.Pen.Color := XYPenColors[PlotNumber];
    GraphImage.Canvas.Pen.Width := 3;
    GraphImage.Canvas.MoveTo(XYLastPlotValue[PlotNumber,1],
      XYLastPlotValue[PlotNumber,2]);
    GraphImage.Canvas.LineTo(XInt,YInt);
    GraphImage.Canvas.Pen.Color := OldColor;
  end;
  XYPLotIsEmpty[PlotNumber] := False;
  XYLastPlotValue[PlotNumber,1] := XInt;
  XYLastPlotValue[PlotNumber,2] := YInt;
end;

Procedure TXYGraph.ExpandXDirection;
begin
  SetXandY(XMin,XMax + XExpandFactor,YMin,YMax);
  RedrawScreen;
  ShowAxesNumbers;
  SetVLabel(YLabel);
  SetHLabel(XLabel);
end;

Procedure TXYGraph.ExpandYDirection;
begin
  if YMax >= AbsYMax then Exit;
  SetXandY(XMin,XMax,YMin,YMax + YExpandFactor);
  RedrawScreen;
  ShowAxesNumbers;
  SetVLabel(YLabel);
  SetHLabel(XLabel);
end;

procedure TXYGraph.SavePlot2Click(Sender: TObject);
begin
  SavePlot;
end;

procedure TXYGraph.FormCreate(Sender: TObject);
begin
  PlotPoints := TStringList.Create;
  Header := TStringList.Create;
end;

procedure TXYGraph.SetYMaximum1Click(Sender: TObject);
begin
//
end;

end.
