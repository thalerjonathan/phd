unit Barterp;
// March 28, 2007: I moved mutation to newly born agents only, and
//  created GetNextGeneration1. I raised the mutation rate to 0.1.
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label2: TLabel;
    NumAgentsBox: TEdit;
    Label3: TLabel;
    Button2: TButton;
    Button3: TButton;
    CurrentRound: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    MutationRateBox: TEdit;
    TotalPeriodsBox: TEdit;
    ShowPayoffsBox: TCheckBox;
    ShowRelPricesBox: TCheckBox;
    CheckEfficiencyBox: TCheckBox;
    Label9: TLabel;
    CheckPeriodBox: TEdit;
    Label10: TLabel;
    ReplacementRateBox: TEdit;
    Label12: TLabel;
    NumGoodsBox: TEdit;
    Label13: TLabel;
    MaxTriesBox: TEdit;
    Label14: TLabel;
    OXBox: TEdit;
    Label11: TLabel;
    ProducerShiftRateBox: TEdit;
    VarySupplyCkBox: TCheckBox;
    CommonPriceCheckBox: TCheckBox;
    CalcPriceDispersionBox: TCheckBox;
    EquiInitialPriceBox: TCheckBox;
    PauseBtn: TButton;
    Label1: TLabel;
    ReproducePeriodBox: TEdit;
    ShowInventoriesCkBox: TCheckBox;
    Label6: TLabel;
    DepreciationRateBox: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button3Click(Sender: TObject);
    procedure Button2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VarySupplyCkBoxClick(Sender: TObject);
    procedure PauseBtnClick(Sender: TObject);
  private
  public
    Running : Boolean;
  end;
  RealArray = Array of Extended;
	Agent = class(TObject)
		Index,Produces : Integer;
    Kill,Reproduce : Boolean;
    P,X,Buy,ExchangeFor : RealArray;
    ProduceAmount,Score,Fitness : Extended;
    Constructor Init(I,Ind : Integer);
    Procedure Copy(N : Integer); overload;
    Procedure CopyAndMutate(N : Integer);
    Procedure Copy(K,N : Integer); overload;
    Procedure Eat;
    Procedure CommonPriceEat(EPrices : RealArray);
    Function Trade(B : Agent) : Boolean;
    Function CommonPriceTrade(B : Agent;EPrices : RealArray) : Boolean;
    Function Lambda : Real;
    Function CommonPriceLambda(EPrices : RealArray) : Real;
    Procedure SetDemandAndSupply;
    Procedure SetCommonPriceDemandAndSupply(EPrices : RealArray);
  end;
  Intarray = Array of Integer;
var
  NumGoods : Integer;
  OX : Array of Real;
  Form1: TForm1;

implementation

{$R *.DFM}

uses Math,Pile,InputParameters, PriceMeter, XYGraphu, StatisticsU;

var
	Agents : Array of Array of Agent;
  PAgents : Array of Intarray;
  PGoods : IntArray;
  Prices : TStatistics;
	ReproducePeriod,CheckPeriod,Periods : Integer;
	MaxTries : Integer;
  NumAgents : Array of Integer;
  TOX : RealArray;
  AvePDev,AvePrice : RealArray;
  ScoreAve : RealArray;
  TotalAgents : Integer;
  TotalPeriods : Longint;
	RSeed : Longint = 123456;
  CalcPriceDispersion,EquiInitialPrices,VarySupply,CheckEfficiency,
    HaltSimulation,Pausing : Boolean;
  AverageScore,CommonPriceScore,ReplacementRate,
    DepreciationRate,ProducerShiftRate,MRate : Extended;

var
  PriceGraph,PopGraph,StdDevGraph,InventoryGraph : TXYGraph;

Constructor Agent.Init(I,Ind : Integer);
var
  J : Integer;
begin
	inherited Create;
  Index := Ind;
  Score := 0;
  Produces := I;
  SetLength(P,NumGoods+1);
  SetLength(X,NumGoods+1);
  SetLength(Buy,NumGoods+1);
  SetLength(ExchangeFor,NumGoods+1);
  if CheckEfficiency then for J := 1 to NumGoods do P[J] := 1
  else if EquiInitialPrices then
    for J := 1 to NumGoods do P[J] := OX[NumGoods]/OX[J]
  else for J := 1 to NumGoods do P[J] := Random;
  for J := 1 to NumGoods do X[J] := 0;
  ProduceAmount := NumGoods*OX[Produces];
  X[Produces] := ProduceAmount;
  P[Produces] := P[Produces]*0.8;
  SetDemandAndSupply;
end;

Procedure Agent.CopyAndMutate(N : Integer);
const
  delta = 0.95;
var
  I : Integer;
begin
  for I := 1 to NumGoods do begin
    P[I] := Agents[Produces,N].P[I];
    if not CheckEfficiency and (Random < MRate) then begin
      if Random < 0.5 then P[I] := P[I]*Delta
      else P[I] := P[I]/Delta;
    end;
  end;
end;

Procedure Agent.Copy(N : Integer);
var
  I : Integer;
begin
  for I := 1 to NumGoods do P[I] := Agents[Produces,N].P[I];
end;

Procedure Agent.Copy(K,N : Integer);
var
  I,OldIndex,OldProduces: Integer;
begin
  if NumAgents[Produces] < 10 then Exit;
  OldIndex := Index;
  OldProduces := Produces;
  Agents[K,NumAgents[K]+1] := Self;
  for I := 1 to NumGoods do begin
    P[I] := Agents[K,N].P[I];
    X[I] := Agents[K,N].X[I];
    Buy[I] := Agents[K,N].Buy[I];
    ExchangeFor[I] := Agents[K,N].ExchangeFor[I];
  end;
  ProduceAmount := Agents[K,N].ProduceAmount;
  Produces := K;
  Inc(NumAgents[K]);
  Index := NumAgents[K];
  if OldIndex <> NumAgents[OldProduces] then begin
    Agents[OldProduces,OldIndex] := Agents[OldProduces,NumAgents[OldProduces]];
    Agents[OldProduces,OldIndex].Index := OldIndex;
  end;
  Dec(NumAgents[OldProduces]);
end;

Procedure Agent.Eat;
var
  R : Real;
  I : Integer;
begin
  R := X[1]/OX[1];
  for I := 2 to NumGoods do
    if X[I]/OX[I] < R then R := X[I]/OX[I];
  if R = 0 then Exit;
  Score := Score + R;
  If R > 1 then R := 1;
  if R < 1 then
    for I := 1 to NumGoods do X[I] := X[I]*(1-R)
  else begin
    for I := 1 to NumGoods do X[I] := 0;
    X[Produces] := ProduceAmount;
    SetDemandAndSupply;
  end;
end;

Procedure Agent.CommonPriceEat(EPrices : RealArray);
var
  R : Real;
  I : Integer;
begin
  R := X[1]/OX[1];
  for I := 2 to NumGoods do
    if X[I]/OX[I] < R then R := X[I]/OX[I];
  if R = 0 then Exit;
  Score := Score + R;
  If R > 1 then R := 1;
  if R < 1 then
    for I := 1 to NumGoods do X[I] := X[I]*(1-R)
  else begin
    for I := 1 to NumGoods do X[I] := 0;
    X[Produces] := ProduceAmount;
    SetCommonPriceDemandAndSupply(EPrices);
  end;
end;

Function Agent.Lambda : Real;
var
  R : Real;
  I : Integer;
begin
  R := 0;
  for I := 1 to NumGoods do R := R + P[I]*OX[I];
  Lambda := P[Produces]*X[Produces]/R;
end;

Function Agent.CommonPriceLambda(EPrices : RealArray) : Real;
var
  R : Real;
  I : Integer;
begin
  R := 0;
  for I := 1 to NumGoods do R := R + EPrices[I]*OX[I];
  CommonPriceLambda := EPrices[Produces]*X[Produces]/R;
end;

// X[I]           Stock of good I
// Buy[I]         Amount of I agent wants to buy
// ExchangeFor[I] Amount of Production good Agent is willing to
//                  exchange for Buy[I]
// P[I]           Price agent is willing to pay for I
// Thus P[Produces]*ExchangeFor[I] := P[I]*Buy[I]
Procedure Agent.SetDemandAndSupply;
var
  I : Integer;
  Rl : Real;
begin
  Rl := Lambda;
  for I := 1 to NumGoods do begin
    Buy[I] := OX[I]*Rl-X[I];
    if Buy[I] < 0 then Buy[I] := 0;
    ExchangeFor[I] := P[I]*Buy[I]/P[Produces];
  end;
  Buy[Produces] := 0;
  ExchangeFor[Produces] := 0;
end;

Procedure Agent.SetCommonPriceDemandAndSupply(EPrices : RealArray);
var
  I : Integer;
  Rl : Real;
begin
  for I := 1 to NumGoods do X[I] := 0;
  X[Produces] := ProduceAmount;
  Rl := CommonPriceLambda(EPrices);
  for I := 1 to NumGoods do begin
    Buy[I] := OX[I]*Rl;
    ExchangeFor[I] := EPrices[I]*Buy[I]/EPrices[Produces];
  end;
  Buy[Produces] := 0;
  ExchangeFor[Produces] := 0;
end;

Function TotInventory(K : Integer) : Extended;
var
  I,J : Integer;
  R : Extended;
begin
  R := 0;
  for I := 1 to NumGoods do begin
    for J := 1 to NumAgents[I] do begin
      if I <> K then
        R := R + Agents[I,J].X[K];
    end;
  end;
  Result := R;
end;

Procedure CalculateScoreAve;
var
  I,J,K : Integer;
begin
  for I := 1 to NumGoods do begin
    AvePrice[I] := 0;
    ScoreAve[I] := 0;
  end;
  for I := 1 to NumGoods do begin
    for J := 1 to NumAgents[I] do begin
      for K := 1 to NumGoods do
        AvePrice[K] := AvePrice[K] + Agents[I,J].P[K]/Agents[I,J].P[NumGoods];
        ScoreAve[I] := ScoreAve[I] + Agents[I,J].Score;
    end;
  end;
  AverageScore := 0;
  Prices.Add(AvePrice[1]/NumAgents[1]);
  for I := 1 to NumGoods do begin
    ScoreAve[I] := ScoreAve[I]/NumAgents[I]/NumGoods;
    AverageScore := AverageScore + ScoreAve[I];
    AvePrice[I] := AvePrice[I]/NumAgents[I]/NumGoods;
    AvePDev[I] := AvePrice[I]/(OX[NumGoods]/OX[I]);
  end;
  AverageScore := AverageScore/NumGoods;
end;

Procedure ProducerShift;
var
	I,J,K,L,M,N,Replacements : Integer;
begin
	Replacements := Round(TotalAgents*ProducerShiftRate);
  for I := 1 to NumGoods do begin
    for J := 1 to Replacements do begin
      Application.ProcessMessages;
      if HaltSimulation then Exit;
      K := 1 + Random(NumGoods);
      repeat
        L := 1 + Random(NumGoods);
      until L <> K;
      M := 1 + Random(NumAgents[K]);
      N := 1 + Random(NumAgents[L]);
      if ScoreAve[K] < ScoreAve[L] then Agents[K,M].Copy(L,N)
      else Agents[L,N].Copy(K,M);
//      if Agents[K,M].Score < Agents[L,N].Score then Agents[K,M].Copy(L,N)
//      else Agents[L,N].Copy(K,M);
    end;
  end;
end;

Procedure GetNextGeneration(G : Integer);
var
	I,J,K,KK,Replacements : Integer;
  MaxScore,MinScore : Real;
begin
	Replacements := Round(NumAgents[G]*ReplacementRate);
  if Replacements = 0 then Inc(Replacements);
  MaxScore := -1000000;
  MinScore := 1000000;
  for I := 1 to NumAgents[G] do begin
    if Agents[G,I].Score > MaxScore then MaxScore := Agents[G,I].Score;
    if Agents[G,I].Score < MinScore then MinScore := Agents[G,i].Score;
  end;
  if MinScore = MaxScore then Exit;
  for I := 1 to NumAgents[G] do begin
    Agents[G,I].Reproduce := False;
    Agents[G,I].Kill := False;
    Agents[G,I].Fitness := (Agents[G,I].Score-MinScore)/(MaxScore-MinScore);
  end;
  I := 0;
  while I < Replacements do begin
    Application.ProcessMessages;
    if HaltSimulation then Exit;
    J := 1 + Random(NumAgents[G]);
    if Random < Agents[G,J].Fitness then begin
      Agents[G,J].Reproduce := True;
      Inc(I);
    end;
  end;
  I := 0;
  while I < Replacements do begin
    Application.ProcessMessages;
    if HaltSimulation then Exit;
    J := 1 + Random(NumAgents[G]);
    if Agents[G,J].Fitness < Random then begin
      Agents[G,J].Kill := True;
      Inc(I);
    end;
  end;
  I := 0;
  while I < Replacements do begin
    J := 1 + Random(NumAgents[G]);
    while not Agents[G,J].Reproduce do begin
      Inc(J);
      if J > NumAgents[G] then J := 1;
    end;
    K := 1 + Random(NumAgents[G]);
    KK := K;
    while not Agents[G,K].Kill do begin
      Inc(K);
      if K > NumAgents[G] then K := 1;
      if K = KK then begin
        Inc(I);
        Break;
      end;
    end;
    if K = KK then Continue;
    Agents[G,K].Copy(J);
    Agents[G,K].Kill := False;
    Inc(I);
  end;
  for I := 1 to NumAgents[G] do Agents[G,I].Score := 0;
end;

Procedure GetNextGeneration1(G : Integer);
var
	I,J,K,Replacements : Integer;
begin
	Replacements := Round(NumAgents[G]*ReplacementRate);
  if Replacements = 0 then Inc(Replacements);
  for I := 1 to Replacements do begin
    Application.ProcessMessages;
    if HaltSimulation then Exit;
    J := 1 + Random(NumAgents[G]);
    K := 1 + Random(NumAgents[G]);
    if Agents[G,J].Score > Agents[G,K].Score then
      Agents[G,K].CopyAndMutate(J)
    else Agents[G,J].CopyAndMutate(K);
  end;
  for I := 1 to NumAgents[G] do Agents[G,I].Score := 0;
end;

Function Agent.Trade(B : Agent) : Boolean;
var
  I,WantGood,GiveGood : Integer;
  WantAmount,GiveAmount : Real;
begin
  Trade := False;
  WantGood := B.Produces;
  GiveGood := Produces;
  if (B.Buy[GiveGood] = 0) or (B.ExchangeFor[GiveGood] = 0) or
    (B.X[WantGood] = 0) then Exit;
  if B.P[GiveGood]*ExchangeFor[WantGood] < B.P[WantGood]*Buy[WantGood] then Exit;
  Trade := True;
  WantAmount := Buy[WantGood];
  GiveAmount := ExchangeFor[WantGood];
  if B.ExchangeFor[GiveGood] < Buy[WantGood] then begin
    WantAmount := B.ExchangeFor[GiveGood];
    GiveAmount := ExchangeFor[WantGood]*B.ExchangeFor[GiveGood]/Buy[WantGood];
  end;
  if GiveAmount = 0 then Exit;
  if GiveAmount > B.Buy[GiveGood] then begin
    WantAmount := WantAmount*B.Buy[GiveGood]/GiveAmount;
    GiveAmount := B.Buy[GiveGood];
  end;
  if GiveAmount > X[GiveGood] then begin
    WantAmount := WantAmount*X[GiveGood]/GiveAmount;
    GiveAmount := X[GiveGood];
  end;
  if WantAmount > B.X[WantGood] then begin
    GiveAmount := GiveAmount*B.X[WantGood]/WantAmount;
    WantAmount := B.X[WantGood];
  end;
  B.X[GiveGood] := B.X[GiveGood] + GiveAmount;
  B.X[WantGood] := B.X[WantGood] - WantAmount;
  B.Buy[GiveGood] := B.Buy[GiveGood] - GiveAmount;
  B.ExchangeFor[GiveGood] := B.ExchangeFor[GiveGood] - WantAmount;
  X[GiveGood] := X[GiveGood] - GiveAmount;
  X[WantGood] := X[WantGood] + WantAmount;
  Buy[WantGood] := Buy[WantGood] - WantAmount;
  ExchangeFor[WantGood] := ExchangeFor[WantGood] - GiveAmount;
  if (Buy[Produces] > 0) or (B.Buy[B.Produces] > 0) then
    ShowMessage('Illegal Buy');
  for I := 1 to NumGoods do begin
    if Abs(X[I]) < 0.01 then X[I] := 0;
    if Abs(B.X[I]) < 0.01 then B.X[I] := 0;
    if Abs(ExchangeFor[I]) < 0.01 then ExchangeFor[I] := 0;
    if Abs(B.ExchangeFor[I]) < 0.01 then B.ExchangeFor[I] := 0;
    if Abs(Buy[I]) < 0.01 then Buy[I] := 0;
    if Abs(B.Buy[I]) < 0.01 then B.Buy[I] := 0;
    if (Buy[I] < 0) or (B.Buy[I] < 0) then
      ShowMessage('Negative Buy');
    if (ExchangeFor[I] < 0) or (B.ExchangeFor[I] < 0) then
      ShowMessage('Negative ExchangeFor');
    if (X[I] < 0) or (B.X[I] < 0) then
      ShowMessage('Negative X');
  end;
end;

Function Agent.CommonPriceTrade(B : Agent;EPrices : RealArray) : Boolean;
var
  I,WantGood,GiveGood : Integer;
  WantAmount,GiveAmount : Real;
begin
  CommonPriceTrade := False;
  WantGood := B.Produces;
  GiveGood := Produces;
  if (B.Buy[GiveGood] = 0) or (B.ExchangeFor[GiveGood] = 0) or
    (B.X[WantGood] = 0) then Exit;
  if EPrices[GiveGood]*ExchangeFor[WantGood] <
    EPrices[WantGood]*Buy[WantGood] then Exit;
  CommonPriceTrade := True;
  WantAmount := Buy[WantGood];
  GiveAmount := ExchangeFor[WantGood];
  if B.ExchangeFor[GiveGood] < Buy[WantGood] then begin
    WantAmount := B.ExchangeFor[GiveGood];
    GiveAmount := ExchangeFor[WantGood]*B.ExchangeFor[GiveGood]/Buy[WantGood];
  end;
  if GiveAmount = 0 then Exit;
  if GiveAmount > B.Buy[GiveGood] then begin
    WantAmount := WantAmount*B.Buy[GiveGood]/GiveAmount;
    GiveAmount := B.Buy[GiveGood];
  end;
  if GiveAmount > X[GiveGood] then begin
    WantAmount := WantAmount*X[GiveGood]/GiveAmount;
    GiveAmount := X[GiveGood];
  end;
  if WantAmount > B.X[WantGood] then begin
    GiveAmount := GiveAmount*B.X[WantGood]/WantAmount;
    WantAmount := B.X[WantGood];
  end;
  B.X[GiveGood] := B.X[GiveGood] + GiveAmount;
  B.X[WantGood] := B.X[WantGood] - WantAmount;
  B.Buy[GiveGood] := B.Buy[GiveGood] - GiveAmount;
  B.ExchangeFor[GiveGood] := B.ExchangeFor[GiveGood] - WantAmount;
  X[GiveGood] := X[GiveGood] - GiveAmount;
  X[WantGood] := X[WantGood] + WantAmount;
  Buy[WantGood] := Buy[WantGood] - WantAmount;
  ExchangeFor[WantGood] := ExchangeFor[WantGood] - GiveAmount;
  if (Buy[Produces] > 0) or (B.Buy[B.Produces] > 0) then
    ShowMessage('Illegal Buy');
  for I := 1 to NumGoods do begin
    if Abs(X[I]) < 0.01 then X[I] := 0;
    if Abs(B.X[I]) < 0.01 then B.X[I] := 0;
    if Abs(ExchangeFor[I]) < 0.01 then ExchangeFor[I] := 0;
    if Abs(B.ExchangeFor[I]) < 0.01 then B.ExchangeFor[I] := 0;
    if Abs(Buy[I]) < 0.01 then Buy[I] := 0;
    if Abs(B.Buy[I]) < 0.01 then B.Buy[I] := 0;
    if (Buy[I] < 0) or (B.Buy[I] < 0) then
      ShowMessage('Negative Buy');
    if (ExchangeFor[I] < 0) or (B.ExchangeFor[I] < 0) then
      ShowMessage('Negative ExchangeFor');
    if (X[I] < 0) or (B.X[I] < 0) then
      ShowMessage('Negative X');
  end;
end;

Procedure Shuffle(Permute: IntArray;Size : Integer);
var
  I : Integer;
  Pile : PileClass;
begin
  Pile := PileClass.Create(Size);
  for I := 1 to Size do Permute[I] := Pile.ChooseRandom;
  Pile.Destroy;
end;

Procedure Play;
var
  I,J,K,L,M : Integer;
  A : Agent;
begin
  Shuffle(PGoods,NumGoods);
  for I := 1 to NumGoods do Shuffle(PAgents[PGoods[I]],NumAgents[PGoods[I]]);
  for I := 1 to NumGoods do begin
    for J := 1 to NumAgents[PGoods[I]] do begin
      A := Agents[PGoods[I],PAgents[PGoods[I],J]];
      Assert(Assigned(A));
      for K := 1 to NumGoods do begin
        if A.Buy[PGoods[K]] > 0 then begin
          for L := 1 to MaxTries do begin
            M := 1 + Random(NumAgents[PGoods[K]]);
            if A.Trade(Agents[PGoods[K],M]) then begin
              A.Eat;
              A.SetDemandAndSupply;
              Agents[PGoods[K],M].Eat;
              Agents[PGoods[K],M].SetDemandAndSupply;
//              Break;
            end;
          end;
        end;
      end;
    end;
  end;
end;

Procedure InitializePayoffsGraph;
const
  GraphHeight = 300;
  GraphWidth = 550;
var
  I : Integer;
begin
  with XYGraph do begin
    Initialize(GraphWidth,GraphHeight,Form1.Left + Form1.Width + 25,10);
    ShowGraph;
    SetHLabel('Period');
    SetLegend('Average Score');
    for I := 1 to NumGoods do
      SetLegend('Good '+IntToStr(I)+' Score');
    SetXAfterDecimalPoint(0);
    SetYAfterDecimalPoint(2);
    SetXYLimits(0,TotalPeriods,0,1);
    Caption := 'Scores';
  end;
end;

Procedure InitializeInventoryGraph;
const
  GraphHeight = 300;
  GraphWidth = 550;
var
  I : Integer;
begin
  with InventoryGraph do begin
    Initialize(GraphWidth,GraphHeight,Form1.Left + Form1.Width + 45,30);
    ShowGraph;
    SetHLabel('Period');
    for I := 1 to NumGoods do
      SetLegend('Good '+IntToStr(I)+' Score');
    SetLegend('Inventories');
    SetXAfterDecimalPoint(0);
    SetYAfterDecimalPoint(2);
    SetXYLimits(0,TotalPeriods,0,1);
    Caption := 'Inventories';
  end;
end;

Procedure InitializePriceGraph;
const
  GraphHeight = 300;
  GraphWidth = 550;
var
  I : Integer;
begin
  with PriceGraph do begin
    Initialize(GraphWidth,GraphHeight,Form1.Left + Form1.Width +50,350);
    ShowGraph;
    SetHLabel('Period');
    SetLegend('Std Price/Equi Price');
    for I := 1 to NumGoods do
      SetLegend('Price/Equi Price Good '+IntToStr(I));
    SetXAfterDecimalPoint(0);
    SetYAfterDecimalPoint(2);
    SetXYLimits(0,TotalPeriods,-1,1);
    Caption := 'Prices';
  end;
end;

Procedure InitializePopGraph;
const
  GraphHeight = 300;
  GraphWidth = 550;
begin
  with PopGraph do begin
    Initialize(GraphWidth,GraphHeight,Form1.Left + Form1.Width + 25,550);
    ShowGraph;
    SetHLabel('Period');
    SetLegend('Share of Good 1 Producer');
    SetLegend('Share of Good 2 Producer');
    SetLegend('Share of Good 3 Producer');
    SetLegend('Share of Good 4 Producer');
    SetLegend('Share of Good 5 Producer');
    SetLegend('Share of Good 6 Producer');
    SetXAfterDecimalPoint(0);
    SetYAfterDecimalPoint(2);
    SetXYLimits(0,TotalPeriods,0,0.6);
    Caption := 'Sectoral Shares';
  end;
end;

Procedure InitializeStdDevGraph;
const
  GraphHeight = 300;
  GraphWidth = 550;
begin
  with StdDevGraph do begin
    Initialize(GraphWidth,GraphHeight,Form1.Left-40,300);
    ShowGraph;
    SetHLabel('Period');
    SetLegend('Std Dev of Producer Prices');
    SetLegend('Std Dev of Consumper Prices');
    SetLegend('Std Dev of Prices Across Periods');
    SetXAfterDecimalPoint(0);
    SetYAfterDecimalPoint(2);
    SetXYLimits(0,TotalPeriods,0,1);
    Caption := 'Price Dispersion';
  end;
end;

Function CalculateProducerPriceStdDev(K : Integer) : Real;
var
  I : Integer;
  Ave,R : Real;
begin
  Ave := 0;
  for I := 1 to NumAgents[K] do Ave := Ave +
    Agents[K,I].P[K]/Agents[K,I].P[NumGoods];
  Ave := Ave/NumAgents[K];
  R := 0;
  for I := 1 to NumAgents[K] do
    R := R + (Agents[K,I].P[K]/Agents[K,I].P[NumGoods] - Ave)*
      (Agents[K,I].P[K]/Agents[K,I].P[NumGoods]-Ave);
  CalculateProducerPriceStdDev := Sqrt(R)/NumAgents[K];
end;

Function CalculateConsumerPriceStdDev(K : Integer) : Real;
var
  I,J : Integer;
  Ave,R : Real;
begin
  Ave := 0;
  for I := 1 to NumGoods do
    if I <> K then
      for J := 1 to NumAgents[I] do
        Ave := Ave + Agents[I,J].P[K]/Agents[I,J].P[NumGoods];
  Ave := Ave/NumAgents[K]/(NumGoods-1);
  R := 0;
  for I := 1 to NumGoods do
    if I <> K then
      for J := 1 to NumAgents[I] do
        R := R + (Agents[I,J].P[K]/Agents[I,J].P[NumGoods] - Ave)*
          (Agents[I,J].P[K]/Agents[I,J].P[NumGoods]-Ave);
  CalculateConsumerPriceStdDev := Sqrt(R)/NumAgents[K]/(NumGoods-1);
end;

Function CPPriceDifferential(K : Integer) : Real;
var
  I,J : Integer;
  RC,RP : Real;
begin
  RC := 0;
  RP := 0;
  for I := 1 to NumGoods do
    if I <> K then
      for J := 1 to  NumAgents[I] do RC :=  RC + Agents[I,J].P[K]
    else for J := 1 to  NumAgents[I] do RP := RP + Agents[I,J].P[K];
  RC := RC/(TotalAgents-NumAgents[K])/(NumGoods-1);
  RP := RP/NumAgents[K];
  CPPriceDifferential := (RC-RP)/RP;
end;

Function GetCommonPriceScore(EPrices : RealArray) : Real;
var
  I,J,K,L,M : Integer;
  R : Real;
begin
  for I := 1 to NumGoods do
    for J := 1 to NumAgents[I] do
      Agents[I,J].SetCommonPriceDemandAndSupply(EPrices);
  for I := 1 to NumGoods do begin
    for J := 1 to NumAgents[I] do begin
      for K := 1 to NumGoods do begin
        if Agents[I,J].X[K] = 0 then begin
          for M := 1 to MaxTries do begin
            L := 1 + Random(NumAgents[K]);
            if Agents[I,J].CommonPriceTrade(Agents[K,L],EPrices) then begin
              Agents[I,J].CommonPriceEat(EPrices);
              Agents[K,L].CommonPriceEat(EPrices);
              Break;
            end;
          end;
        end;
      end;
    end;
  end;
  CalculateScoreAve;
  R := 0;
  for I := 1 to NumGoods do R := R + ScoreAve[I];
  GetCommonPriceScore := R/NumGoods;
end;

Procedure TForm1.Button1Click(Sender: TObject);
var
	I,J,K,L,M : Integer;
  RT,RTAve,RTMin,RTMax,R,R1 : Real;
begin
  Running := True;
  Pausing := False;
  Prices := TStatistics.Init(10);
  MaxTries := StrToInt(MaxTriesBox.Text);
  ReplacementRate := StrToFloat(ReplacementRateBox.Text);
  ProducerShiftRate := StrToFloat(ProducerShiftRateBox.Text);
  DepreciationRate := StrToFloat(DepreciationRateBox.Text);
  CalcPriceDispersion := CalcPriceDispersionBox.Checked;
  CheckEfficiency := CheckEfficiencyBox.Checked;
  EquiInitialPrices := EquiInitialPriceBox.Checked;
  if Assigned(Agents) then SetLength(Agents,0);
  NumGoods := StrToInt(NumGoodsBox.Text);
	TotalAgents := StrToInt(NumAgentsBox.Text);
  SetLength(NumAgents,1+NumGoods);
  for I := 1 to NumGoods do
    NumAgents[I] := TotalAgents;
  TotalAgents := TotalAgents*NumGoods;
  SetLength(Agents,NumGoods+1,TotalAgents+1);
  SetLength(PAgents,NumGoods+1,TotalAgents+1);
  SetLength(PGoods,NumGoods+1);
  SetLength(OX,1+NumGoods);
  SetParameters(OX,NumGoods,OXBox.Text,'OX');
  SetLength(AvePDev,1+NumGoods);
  SetLength(AvePrice,1+NumGoods);
  SetLength(ScoreAve,1+NumGoods);
  TotalPeriods := StrToInt(TotalPeriodsBox.Text);
  CheckPeriod := StrToInt(CheckPeriodBox.Text);
  VarySupply := VarySupplyCkBox.Checked;
  MRate := StrToFloat(MutationRateBox.Text);
  ReproducePeriod := StrToInt(ReproducePeriodBox.Text);
  for I := 1 to NumGoods do
    for J := 1 to NumAgents[I] do
      Agents[I,J] := Agent.Init(I,J);
  if ShowPayoffsBox.Checked then InitializePayoffsGraph;
  if VarySupply then begin
    if not Assigned(PopGraph) then PopGraph := TXYGraph.Create(Self);
    InitializePopGraph;
  end;
  if CalcPriceDispersion then begin
    if not Assigned(StdDevGraph) then StdDevGraph := TXYGraph.Create(Self);
    InitializeStdDevGraph;
  end;
  if ShowRelPricesBox.Checked then begin
    if not Assigned(PriceGraph) then PriceGraph := TXYGraph.Create(Self);
    InitializePriceGraph;
  end;
  if ShowInventoriesCkBox.Checked then begin
    if not Assigned(InventoryGraph) then InventoryGraph := TXYGraph.Create(Self);
    InitializeInventoryGraph;
  end;
  Periods := 0;
  HaltSimulation := False;
  SetLength(TOX,1+NumGoods);
  if CommonPriceCheckBox.Checked then begin
    RTAve := 0;
    RTMin := 10000;
    RTMax := 0;
    for J := 1 to 100 do begin
      for I := 1 to NumGoods do begin
        TOX[I] := Random;
        for K := 1 to NumAgents[I] do Agents[I,K].Score := 0;
      end;
      RT := GetCommonPriceScore(TOX);
      RTAve := RTAve + RT;
      if RT < RTMin then RTMin := RT;
      if RT > RTMax then RTMax := RT;
    end;
    Exit;
  end;
  if CheckEfficiency then CommonPriceScore := 1
  else begin
    for I := 1 to NumGoods do begin
      TOX[I] := 1/OX[I];
      for K := 1 to NumAgents[I] do Agents[I,K].Score := 0;
    end;
    CommonPriceScore := GetCommonPriceScore(TOX);
  end;
  Form2.Top := Top + Height;
  Form2.Left:= Left + 10;
  Form2.InitializePriceMemo;
  Form2.Show;
  for I := 1 to NumGoods do
    for J := 1 to NumAgents[I] do begin
      for K := 1 to NumGoods do Agents[I,J].X[K] := 0;
      Agents[I,J].X[Agents[I,J].Produces] := Agents[I,J].ProduceAmount;
      Agents[I,J].SetDemandAndSupply;
    end;
	for I := 1 to TotalPeriods do begin
    Inc(Periods);
    if Periods mod CheckPeriod = 0 then
      CurrentRound.Text := IntToStr(Periods);
		Play;
    for J := 1 to NumGoods do begin
      for K := 1 to NumAgents[J] do begin
        for L := 1 to NumGoods do Agents[J,K].X[L] :=
          DepreciationRate*Agents[J,K].X[L];
        Agents[J,K].X[Agents[J,K].Produces] := Agents[J,K].ProduceAmount;
        Agents[J,K].SetDemandAndSupply;
      end;
    end;
    Application.ProcessMessages;
    if HaltSimulation then begin
      Running := False;
      Exit;
    end;
		if Periods mod ReproducePeriod = 0 then begin
      CalculateScoreAve;
      for J := 1 to NumGoods do GetNextGeneration1(J);
      if VarySupply then if Random < ProducerShiftRate then ProducerShift;
    end;
		if (Periods mod CheckPeriod = 0) or (Periods = TotalPeriods) then begin
      if ShowInventoriesCkBox.Checked then with InventoryGraph do begin
        for J := 1 to NumGoods do Addpoint(J,Periods,TotInventory(J));
      end;
      if ShowPayoffsBox.Checked then with XYGraph do begin
        AddPoint(1,Periods,
          AverageScore/ReproducePeriod/CommonPriceScore);
        for J := 1 to NumGoods do
          Addpoint(J+1,Periods,ScoreAve[J]/ReproducePeriod/CommonPriceScore);
      end;
      for J := 1 to NumGoods do Form2.SetPrice(J,AvePrice[J]);
      if ShowRelPricesBox.Checked then with PriceGraph do begin
        R := 0;
        for J := 1 to NumGoods do begin
          R1 := AvePDev[J];
          R := R + (R1-1)*(R1-1);
          Addpoint(J+1,Periods,R1);
        end;
        AddPoint(1,Periods,10*Sqrt(R)/NumGoods);
        AddPoint(NumGoods+1,Periods,1);
      end;
      if CalcPriceDispersion then with StdDevGraph do begin
        Addpoint(1,Periods,CalculateProducerPriceStdDev(1));
        Addpoint(2,Periods,CalculateConsumerPriceStdDev(1));
        Addpoint(3,Periods,Prices.StandardDeviation);
      end;
      if VarySupply then with PopGraph do
        for J := 1 to NumGoods do AddPoint(J,Periods,NumAgents[J]/TotalAgents);
    end;
	end;
  Running := False;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
	HaltSimulation := True;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	HaltSimulation := True;
  Application.Terminate;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  HaltSimulation := True;
  Close;
end;

procedure TForm1.Button2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  HaltSimulation := True;
end;

procedure TForm1.VarySupplyCkBoxClick(Sender: TObject);
begin
  VarySupply := VarySupplyCkBox.Checked;
  ProducerShiftRateBox.Enabled := VarySupply
end;

procedure TForm1.PauseBtnClick(Sender: TObject);
var
  S : String;
begin
  Pausing := not Pausing;
  S := Caption;
  Caption := 'Pausing...';
  while Pausing do begin
    Application.ProcessMessages;
    if HaltSimulation then Break;
  end;
  Caption := S;
end;

initialization
  Randseed := RSeed;
end.

