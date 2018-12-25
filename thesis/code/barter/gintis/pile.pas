unit Pile;

interface

{The pile class is used mainly to pick random numbers in some range,
 and to remove numbers once they have been picked.}

type
  PileClass = class(TObject)
  private
    Index: array of Integer;
  public
    Size,NumLeft: Integer;
    Constructor Create(Size: Integer);
    Destructor Destroy; override;
    Procedure Reset;
    Procedure Initialize(ISize: Integer);
    Function ChooseRandom: Integer;
    Function ChooseFirst: Integer;
  end;

implementation

uses Dialogs;

{ This picks a random number from the first NumLeft elements in the array, and
  then replaces that element with the last element in the array, and removes
  the last element by Lowering NumLeft.
  This way the index that was picked is deleted, and the remaining elements
  are the first NumLeft elements in the array, so it's easy to pick one of
  them at random.}

Function PileClass.ChooseRandom: Integer;
var
  I : Integer;
begin
  if NumLeft = 0 then begin
    ShowMessage('Error In PlleClass.ChooseRandom');
    Halt;
  end;
  I := Random(NumLeft)+1;
  ChooseRandom := Index[I];
  Index[I] := Index[NumLeft];
  Dec(NumLeft);
end;

Function PileClass.ChooseFirst: Integer;
begin
  ChooseFirst := Index[1];
  Index[1] := Index[NumLeft];
  NumLeft := NumLeft - 1;
end;

Constructor PileClass.Create(Size: Integer);
begin
  inherited Create;
  Initialize(Size);
end;

Procedure PileClass.Initialize(ISize: Integer);
var
  I : Integer;
begin
  Size := ISize;
  NumLeft := Size;
  SetLength(Index,Size+1);
  for I := 1 to Size do Index[I] := I;
end;

Procedure PileClass.Reset;
var
  I : Integer;
begin
  NumLeft := Size;
  for I := 1 to Size do Index[I] := I;
end;

Destructor PileClass.Destroy;
begin
  inherited Destroy;
  Index := nil;
end;

end.
