unit InputParameters;

interface

// RArray : Array of Paramters to fill in
// Len : Number of Parameters
// S : Input string
// Msg : Error message
// Returns Filled out string
// String is floating point numbers separated by commas
//   1,,3,4,5 -> 1,2,3,4,5
//   1,2 => 1,2,2,2,2  (if Len = 5)
//   1-5 => 1,2,3,4,5
//   1-7/2 => 1,3,5,7
Function SetParameters(var RArray: Array of Extended;Len : Integer;
  S,Msg : String) : String;

Function UpdateParameters(RArray: Array of Real;Len: Integer) : String;

implementation

uses Dialogs, SysUtils;

const
  Digits = ['0'..'9'];
  FloatChars = ['0'..'9','.'];

  Function IsRange(S : String;var J : Integer) : Boolean;
var
  I : Integer;
begin
  IsRange := True;
  for I := 2 to Length(S)-1 do
    if (S[I] = '-') and (S[I-1] in Digits) and (S[I+1] in Digits) then begin
      J := I;
      Exit;
    end;
  IsRange := False;
end;

Function SetParameters(var RArray: Array of Extended;Len : Integer;
  S,Msg : String) : String;
var
  I,J,K,Mult,Start,Stop,Step,Next : Integer;
  Frag : String;
begin
  SetParameters := S;
  while Pos(' ',S) > 0 do
    Delete(S,Pos(' ',S),1);
  while Pos(',,',S) > 0 do begin
    I := Pos(',,',S);
    if I = 1 then begin
      ShowMessage('Illegal ' + Msg + '!');
      Exit;
    end;
    J := I + 1;
    Dec(I);
    while (I > 1) and (S[I] <> ',') do Dec(I);
    if I > 1 then Inc(I);
    while S[I] <> ',' do begin
      Insert(S[I],S,J);
      Inc(I);
      Inc(J);
    end;
  end;
  while IsRange(S,I) do begin
    S[I] := ',';
    J := I-1;
    Start := 0;
    Mult := 1;
    while (J >= 1) and (S[J] in Digits) do begin
      Start := Start + Mult*Ord(S[J])- Ord('0');
      Dec(J);
      Mult := Mult*10;
    end;
    J := I+1;
    Stop := 0;
    while (J <= Length(S)) and (S[J] in Digits) do begin
      Stop := 10*Stop + Ord(S[J])-Ord('0');
      Inc(J);
    end;
    Step := 1;
    if (S[J] ='/') and (J < Length(S)) and (S[J+1] in Digits) then begin
      Step := Ord(S[J+1])-Ord('0');
      Delete(S,J,2);
      while (J <= Length(S)) and (S[J] in Digits) do begin
        Step := 10*Step + Ord(S[J])-Ord('0');
        Delete(S,J,1);
      end;
    end;
    Frag := '';
    Next := Start + Step;
    while Next < Stop do begin
      Frag := Frag + IntToStr(Next) + ',';
      Inc(Next,Step);
    end;
    Insert(Frag,S,I+1);
  end;
  if S[Length(S)] = ',' then Delete(S,Length(S),1);
  J := 0;
  for I := 1 to Length(S) do
    if S[I] = ',' then Inc(J);
  if J = 0 then Frag := ',' + S
  else begin
    I := Length(S);
    while S[I] <> ',' do Dec(I);
    Inc(I);
    Frag := ',' + Copy(S,I,100);
  end;
  while J < Len - 1 do begin
    S := S + Frag;
    Inc(J);
  end;
  J := 1;
  for I := 1 to Len do begin
    K := J;
    while (K <= Length(S)) and (S[K] in FloatChars) do Inc(K);
    try
      RArray[I] := StrToFloat(Copy(S,J,K-J));
    except
      ShowMessage('Illegal ' + Msg + '!');
      Exit;
    end;
    J := K+1;
  end;
  S := '';
  for I := 1 to Len do begin
    if I > 1 then S := S + ',';
    S := S + IntToStr(Round(RArray[I]));
  end;
  SetParameters := S;
end;

Function UpdateParameters(RArray: Array of Real;Len: Integer) : String;
var
  S : String;
  I : Integer;
begin
  S := '';
  for I := 1 to Len do begin
    if I > 1 then S := S + ',';
    S := S + IntToStr(Round(RArray[I]));
  end;
  UpdateParameters := S;
end;

end.
