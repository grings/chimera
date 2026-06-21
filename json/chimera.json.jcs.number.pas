unit chimera.json.jcs.number;

interface

uses
  System.SysUtils,
  chimera.json;

type
  TJCSNumberEncoder = class
  public
    class function Format(const Value: Double): string;
  end;

implementation

uses
  System.Math;

const
  DOUBLE_MANTISSA_BITS = 52;
  DOUBLE_MANTISSA_MASK = Int64($000FFFFFFFFFFFFF);
  DOUBLE_EXPONENT_BITS = 11;
  DOUBLE_EXPONENT_MASK = $7FF;
  DOUBLE_EXPONENT_BIAS = 1023;
  POS_TABLE_SIZE = 326;
  NEG_TABLE_SIZE = 291;
  POW5_BITCOUNT = 121;
  POW5_INV_BITCOUNT = 122;

{$INCLUDE chimera.json.jcs.number.tables.inc}

type
  TDoubleBits = record
    case Integer of
      0: (Value: Double);
      1: (Bits: UInt64);
  end;

function UShr64(const Value: Int64; Shift: Integer): Int64; inline;
begin
  Result := Int64(UInt64(Value) shr Shift);
end;

function Pow5Bits(E: Integer): Integer; inline;
begin
  Result := ((E * 1217359) shr 19) + 1;
end;

function DecimalLength(const V: Int64): Integer;
begin
  if V >= 1000000000000000000 then Exit(19);
  if V >= 100000000000000000 then Exit(18);
  if V >= 10000000000000000 then Exit(17);
  if V >= 1000000000000000 then Exit(16);
  if V >= 100000000000000 then Exit(15);
  if V >= 10000000000000 then Exit(14);
  if V >= 1000000000000 then Exit(13);
  if V >= 100000000000 then Exit(12);
  if V >= 10000000000 then Exit(11);
  if V >= 1000000000 then Exit(10);
  if V >= 100000000 then Exit(9);
  if V >= 10000000 then Exit(8);
  if V >= 1000000 then Exit(7);
  if V >= 100000 then Exit(6);
  if V >= 10000 then Exit(5);
  if V >= 1000 then Exit(4);
  if V >= 100 then Exit(3);
  if V >= 10 then Exit(2);
  Result := 1;
end;

function Pow5Factor(Value: Int64): Integer;
var
  Temp: Int64;
begin
  Temp := Value;
  Result := 0;
  if (Temp mod 5) <> 0 then Exit;
  if (Temp mod 25) <> 0 then Exit(1);
  if (Temp mod 125) <> 0 then Exit(2);
  if (Temp mod 625) <> 0 then Exit(3);
  Result := 4;
  Temp := Temp div 625;
  while Temp > 0 do
  begin
    if (Temp mod 5) <> 0 then
      Exit;
    Temp := Temp div 5;
    Inc(Result);
  end;
end;

function MultipleOfPowerOf5(Value: Int64; Q: Integer): Boolean;
begin
  Result := Pow5Factor(Value) >= Q;
end;

function MulPow5DivPow2(M: Int64; I, J: Integer): Int64;
var
  MHigh, MLow: Int64;
  Bits13, Bits03, Bits12, Bits02, Bits11, Bits01, Bits10, Bits00: Int64;
  ActualShift: Integer;
begin
  MHigh := UShr64(M, 31);
  MLow := M and $7FFFFFFF;
  Bits13 := MHigh * POW5_SPLIT[I][0];
  Bits03 := MLow * POW5_SPLIT[I][0];
  Bits12 := MHigh * POW5_SPLIT[I][1];
  Bits02 := MLow * POW5_SPLIT[I][1];
  Bits11 := MHigh * POW5_SPLIT[I][2];
  Bits01 := MLow * POW5_SPLIT[I][2];
  Bits10 := MHigh * POW5_SPLIT[I][3];
  Bits00 := MLow * POW5_SPLIT[I][3];
  ActualShift := J - 3 * 31 - 21;
  if ActualShift < 0 then
    raise EChimeraJSONException.Create('Invalid shift in number serializer');
  Result := UShr64(
    UShr64(
      UShr64(
        UShr64(
          UShr64(Bits00, 31) + Bits01 + Bits10, 31) + Bits02 + Bits11, 31) + Bits03 + Bits12, 21) +
    (Bits13 shl 10), ActualShift);
end;

function MulPow5InvDivPow2(M: Int64; I, J: Integer): Int64;
var
  MHigh, MLow: Int64;
  Bits13, Bits03, Bits12, Bits02, Bits11, Bits01, Bits10, Bits00: Int64;
  ActualShift: Integer;
begin
  MHigh := UShr64(M, 31);
  MLow := M and $7FFFFFFF;
  Bits13 := MHigh * POW5_INV_SPLIT[I][0];
  Bits03 := MLow * POW5_INV_SPLIT[I][0];
  Bits12 := MHigh * POW5_INV_SPLIT[I][1];
  Bits02 := MLow * POW5_INV_SPLIT[I][1];
  Bits11 := MHigh * POW5_INV_SPLIT[I][2];
  Bits01 := MLow * POW5_INV_SPLIT[I][2];
  Bits10 := MHigh * POW5_INV_SPLIT[I][3];
  Bits00 := MLow * POW5_INV_SPLIT[I][3];
  ActualShift := J - 3 * 31 - 21;
  if ActualShift < 0 then
    raise EChimeraJSONException.Create('Invalid shift in number serializer');
  Result := UShr64(
    UShr64(
      UShr64(
        UShr64(
          UShr64(Bits00, 31) + Bits01 + Bits10, 31) + Bits02 + Bits11, 31) + Bits03 + Bits12, 21) +
    (Bits13 shl 10), ActualShift);
end;

function SerializeDoubleCore(const Value: Double): string;
var
  Bits: TDoubleBits;
  IeeeExponent: Integer;
  IeeeMantissa: Int64;
  E2: Integer;
  M2: Int64;
  Sign: Boolean;
  Even: Boolean;
  Mv, Mp, Mm: Int64;
  MmShift: Integer;
  Dv, Dp, Dm: Int64;
  E10: Integer;
  DmIsTrailingZeros, DvIsTrailingZeros: Boolean;
  VpLength, Exp, Removed, LastRemovedDigit: Integer;
  ScientificNotation: Boolean;
  Output: Int64;
  OLength: Integer;
  ResultChars: array[0..24] of Char;
  Index, Current, C, I: Integer;
  Q, K, J: Integer;
begin
  Bits.Value := Value;
  IeeeExponent := Integer(UShr64(Int64(Bits.Bits), DOUBLE_MANTISSA_BITS) and DOUBLE_EXPONENT_MASK);
  IeeeMantissa := Int64(Bits.Bits) and DOUBLE_MANTISSA_MASK;
  if IeeeExponent = 0 then
  begin
    E2 := 1 - DOUBLE_EXPONENT_BIAS - DOUBLE_MANTISSA_BITS;
    M2 := IeeeMantissa;
  end
  else
  begin
    E2 := IeeeExponent - DOUBLE_EXPONENT_BIAS - DOUBLE_MANTISSA_BITS;
    M2 := IeeeMantissa or (Int64(1) shl DOUBLE_MANTISSA_BITS);
  end;

  Sign := Int64(Bits.Bits) < 0;
  Even := (M2 and 1) = 0;
  Mv := 4 * M2;
  Mp := 4 * M2 + 2;
  if (M2 <> (Int64(1) shl DOUBLE_MANTISSA_BITS)) or (IeeeExponent <= 1) then
    MmShift := 1
  else
    MmShift := 0;
  Mm := 4 * M2 - 1 - MmShift;
  Dec(E2, 2);

  DmIsTrailingZeros := False;
  DvIsTrailingZeros := False;
  if E2 >= 0 then
  begin
    Q := Max(0, ((E2 * 78913) shr 18) - 1);
    K := POW5_INV_BITCOUNT + Pow5Bits(Q) - 1;
    I := -E2 + Q + K;
    Dv := MulPow5InvDivPow2(Mv, Q, I);
    Dp := MulPow5InvDivPow2(Mp, Q, I);
    Dm := MulPow5InvDivPow2(Mm, Q, I);
    E10 := Q;
    if Q <= 21 then
    begin
      if (Mv mod 5) = 0 then
        DvIsTrailingZeros := MultipleOfPowerOf5(Mv, Q)
      else if Even then
        DmIsTrailingZeros := MultipleOfPowerOf5(Mm, Q)
      else if MultipleOfPowerOf5(Mp, Q) then
        Dec(Dp);
    end;
  end
  else
  begin
    Q := Max(0, ((-E2 * 732923) shr 20) - 1);
    I := -E2 - Q;
    K := Pow5Bits(I) - POW5_BITCOUNT;
    J := Q - K;
    Dv := MulPow5DivPow2(Mv, I, J);
    Dp := MulPow5DivPow2(Mp, I, J);
    Dm := MulPow5DivPow2(Mm, I, J);
    E10 := Q + E2;
    if Q <= 1 then
    begin
      DvIsTrailingZeros := True;
      if Even then
        DmIsTrailingZeros := MmShift = 1
      else
        Dec(Dp);
    end
    else if Q < 63 then
      DvIsTrailingZeros := (Mv and ((Int64(1) shl (Q - 1)) - 1)) = 0;
  end;

  VpLength := DecimalLength(Dp);
  Exp := E10 + VpLength - 1;
  ScientificNotation := not ((Exp >= -6) and (Exp < 21));
  Removed := 0;
  LastRemovedDigit := 0;

  if DmIsTrailingZeros or DvIsTrailingZeros then
  begin
    while (Dp div 10) > (Dm div 10) do
    begin
      DmIsTrailingZeros := DmIsTrailingZeros and ((Dm mod 10) = 0);
      DvIsTrailingZeros := DvIsTrailingZeros and (LastRemovedDigit = 0);
      LastRemovedDigit := Integer(Dv mod 10);
      Dp := Dp div 10;
      Dv := Dv div 10;
      Dm := Dm div 10;
      Inc(Removed);
    end;
    if DmIsTrailingZeros and Even then
    begin
      while (Dm mod 10) = 0 do
      begin
        DvIsTrailingZeros := DvIsTrailingZeros and (LastRemovedDigit = 0);
        LastRemovedDigit := Integer(Dv mod 10);
        Dp := Dp div 10;
        Dv := Dv div 10;
        Dm := Dm div 10;
        Inc(Removed);
      end;
    end;
    if DvIsTrailingZeros and (LastRemovedDigit = 5) and ((Dv mod 2) = 0) then
      LastRemovedDigit := 4;
    Output := Dv + IfThen((Dv = Dm) and not (DmIsTrailingZeros and Even) or (LastRemovedDigit >= 5), 1, 0);
  end
  else
  begin
    while (Dp div 10) > (Dm div 10) do
    begin
      LastRemovedDigit := Integer(Dv mod 10);
      Dp := Dp div 10;
      Dv := Dv div 10;
      Dm := Dm div 10;
      Inc(Removed);
    end;
    Output := Dv + IfThen((Dv = Dm) or (LastRemovedDigit >= 5), 1, 0);
  end;

  OLength := VpLength - Removed;
  Index := 0;
  if Sign then
  begin
    ResultChars[Index] := '-';
    Inc(Index);
  end;

  if ScientificNotation then
  begin
    for I := 0 to OLength - 2 do
    begin
      C := Integer(Output mod 10);
      Output := Output div 10;
      ResultChars[Index + OLength - I] := Char(Ord('0') + C);
    end;
    ResultChars[Index] := Char(Ord('0') + Integer(Output mod 10));
    if OLength > 1 then
      ResultChars[Index + 1] := '.'
    else
      Dec(Index);
    Inc(Index, OLength + 1);

    ResultChars[Index] := 'e';
    Inc(Index);
    if Exp < 0 then
    begin
      ResultChars[Index] := '-';
      Inc(Index);
      Exp := -Exp;
    end
    else
    begin
      ResultChars[Index] := '+';
      Inc(Index);
    end;
    if Exp >= 100 then
    begin
      ResultChars[Index] := Char(Ord('0') + (Exp div 100));
      Inc(Index);
      Exp := Exp mod 100;
      ResultChars[Index] := Char(Ord('0') + (Exp div 10));
      Inc(Index);
    end
    else if Exp >= 10 then
    begin
      ResultChars[Index] := Char(Ord('0') + (Exp div 10));
      Inc(Index);
    end;
    ResultChars[Index] := Char(Ord('0') + (Exp mod 10));
    Inc(Index);
  end
  else if Exp < 0 then
  begin
    ResultChars[Index] := '0';
    Inc(Index);
    ResultChars[Index] := '.';
    Inc(Index);
    for I := -1 downto Exp + 1 do
    begin
      ResultChars[Index] := '0';
      Inc(Index);
    end;
    Current := Index;
    for I := 0 to OLength - 1 do
    begin
      ResultChars[Current + OLength - I - 1] := Char(Ord('0') + Integer(Output mod 10));
      Output := Output div 10;
      Inc(Index);
    end;
  end
  else if Exp + 1 >= OLength then
  begin
    for I := 0 to OLength - 1 do
    begin
      ResultChars[Index + OLength - I - 1] := Char(Ord('0') + Integer(Output mod 10));
      Output := Output div 10;
    end;
    Inc(Index, OLength);
    for I := OLength to Exp do
    begin
      ResultChars[Index] := '0';
      Inc(Index);
    end;
  end
  else
  begin
    Current := Index + 1;
    for I := 0 to OLength - 1 do
    begin
      if (OLength - I - 1) = Exp then
      begin
        ResultChars[Current + OLength - I - 1] := '.';
        Dec(Current);
      end;
      ResultChars[Current + OLength - I - 1] := Char(Ord('0') + Integer(Output mod 10));
      Output := Output div 10;
    end;
    Inc(Index, OLength + 1);
  end;

  SetString(Result, PChar(@ResultChars[0]), Index);
end;

class function TJCSNumberEncoder.Format(const Value: Double): string;
var
  Bits: TDoubleBits;
begin
  if IsNan(Value) then
    raise EChimeraJSONException.Create('NaN is not permitted in JSON');
  if IsInfinite(Value) then
    raise EChimeraJSONException.Create('Infinity is not permitted in JSON');

  Bits.Value := Value;
  if (Bits.Bits and $7FFFFFFFFFFFFFFF) = 0 then
    Exit('0');

  Result := SerializeDoubleCore(Value);
end;

end.
