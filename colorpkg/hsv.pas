unit HSV;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Math, ColorSpace;

type
  { THSVColorSpace }

  THSVColorSpace = class(IColorSpace)
  private
    FHue: Integer;
    FSaturation: Integer;
    FOnChangeValue: TChangeValueEvent;
    FValue: Integer;
    procedure SetHue(AHue: Integer);
    procedure SetSaturation(ASaturation: Integer);
    procedure SetValue(AValue: Integer);
    function GetHue: Integer;
    function GetSaturation: Integer;
    function GetValue: Integer;
    function CreateRGBTriple(const ARed, AGreen, ABlue: Byte): TRGBTriple;
  public
    constructor Create;
    constructor Create(AHue, ASaturation, AValue: Integer);
    function ToRGBTriple: TRGBTriple;
    procedure FromRGB(const ARed, AGreen, ABlue: Byte);
    property Hue: Integer read GetHue write SetHue;
    property Saturation: Integer read GetSaturation write SetSaturation;
    property Value: Integer read GetValue write SetValue;
    property OnChangeValue: TChangeValueEvent read FOnChangeValue write FOnChangeValue;
    function ToString: Ansistring; override;
  end;

implementation

{ THSVColorSpace }

procedure THSVColorSpace.SetHue(AHue: Integer);
begin
  if FHue <> AHue then
  begin
    FHue := AHue;
    FOnChangeValue(Self);
  end;
end;

procedure THSVColorSpace.SetSaturation(ASaturation: Integer);
begin
  if FSaturation <> ASaturation then
  begin
    FSaturation := ASaturation;
    FOnChangeValue(Self);
  end;
end;

procedure THSVColorSpace.SetValue(AValue: Integer);
begin
  if FValue <> AValue then
  begin
    FValue := AValue;
    FOnChangeValue(Self);
  end;

end;

function THSVColorSpace.GetHue: Integer;
begin
  Result := FHue;
end;

function THSVColorSpace.GetSaturation: Integer;
begin
  Result := FSaturation;
end;

function THSVColorSpace.GetValue: Integer;
begin
  Result := FValue;
end;

function THSVColorSpace.CreateRGBTriple(const ARed, AGreen, ABlue: Byte): TRGBTriple;
begin
  with Result do
  begin
    rgbtRed := ARed;
    rgbtGreen := AGreen;
    rgbtBlue := ABlue;
  end;
end;

constructor THSVColorSpace.Create;
begin
  FHue := 0;
  FSaturation := 0;
  FValue := 0;
end;

constructor THSVColorSpace.Create(AHue, ASaturation, AValue: Integer);
begin
  FHue := AHue;
  FSaturation := ASaturation;
  FValue := AValue;
end;

function THSVColorSpace.ToRGBTriple: TRGBTriple;
const
  Divisor: Integer = 255 * 60;
var
  F: Integer;
  HTemp: Integer;
  P, Q, T: Integer;
  VS: Integer;
begin
  if FSaturation = 0 then
    Result := CreateRGBTriple(FValue, FValue, FValue)
  else
  begin
    if FHue = 360 then
      HTemp := 0
    else
      HTemp := FHue;
    F := HTemp mod 60;
    HTemp := HTemp div 60;
    VS := FValue * FSaturation;
    P := FValue - VS div 255;
    Q := FValue - (VS * F) div divisor;
    T := FValue - (VS * (60 - F)) div divisor;

    case HTemp of
      0: Result := CreateRGBTriple(FValue, T, P);
      1: Result := CreateRGBTriple(Q, FValue, P);
      2: Result := CreateRGBTriple(P, FValue, T);
      3: Result := CreateRGBTriple(P, Q, FValue);
      4: Result := CreateRGBTriple(T, P, FValue);
      5: Result := CreateRGBTriple(FValue, P, Q);
      else
        Result := CreateRGBTriple(0, 0, 0);
    end;
  end;
end;

procedure THSVColorSpace.FromRGB(const ARed, AGreen, ABlue: Byte);
var
  ColorData: TRGBTriple;
  Delta: Integer;
  Min: Integer;
begin
  ColorData:= CreateRGBTriple(ARed, AGreen, ABlue);
  Min := MinValue([ColorData.rgbtRed, ColorData.rgbtGreen, ColorData.rgbtBlue]);
  FValue := MaxValue([ColorData.rgbtRed, ColorData.rgbtGreen, ColorData.rgbtBlue]);

  Delta := FValue - Min;

  // Calculate the saturation: saturation is 0, if all triplets(r, g, b) are 0
  if FValue = 0.0 then
    FSaturation := 0
  else
    FSaturation := MulDiv(Delta, 255, FValue);

  if FSaturation = 0.0 then
    FHue := 0 // Achromatic: When Saturation = 0, Hue is undefined
  else
  begin // Chromatic
    with ColorData do
    begin
      if rgbtRed = FValue then // Degrees between yellow and magenta
        FHue := MulDiv(rgbtGreen - rgbtBlue, 60, Delta)
      else
      if rgbtGreen = FValue then // Degrees between cyano and yellow
        FHue := 120 + MulDiv(rgbtBlue - rgbtRed, 60, Delta)
      else
      if rgbtBlue = FValue then
        FHue := 240 + MulDiv(rgbtRed - rgbtGreen, 60, Delta);
    end;
    if FHue < 0 then
      FHue := FHue + 360;
  end;
  FOnChangeValue(Self);
end;

function THSVColorSpace.ToString: Ansistring;
begin
  Result := Format('H:%d, S:%d, V:%d', [FHue, FSaturation, FValue]);
end;


end.
