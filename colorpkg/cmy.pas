unit CMY;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, ColorSpace;

type
  { TCMYColorSpace }

  TCMYColorSpace = class(IColorSpace)
  private
    FCyan: Byte;
    FMagenta: Byte;
    FYellow: Byte;
    FOnChangeValue: TChangeValueEvent;
    procedure SetCyan(ACyan: Byte);
    procedure SetMagenta(AMagenta: Byte);
    procedure SetYellow(AYellow: Byte);
    function GetCyan: Byte;
    function GetMagenta: Byte;
    function GetYellow: Byte;
  public
    constructor Create;
    constructor Create(ACyan, AMagenta, AYellow: Byte);
    function ToRGBTriple: TRGBTriple;
    procedure FromRGB(const ARed, AGreen, ABlue: Byte);
    function ToString: Ansistring; override;
    property Cyan: Byte read GetCyan write SetCyan;
    property Magenta: Byte read GetMagenta write SetMagenta;
    property Yellow: Byte read GetYellow write SetYellow;
    property OnChangeValue: TChangeValueEvent read FOnChangeValue write FOnChangeValue;
  end;

implementation

{ TCMYColorSpace }

procedure TCMYColorSpace.SetCyan(ACyan: Byte);
begin
  if (FOnChangeValue <> nil) and (FCyan <> ACyan) then
  begin
    FCyan := ACyan;
    FOnChangeValue(Self);
  end;
end;

procedure TCMYColorSpace.SetMagenta(AMagenta: Byte);
begin

  if (FOnChangeValue <> nil) and (FMagenta <> AMagenta) then
  begin
    FMagenta := AMagenta;
    FOnChangeValue(Self);
  end;
end;

procedure TCMYColorSpace.SetYellow(AYellow: Byte);
begin
  if (FOnChangeValue <> nil) and (FYellow <> AYellow) then
  begin
    FYellow := AYellow;
    FOnChangeValue(Self);
  end;
end;

function TCMYColorSpace.GetCyan: Byte;
begin
  Result := FCyan;
end;

function TCMYColorSpace.GetMagenta: Byte;
begin
  Result := FMagenta;
end;

function TCMYColorSpace.GetYellow: Byte;
begin
  Result := FYellow;
end;

constructor TCMYColorSpace.Create;
begin
  FCyan := 0;
  FMagenta := 0;
  FYellow := 0;
end;

constructor TCMYColorSpace.Create(ACyan, AMagenta, AYellow: Byte);
begin
  FCyan := ACyan;
  FMagenta := AMagenta;
  FYellow := AYellow;
end;

function TCMYColorSpace.ToRGBTriple: TRGBTriple;
begin
  Result.rgbtRed := 255 - FCyan;
  Result.rgbtGreen := 255 - FMagenta;
  Result.rgbtBlue := 255 - FYellow;
end;

procedure TCMYColorSpace.FromRGB(const ARed, AGreen, ABlue: Byte);
begin
  FCyan := 255 - ARed;
  FMagenta := 255 - AGreen;
  FYellow := 255 - ABlue;
end;

function TCMYColorSpace.ToString: Ansistring;
begin
  Result := Format('Cyan:%d, Magenta:%d, Yellow: %d', [FCyan, FMagenta, FYellow]);
end;

end.
