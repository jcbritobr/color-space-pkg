unit MainFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, LCLType, HSV;

type
  THSVGradient = (GrHue, GrSaturation, GrValue);

  { TFrmMainFrame }

  TFrmMainFrame = class(TForm)
    GrbRGB: TGroupBox;
    GrbHSV: TGroupBox;
    GrbColor: TGroupBox;
    ImgBlue: TImage;
    ImgSaturation: TImage;
    ImgRed: TImage;
    ImgGreen: TImage;
    ImgHue: TImage;
    ImgValue: TImage;
    LblOutput: TLabel;
    LblOutputColor: TLabel;
    LblBlue: TLabel;
    LblSaturation: TLabel;
    LblRed: TLabel;
    LblGreen: TLabel;
    LblHue: TLabel;
    LblValue: TLabel;
    ShpColor: TShape;
    TkbBlue: TTrackBar;
    TkbSaturation: TTrackBar;
    TkbRed: TTrackBar;
    TkbGreen: TTrackBar;
    TkbHue: TTrackBar;
    TkbValue: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure TkbBlueChange(Sender: TObject);
    procedure TkbGreenChange(Sender: TObject);
    procedure TkbRedChange(Sender: TObject);
  private
    FHSVData: THSVColorSpace;
    procedure CreateRGBGradient(const RedFactor, GreenFactor, BlueFactor: Integer;
      const Image: TImage);
    procedure CreateHSVGradient(const Gradient: THSVGradient; const Image: TImage);
    procedure OnChangeHSVData(ASender: TObject);
  public
    destructor Destroy; override;

  end;

var
  FrmMainFrame: TFrmMainFrame;

implementation

{$R *.lfm}

{ TFrmMainFrame }

procedure TFrmMainFrame.FormCreate(Sender: TObject);
begin
  FHSVData := THSVColorSpace.Create;
  FHSVData.OnChangeValue := @OnChangeHSVData;
  CreateRGBGradient(1, 0, 0, ImgRed);
  CreateRGBGradient(0, 1, 0, ImgGreen);
  CreateRGBGradient(0, 0, 1, ImgBlue);
  CreateHSVGradient(GrHue, ImgHue);
  CreateHSVGradient(GrSaturation, ImgSaturation);
  CreateHSVGradient(GrValue, ImgValue);
end;

procedure TFrmMainFrame.TkbBlueChange(Sender: TObject);
var
  ColorData: TRGBTriple;
begin
  if Sender <> nil then
  begin
    ColorData.rgbtBlue := TTrackBar(Sender).Position;
    ColorData.rgbtRed := TkbRed.Position;
    ColorData.rgbtGreen := TkbGreen.Position;
    FHSVData.FromRGBTriple(ColorData);
  end;
end;

procedure TFrmMainFrame.TkbGreenChange(Sender: TObject);
var
  ColorData: TRGBTriple;
begin
  if Sender <> nil then
  begin
    ColorData.rgbtGreen := TTrackBar(Sender).Position;
    ColorData.rgbtRed := TkbRed.Position;
    ColorData.rgbtBlue := TkbBlue.Position;
    FHSVData.FromRGBTriple(ColorData);
  end;

end;

procedure TFrmMainFrame.TkbRedChange(Sender: TObject);
var
  ColorData: TRGBTriple;
begin
  if Sender <> nil then
  begin
    ColorData.rgbtRed := TTrackBar(Sender).Position;
    ColorData.rgbtGreen := TkbGreen.Position;
    ColorData.rgbtBlue := TkbBlue.Position;
    FHSVData.FromRGBTriple(ColorData);
  end;

end;

procedure TFrmMainFrame.CreateRGBGradient(
  const RedFactor, GreenFactor, BlueFactor: Integer; const Image: TImage);
var
  Bitmap: TBitmap;
  I: Integer;
  J: Integer;
  Row: PRGBTriple;
  Data: TRGBTriple;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf24bit;
    Bitmap.Width := Image.Width;
    Bitmap.Height := Image.Height;

    for J := 0 to Bitmap.Height - 1 do
    begin
      Row := Bitmap.ScanLine[j];
      for I := 0 to Bitmap.Width - 1 do
      begin
        Data.rgbtRed := RedFactor * i;
        Data.rgbtGreen := GreenFactor * i;
        Data.rgbtBlue := BlueFactor * i;
        Row[i] := Data;
      end;
    end;
    Image.Picture.Graphic := Bitmap;
  finally
    Bitmap.Free;
  end;
end;

procedure TFrmMainFrame.CreateHSVGradient(const Gradient: THSVGradient;
  const Image: TImage);
var
  Bitmap: TBitmap;
  I, J: Integer;
  Row: PRGBTriple;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf24bit;
    Bitmap.Width := Image.Width;
    Bitmap.Height := Image.Height;

    for J := 0 to Bitmap.Height - 1 do
    begin
      Row := Bitmap.ScanLine[J];
      for I := 0 to Bitmap.Width - 1 do
      begin
        case Gradient of
          GrHue:
          begin
            FHSVData.Hue := MulDiv(360, I, 255);
            FHSVData.Saturation := 255;
            FHSVData.Value := 255;
            Row[i] := FHSVData.ToRGBTriple;
          end;
          GrSaturation:
          begin
            FHSVData.Hue := 360;
            FHSVData.Saturation := I;
            FHSVData.Value := 255;
            Row[i] := FHSVData.ToRGBTriple;
          end;
          GrValue:
          begin
            FHSVData.Hue := 360;
            FHSVData.Saturation := 255;
            FHSVData.Value := I;
            Row[i] := FHSVData.ToRGBTriple;
          end;
        end;
      end;
    end;
    Image.Picture.Graphic := Bitmap;
  finally
    Bitmap.Free;
  end;
end;

procedure TFrmMainFrame.OnChangeHSVData(ASender: TObject);
var
  ColorData: TRGBTriple;
begin
  if ASender <> nil then
  begin
    ColorData := THSVColorSpace(ASender).ToRGBTriple;
    ShpColor.Brush.Color := RGBToColor(ColorData.rgbtRed, ColorData.rgbtGreen,
      ColorData.rgbtBlue);
    TkbHue.Position:= FHSVData.Hue;
    TkbSaturation.Position:=FHSVData.Saturation;
    TkbValue.Position:=FHSVData.Value;
    LblOutput.Caption := ColorToString(ShpColor.Brush.Color);
  end;
end;

destructor TFrmMainFrame.Destroy;
begin
  FHSVData.Free;
  inherited Destroy;
end;

end.
