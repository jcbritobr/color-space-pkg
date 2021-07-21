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
    LblBlueOutput: TLabel;
    LblGreenOutput: TLabel;
    LblRedOutput: TLabel;
    LblValueOutput: TLabel;
    LblSatOutput: TLabel;
    LblHueOutput: TLabel;
    LblOutput: TLabel;
    LblColorCode: TLabel;
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
    procedure OnRGBChange(Sender: TObject);
    procedure OnRGBPress(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnRGBUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnHSVChange(Sender: TObject);
  private
    FHSVData: THSVColorSpace;
    FIsRGBProcessing: Boolean;
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
  FIsRGBProcessing := False;
  FHSVData := THSVColorSpace.Create;
  FHSVData.OnChangeValue := @OnChangeHSVData;
  CreateRGBGradient(1, 0, 0, ImgRed);
  CreateRGBGradient(0, 1, 0, ImgGreen);
  CreateRGBGradient(0, 0, 1, ImgBlue);
  CreateHSVGradient(GrHue, ImgHue);
  CreateHSVGradient(GrSaturation, ImgSaturation);
  CreateHSVGradient(GrValue, ImgValue);
end;

procedure TFrmMainFrame.OnRGBChange(Sender: TObject);
var
  ColorData: TRGBTriple;
begin
  if (Sender <> nil) and (FIsRGBProcessing) then
  begin
    ColorData := FHSVData.ToRGBTriple;
    case (Sender as TTrackBar).Name of
      'TkbRed': ColorData.rgbtRed := TTrackBar(Sender).Position;
      'TkbGreen': ColorData.rgbtGreen := TTrackBar(Sender).Position;
      'TkbBlue': ColorData.rgbtBlue := TTrackBar(Sender).Position;
    end;
    FHSVData.FromRGBTriple(ColorData);
  end;
end;

{$PUSH}
{$WARN 5024 OFF : Parameter "$1" not used}
procedure TFrmMainFrame.OnRGBPress(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FIsRGBProcessing := True;
end;

{$POP}

procedure TFrmMainFrame.OnRGBUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FIsRGBProcessing := False;
end;

procedure TFrmMainFrame.OnHSVChange(Sender: TObject);
begin
  if (Sender <> nil) and (not FIsRGBProcessing) then
  begin
    case (Sender as TTrackBar).Name of
      'TkbHue': FHSVData.Hue := TTrackBar(Sender).Position;
      'TkbSaturation': FHSVData.Saturation := TTrackBar(Sender).Position;
      'TkbValue': FHSVData.Value := TTrackBar(Sender).Position;
    end;
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

    TkbHue.Position := FHSVData.Hue;
    TkbSaturation.Position := FHSVData.Saturation;
    TkbValue.Position := FHSVData.Value;

    if not FIsRGBProcessing then
    begin
      TkbRed.Position := ColorData.rgbtRed;
      TkbGreen.Position := ColorData.rgbtGreen;
      TkbBlue.Position := ColorData.rgbtBlue;
    end;


    ShpColor.Brush.Color := RGBToColor(ColorData.rgbtRed, ColorData.rgbtGreen,
      ColorData.rgbtBlue);

    LblHueOutput.Caption := FHSVData.Hue.ToString;
    LblSatOutput.Caption := FHSVData.Saturation.ToString;
    LblValueOutput.Caption := FHSVData.Value.ToString;

    LblRedOutput.Caption := ColorData.rgbtRed.ToString;
    LblGreenOutput.Caption := ColorData.rgbtGreen.ToString;
    LblBlueOutput.Caption := ColorData.rgbtBlue.ToString;

    LblOutput.Caption := ColorToString(ShpColor.Brush.Color);
  end;
end;

destructor TFrmMainFrame.Destroy;
begin
  FHSVData.Free;
  inherited Destroy;
end;

end.
