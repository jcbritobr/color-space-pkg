unit MainFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, Windows, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, LCLType, Buttons, HSV, CMY, ColorSpace, Clipbrd, MacbethColor, Printers;

type
  THSVGradient = (GrHue, GrSaturation, GrValue);

  { TFrmMainFrame }

  TFrmMainFrame = class(TForm)
    BtnToClipboard: TBitBtn;
    BtnCMYToClipboard: TBitBtn;
    BtnMacbethPrint: TButton;
    BtnMacbethSave: TButton;
    GrbCMY: TGroupBox;
    GrbRGB: TGroupBox;
    GrbHSV: TGroupBox;
    GrbColor: TGroupBox;
    GbColor: TGroupBox;
    ImgMacbethColors: TImage;
    ImgBlue: TImage;
    ImgCyan: TImage;
    ImgSaturation: TImage;
    ImgRed: TImage;
    ImgGreen: TImage;
    ImgHue: TImage;
    ImgMagenta: TImage;
    ImgValue: TImage;
    ImgYellow: TImage;
    LblMacbethColor: TLabel;
    LblBlueOutput: TLabel;
    LblCMYColorCode: TLabel;
    LblGreenOutput: TLabel;
    LblCyan: TLabel;
    LblCyanOutput: TLabel;
    LblCMYOutput: TLabel;
    LblRedOutput: TLabel;
    LblMagentaOutput: TLabel;
    LblMagenta: TLabel;
    LblYellow: TLabel;
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
    LblYellowOutput: TLabel;
    PgMain: TPageControl;
    ShpCMYColor: TShape;
    ShpColor: TShape;
    TbMacbeth: TTabSheet;
    TbHSV: TTabSheet;
    TbCMY: TTabSheet;
    TkbBlue: TTrackBar;
    TkbCyan: TTrackBar;
    TkbSaturation: TTrackBar;
    TkbRed: TTrackBar;
    TkbGreen: TTrackBar;
    TkbHue: TTrackBar;
    TkbMagenta: TTrackBar;
    TkbValue: TTrackBar;
    TkbYellow: TTrackBar;
    procedure BtnMacbethPrintClick(Sender: TObject);
    procedure BtnMacbethSaveClick(Sender: TObject);
    procedure BtnToClipboardClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImgMacbethColorsMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure OnRGBChange(Sender: TObject);
    procedure OnCMYChange(Sender: TObject);
    procedure OnRGBPress(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnRGBUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnHSVChange(Sender: TObject);
  private
    FHSVData: THSVColorSpace;
    FCMYData: TCMYColorSpace;
    FIsRGBProcessing: Boolean;
    procedure CreateRGBGradient(const RedFactor, GreenFactor, BlueFactor: Integer;
      const Image: TImage);
    procedure CreateHSVGradient(const Gradient: THSVGradient; const Image: TImage);
    procedure OnChangeHSVData(ASender: TObject);
    procedure OnChangeCMYData(ASender: TObject);
    procedure DrawMacbethChart(const ACanvas: TCanvas);
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
  FCMYData := TCMYColorSpace.Create;

  FHSVData.OnChangeValue := @OnChangeHSVData;
  FCMYData.OnChangeValue := @OnChangeCMYData;
  DrawMacbethChart(ImgMacbethColors.Canvas);

  CreateRGBGradient(1, 0, 0, ImgRed);
  CreateRGBGradient(0, 1, 0, ImgGreen);
  CreateRGBGradient(0, 0, 1, ImgBlue);

  CreateHSVGradient(GrHue, ImgHue);
  CreateHSVGradient(GrSaturation, ImgSaturation);
  CreateHSVGradient(GrValue, ImgValue);

  CreateRGBGradient(0, 1, 1, ImgCyan);
  CreateRGBGradient(1, 0, 1, ImgMagenta);
  CreateRGBGradient(1, 1, 0, ImgYellow);
end;

procedure TFrmMainFrame.ImgMacbethColorsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  LocalColor: TColor;
begin
  LocalColor := ImgMacbethColors.Canvas.Pixels[X, Y];
  if LocalColor <> clBlack then
  begin
    LblMacbethColor.Caption :=
      ColorToString(LocalColor) + Format(' RGB(%d, %d, %d)',
      [GetRValue(LocalColor), GetGValue(LocalColor), GetBValue(LocalColor)]);
  end;

end;

procedure TFrmMainFrame.BtnToClipboardClick(Sender: TObject);
var
  ColorData: TRGBTriple;
begin
  ColorData := FHSVData.ToRGBTriple;
  ClipBoard.AsText := '#' + IntToHex(ColorData.rgbtRed, 2) +
    IntToHex(ColorData.rgbtGreen, 2) + IntToHex(ColorData.rgbtBlue, 2);
end;

procedure TFrmMainFrame.BtnMacbethPrintClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    Printer.Orientation := poLandscape;
    Printer.BeginDoc;
    DrawMacbethChart(Printer.Canvas);
    Printer.EndDoc;
    ShowMessage('Macbeth color chart printed.');
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFrmMainFrame.BtnMacbethSaveClick(Sender: TObject);
begin
  if ImgMacbethColors.Picture.Jpeg <> nil then
  begin
    ImgMacbethColors.Picture.Jpeg.SaveToFile('macbethcolors.jpg');
    ShowMessage('Image saved with success');
  end;
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
      else
        ;
    end;
    FHSVData.FromRGB(ColorData.rgbtRed, ColorData.rgbtGreen, ColorData.rgbtBlue);
  end;
end;

procedure TFrmMainFrame.OnCMYChange(Sender: TObject);
begin
  if Sender <> nil then
  begin
    case (Sender as TTrackBar).Name of
      'TkbCyan': FCMYData.Cyan := (Sender as TTrackBar).Position;
      'TkbMagenta': FCMYData.Magenta := (Sender as TTrackBar).Position;
      'TkbYellow': FCMYData.Yellow := (Sender as TTrackBar).Position;
      else
        ;
    end;
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

{$PUSH}
{$WARN 5024 OFF : Parameter "$1" not used}
procedure TFrmMainFrame.OnRGBUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FIsRGBProcessing := False;
end;

{$POP}

procedure TFrmMainFrame.OnHSVChange(Sender: TObject);
begin
  if (Sender <> nil) and (not FIsRGBProcessing) then
  begin
    case (Sender as TTrackBar).Name of
      'TkbHue': FHSVData.Hue := TTrackBar(Sender).Position;
      'TkbSaturation': FHSVData.Saturation := TTrackBar(Sender).Position;
      'TkbValue': FHSVData.Value := TTrackBar(Sender).Position;
      else
        ;
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
    ColorData := (ASender as IColorSpace).ToRGBTriple;

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

    LblOutput.Caption := FHSVData.ToString;
  end;
end;

procedure TFrmMainFrame.OnChangeCMYData(ASender: TObject);
var
  ColorData: TRGBTriple;
begin
  LblCMYOutput.Caption := (ASender as TCMYColorSpace).ToString;
  ColorData := (ASender as TCMYColorSpace).ToRGBTriple;
  LblCyanOutput.Caption := (ASender as TCMYColorSpace).Cyan.ToString;
  LblMagentaOutput.Caption := (ASender as TCMYColorSpace).Magenta.ToString;
  LblYellowOutput.Caption := (ASender as TCMYColorSpace).Yellow.ToString;
  ShpCMYColor.Brush.Color := RGBToColor(ColorData.rgbtRed, ColorData.rgbtGreen,
    ColorData.rgbtBlue);
end;

procedure TFrmMainFrame.DrawMacbethChart(const ACanvas: TCanvas);
const
  ColumnCount = 6;
  RowCount = 4;
  ActualSeparator = 5;
  ActualWidth = 40;
  ActualHeight = 40;
  TotalWidth = (ColumnCount + 1) * ActualSeparator + ColumnCount * ActualWidth;
  TotalHeight = (RowCount + 1) * ActualSeparator + RowCount * ActualHeight;
var
  AHeight: Integer;
  I: Integer;
  IPixel: Integer;
  J: Integer;
  JPixel: Integer;
  PatchHeight: Integer;
  PatchIndex: Integer;
  PatchWidth: Integer;
  AWidth: Integer;
begin
  ACanvas.Brush.Color := clBlack;
  ACanvas.Rectangle(ACanvas.ClipRect.Left, ACanvas.ClipRect.Top,
    ACanvas.ClipRect.Right, ACanvas.ClipRect.Bottom);

  AWidth := ACanvas.ClipRect.Right - ACanvas.ClipRect.Left;
  AHeight := ACanvas.ClipRect.Bottom - ACanvas.ClipRect.Top;

  PatchWidth := (ActualWidth * AWidth) div TotalWidth;
  PatchHeight := (ActualHeight * AHeight) div TotalHeight;
  for PatchIndex := Low(MacbethColors) to High(MacbethColors) do
  begin
    J := (PatchIndex - 1) div ColumnCount;
    I := (PatchIndex - 1) mod ColumnCount;
    IPixel := ((ActualSeparator * (I + 1) + ActualWidth * I) * AWidth) div TotalWidth;
    JPixel := ((ActualSeparator * (J + 1) + ActualHeight * J) * AHeight) div TotalHeight;
    ACanvas.Brush.Color := MacbethColors[PatchIndex];
    ACanvas.Rectangle(IPixel, JPixel, IPixel + PatchWidth, JPixel + PatchHeight);
  end;
end;

destructor TFrmMainFrame.Destroy;
begin
  FHSVData.Free;
  FCMYData.Free;
  inherited Destroy;
end;

end.
