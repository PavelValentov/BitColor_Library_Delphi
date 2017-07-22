(*
  BitColorLibrary for Delphi - Simple bit colors images

  The MIT License (MIT)
  Copyright (c) 2017 PavelValentov

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
*)

unit BitColors;

interface

uses
  System.Generics.Collections,
  System.Types,
  System.Classes,
  FMX.Graphics,
  VCL.Graphics,
  System.UITypes;

const
  // CABlack  = $FF000000;
  // CAWhite  = $FFFFFFFF;
  // CARed    = $FFFF0000;
  // CAGreen  = $FF00FF00;
  // CABlue   = $FF0000FF;
  // CAYellow = $FFFFFF00;
  // CAPink   = $FFFF00FF;
  // CACyan   = $FF00FFFF;
  // CBlack  = $00;
  // CWhite  = $07;
  // CRed    = $01;
  // CGreen  = $02;
  // CBlue   = $03;
  // CYellow = $04;
  // CPink   = $05;
  // CCyan   = $06;
  acBlack   = $FF000000;
  acWhite   = $FFFFFFFF;
  acRed     = $FFFF0000;
  acGreen   = $FF00FF00;
  acBlue    = $FF0000FF;
  acYellow  = $FFFFFF00;
  acMagenta = $FFFF00FF;
  acCyan    = $FF00FFFF;

  bcBlack     = $00;
  bcWhite     = $3F;
  bcGray111   = $15;
  bcGray222   = $2A;
  bcGray113   = $17;
  bcGray131   = $1D;
  bcGray133   = $1F;
  bcGray313   = $37;
  bcGray331   = $3D;
  bcGray311   = $35;
  bcRed       = $30;
  bcRed2      = $20;
  bcRed1      = $10;
  bcGreen     = $0C;
  bcGreen2    = $08;
  bcGreen1    = $04;
  bcBlue      = $03;
  bcBlue2     = $02;
  bcBlue1     = $01;
  bcYellow    = $3C;
  bcYellow31  = $34;
  bcYellow13  = $1C;
  bcYellow11  = $14;
  bcYellow22  = $28;
  bcYellow12  = $18;
  bcYellow21  = $24;
  bcMagenta   = $33;
  bcMagenta11 = $11;
  bcMagenta22 = $22;
  bcMagenta13 = $13;
  bcMagenta31 = $31;
  bcMagenta12 = $12;
  bcMagenta21 = $21;
  bcCyan      = $0F;
  bcCyan11    = $05;
  bcCyan22    = $0A;
  bcCyan31    = $0D;
  bcCyan13    = $07;
  bcCyan21    = $09;
  bcCyan12    = $06;

type
  TDataArray = array of byte;
  PDataArray = ^TDataArray;

  RFringe = Record
    isR: boolean;
    isG: boolean;
    isB: boolean;
    redLowFringe: byte;
    redHiFringe: byte;
    greenLowFringe: byte;
    greenHiFringe: byte;
    blueLowFringe: byte;
    blueHiFringe: byte;
  public
    constructor create(aLF, aHF: byte; aR, aG, aB: boolean); overload;
    constructor create(aRLF, aRHF, aGLF, aGHF, aBLF, aBHF: byte; aR, aG, aB: boolean); overload;
  end;

  // EWColor = (wcBlack = $00, wcWhite = $3F, wcGray111 = $15, wcGray222 = $2A, wcGray113 = $17, wcGray131 = $1D,
  // wcGray133 = $1F, wcGray313 = $37, wcGray331 = $3D, wcGray311 = $35, wcRed = $30, wcRed2 = $20, wcRed1 = $10,
  // wcGreen = $0C, wcGreen2 = $08, wcGreen1 = $04, wcBlue = $03, wcBlue2 = $02, wcBlue1 = $01, wcYellow = $3C,
  // wcYellow31 = $34, wcYellow13 = $1C, wcYellow11 = $14, wcYellow22 = $28, wcYellow12 = $18, wcYellow21 = $24,
  // wcMagenta = $33, wcMagenta11 = $11, wcMagenta22 = $22, wcMagenta13 = $13, wcMagenta31 = $31, wcMagenta12 = $12,
  // wcMagenta21 = $21, wcCyan = $0F, wcCyan11 = $05, wcCyan22 = $0A, wcCyan31 = $0D, wcCyan13 = $07, wcCyan21 = $09,
  // wcCyan12 = $06);
  TBitColors = set of byte;
  // TBitColorArray = array of EWColor;

  // EBitColor = (bcBlack = $00, bcWhite = $07, bcRed = $01, bcGreen = $02, bcBlue = $03, bcYellow = $04, bcPink = $05,
  // bcCyan = $06);
  // TBitColors = set of EBitColor;

  RBitColorRect = Record
    Rect: Trect;
    Colors: TDataArray;
    Quantity: integer;
    Pixels: integer;
  End;

  RBitColorOverlap = Record
    X: word;            // X координата в пр€моугольнике, где найден паттерн
    Y: word;            // X координата в пр€моугольнике, где найден паттерн
    Color: double;      // % совпадений весомых цветов
    Black: double;      // % совпадений черного цвета
    Miss: double;       // % несовпадений цветов
    Dispersion: double; // положительное число в %, возможное отклонение при поиске цвета

    procedure SetThresholds(aColor, aBlack, aDisp, aMiss: double);
  End;

  RBitColorPoint = Record
    X: word;
    Y: word;
    C: byte;
    function A: TAlphaColor;
  End;

  TBitmap8 = class(TObject)
  private
    FData             : TDataArray;
    FMeanMap          : TArray<RBitColorPoint>;
    FHeight           : word;
    FWidth            : word;
    FRedCount         : cardinal;
    FGreenCount       : cardinal;
    FBlueCount        : cardinal;
    FFRinge           : RFringe;
    FPatternColors    : TDataArray;
    FPatternRect      : TrectF;
    FPatternThresholds: RBitColorOverlap;
    FSourceBitmap     : FMX.Graphics.TBitmap;
    function RasterByteColors(aFringe: RFringe; const aR, aG, aB: byte; IncCounter: boolean): byte;
  public
    property Height           : word read FHeight;
    property Width            : word read FWidth;
    property Data             : TDataArray read FData;
    property MeanMap          : TArray<RBitColorPoint> read FMeanMap;
    property Fringe           : RFringe read FFRinge write FFRinge;
    property RedCount         : cardinal read FRedCount;
    property GreenCount       : cardinal read FGreenCount;
    property BlueCount        : cardinal read FBlueCount;
    property PatternColors    : TDataArray read FPatternColors write FPatternColors;
    property PatternRect      : TrectF read FPatternRect write FPatternRect;
    property PatternThresholds: RBitColorOverlap read FPatternThresholds write FPatternThresholds;
    property SourceBitmap     : FMX.Graphics.TBitmap read FSourceBitmap;

    constructor create(aWidth, aHeight: cardinal; aFringe: RFringe); overload;
    constructor create(aBitmap: FMX.Graphics.TBitmap; aFringe: RFringe); overload;
    constructor create(aBitmap: FMX.Graphics.TBitmap; aFringe: RFringe; ARect: Trect); overload;
    constructor create(aStream: TStream); overload;
    destructor Destroy; override;

    function AlphaToColor(aColor: TAlphaColor): byte;
    procedure Clear;
    procedure Empty;
    function ColorToAlpha(aColor: byte): TAlphaColor;
    procedure DrawMean;

    function CopyBitmap8(var oBitmap8: TBitmap8): boolean;
    procedure GetBitmap(oBitmap: FMX.Graphics.TBitmap); overload;
    procedure GetBitmap(oBitmap: FMX.Graphics.TBitmap; ARect: Trect); overload;
    function GetBitmap8(out oBitmap8: TBitmap8; ARect: Trect): boolean; overload;
    function GetBitmap8(out oBitmap8: TBitmap8; ARect: Trect; aFringe: RFringe): boolean; overload;
    function GetColorRect(aColors: TDataArray): RBitColorRect; overload;
    function GetColorRect(aColors: TDataArray; ARect: Trect): RBitColorRect; overload;

    function FindPattern8(APattern: TBitmap8; out AThresholds: RBitColorOverlap; AFindBest: boolean = false)
      : boolean; overload;
    function FindPattern8(ARect: Trect; APattern: TBitmap8; out AThresholds: RBitColorOverlap;
      AFindBest: boolean = false): boolean; overload;

    function FindBitmap8(ARect: Trect; ATarget: TBitmap8; AThreshold: double; aStep: byte = 1): RBitColorOverlap;
    function LoadFromVCL(aBitmap: VCL.Graphics.TBitmap): boolean;
    function LoadFromFMX(aBitmap: FMX.Graphics.TBitmap): boolean;
    procedure MakeMeanFromMap(aAlphaColor: byte = bcBlack);
    function RasterAlphaColor(aFringe: RFringe; const aColor: TAlphaColor; IncCounter: boolean = false): byte;
    function RasterBitColor(aSourceColor: byte; aR: RFringe): byte;
    function SaveMeanToFile(AFName: String): integer;

    function GetPixelAlpha(aX, aY: cardinal): TAlphaColor;
    function GetPixel(aX, aY: cardinal): byte;
    procedure SetPixel(aX, aY: cardinal; aColor: byte);
    procedure SetPixelAlpha(aX, aY: cardinal; aColor: TAlphaColor);
  end;

implementation

uses FMX.Types, Winapi.Windows, System.Math, System.SysUtils, FMX.Dialogs;

{ TBitmap8 }

function TBitmap8.AlphaToColor(aColor: TAlphaColor): byte;
begin
  case aColor of
    acBlack:
      Result := byte(bcBlack);
    acWhite:
      Result := byte(bcWhite);
    cardinal(acRed):
      Result := byte(bcRed);
    acGreen:
      Result := byte(bcGreen);
    acBlue:
      Result := byte(bcBlue);
    acYellow:
      Result := byte(bcYellow);
    acMagenta:
      Result := byte(bcMagenta);
    acCyan:
      Result := byte(bcCyan);
  else
    Result := $00
  end;
end;

procedure TBitmap8.Clear;
var
  i: integer;
begin
  for i      := Low(FData) to High(FData) do
    FData[i] := byte(bcBlack);

  SetLength(FMeanMap, 0);

  FRedCount   := 0;
  FGreenCount := 0;
  FBlueCount  := 0;
end;

function TBitmap8.ColorToAlpha(aColor: byte): TAlphaColor;
begin
  case aColor of
    bcBlack:
      Result := TAlphaColor(acBlack);
    bcWhite:
      Result := TAlphaColor(acWhite);
    bcRed:
      Result := TAlphaColor(acRed);
    bcGreen:
      Result := TAlphaColor(acGreen);
    bcBlue:
      Result := TAlphaColor(acBlue);
    bcYellow:
      Result := TAlphaColor(acYellow);
    bcMagenta:
      Result := TAlphaColor(acMagenta);
    bcCyan:
      Result := TAlphaColor(acCyan);
  else
    Result := TAlphaColor(acBlack);
  end;
end;

function TBitmap8.CopyBitmap8(var oBitmap8: TBitmap8): boolean;
begin
  Result := GetBitmap8(oBitmap8, Trect.create(0, 0, Self.Width, Self.Height));
end;

constructor TBitmap8.create(aStream: TStream);
var
  l: cardinal;
begin
  inherited create;

  FRedCount   := 0;
  FGreenCount := 0;
  FBlueCount  := 0;

  aStream.read(FWidth, 2);
  aStream.read(FHeight, 2);
  SetLength(FData, FWidth * FHeight);

  aStream.read(l, 4);
  SetLength(FMeanMap, l);

  aStream.read(FFRinge, SizeOf(FFRinge));

  aStream.read(FMeanMap[0], Length(FMeanMap) * SizeOf(RBitColorPoint));

  DrawMean;
end;

constructor TBitmap8.create(aBitmap: FMX.Graphics.TBitmap; aFringe: RFringe; ARect: Trect);
var
  RData : TBitmapData;
  WData : TBitmapData;
  lX, lY: cardinal;
  iMean : cardinal;
  AC    : TAlphaColor;
  BC    : byte;
begin
  inherited create;

  create(ARect.Width, ARect.Height, aFringe);
  try
    FSourceBitmap := FMX.Graphics.TBitmap.create;
    FSourceBitmap.SetSize(FWidth, FHeight);
  except
    if Assigned(FSourceBitmap) then
      freeandnil(FSourceBitmap);
    FWidth  := 0;
    FHeight := 0;
    exit;
  end;

  iMean := 0;
  aBitmap.Map(TMapAccess.read, RData);
  FSourceBitmap.Map(TMapAccess.Write, WData);
  try
    SetLength(FMeanMap, FWidth * FHeight);
    for lY   := 0 to FHeight - 1 do
      for lX := 0 to FWidth - 1 do
      begin
        AC := RData.GetPixel(ARect.Left + lX, ARect.Top + lY);
        BC := RasterAlphaColor(aFringe, AC, true);
        WData.SetPixel(lX, lY, AC);
        FData[lY * FWidth + lX] := BC;

        if BC <> bcBlack then
          with FMeanMap[iMean] do
          begin
            X := lX;
            Y := lY;
            C := BC;;
            inc(iMean);
          end;
      end;
    SetLength(FMeanMap, iMean);
  finally
    FSourceBitmap.Unmap(RData);
    FSourceBitmap.Unmap(WData);
  end;
end;

constructor TBitmap8.create(aBitmap: FMX.Graphics.TBitmap; aFringe: RFringe);
var
  BData: TBitmapData;
  // R     : RBitColorPoint;
  lX, lY: cardinal;
  W, H  : cardinal;
  iMean : cardinal;
begin
  inherited create;

  try
    FSourceBitmap := FMX.Graphics.TBitmap.create;
    FSourceBitmap.Assign(aBitmap);
  except
    if Assigned(FSourceBitmap) then
      freeandnil(FSourceBitmap);
    exit;
  end;

  W := FSourceBitmap.Width;
  H := FSourceBitmap.Height;
  create(W, H, aFringe);

  iMean := 0;
  FSourceBitmap.Map(TMapAccess.read, BData);
  try
    SetLength(FMeanMap, W * H);
    for lY   := 0 to H - 1 do
      for lX := 0 to W - 1 do
        with FMeanMap[iMean] do
        begin
          X := lX;
          Y := lY;
          C := RasterAlphaColor(aFringe, BData.GetPixel(lX, lY), true);
          if C <> bcBlack then
          begin
            FData[lY * FWidth + lX] := C;
            inc(iMean);
          end;

          // if R.C <> byte(wcBlack) then
          // begin
          // FMeanMap[iMean] := R;
          // inc(iMean);
          // end;
        end;
    SetLength(FMeanMap, iMean);
  finally
    FSourceBitmap.Unmap(BData);
  end;
end;

constructor TBitmap8.create(aWidth, aHeight: cardinal; aFringe: RFringe);
begin
  inherited create;

  FWidth      := aWidth;
  FHeight     := aHeight;
  FRedCount   := 0;
  FGreenCount := 0;
  FBlueCount  := 0;
  FFRinge     := aFringe;

  SetLength(FData, 0);
  SetLength(FData, aWidth * aHeight);
  SetLength(FMeanMap, 0);

  PatternColors := [bcRed, bcGreen, bcBlue];
  PatternRect   := TrectF.create(0, 0, 100, 100);
end;

destructor TBitmap8.Destroy;
begin
  SetLength(FData, 0);
  SetLength(FMeanMap, 0);
  SetLength(FPatternColors, 0);

  if Assigned(FSourceBitmap) then
    freeandnil(FSourceBitmap);

  inherited;
end;

procedure TBitmap8.DrawMean;
var
  i: integer;
begin
  for i := Low(FMeanMap) to High(FMeanMap) do
  begin
    Data[FMeanMap[i].Y * Self.Width + FMeanMap[i].X] := FMeanMap[i].C;
  end;
end;

procedure TBitmap8.Empty;
begin
  SetLength(FData, 0);
  SetLength(FMeanMap, 0);
  FWidth      := 0;
  FHeight     := 0;
  FRedCount   := 0;
  FGreenCount := 0;
  FBlueCount  := 0;
end;

function TBitmap8.FindBitmap8(ARect: Trect; ATarget: TBitmap8; AThreshold: double; aStep: byte = 1): RBitColorOverlap;
var
  xi, yi, i      : integer;
  Color, Step    : byte;
  lOverlap, lMiss: integer;
  Overlaped      : boolean;
  TargetLength   : cardinal;
  tH, tW         : word;
begin
  TargetLength := Length(ATarget.MeanMap);
  lOverlap     := 0;
  lMiss        := 0;
  Overlaped    := false;
  Step         := max(1, aStep);
  tH           := Min(ARect.Bottom, Self.Height - ATarget.Height);
  tW           := Min(ARect.Right, Self.Width - ATarget.Width);
  yi           := 0;

  for xi := ARect.Left to tW do
  begin
    for yi := ARect.Top to tH do
    begin
      lOverlap  := 0;
      lMiss     := 0;
      Overlaped := false;

      i := 0;
      while i < Length(ATarget.MeanMap) do
      begin
        Color := FData[(yi + ATarget.MeanMap[i].Y) * Self.Width + (xi + ATarget.MeanMap[i].X)];
        // color := self.GetPixel(xi + ATarget.MeanMap[i].X, yi + ATarget.MeanMap[i].Y);
        if Color = ATarget.MeanMap[i].C then
        begin
          inc(lOverlap);
          // if overlap / (TargetLength div Step) > 0.50 then
          // self.SetPixel(xi + ATarget.MeanMap[i].X, yi + ATarget.MeanMap[i].Y, CYellow);
        end
        else
          inc(lMiss);

        i := i + Step;
      end;

      if (lOverlap / (TargetLength div Step) * 100) > AThreshold then
      begin
{$IFDEF DEBUG}
        // нарисовать желтые точки на найденных точках
        // i := 0;
        // while i < Length(ATarget.MeanMap) do
        // begin
        // color := FData[(yi + ATarget.MeanMap[i].Y) * self.Width + (xi + ATarget.MeanMap[i].X)];
        // if color = ATarget.MeanMap[i].C then
        // begin
        // self.SetPixel(xi + ATarget.MeanMap[i].X, yi + ATarget.MeanMap[i].Y, byte(bcYellow));
        // end
        // else
        // self.SetPixel(xi + ATarget.MeanMap[i].X, yi + ATarget.MeanMap[i].Y, byte(bcCyan));
        //
        // i := i + Step;
        // end;
{$ENDIF}
        Overlaped := true;
        break;
      end;
    end;
    if Overlaped then
      break;
  end;

  Result.X     := xi;
  Result.Y     := yi;
  Result.Color := lOverlap / (TargetLength div Step) * 100;
  Result.Miss  := lMiss / (TargetLength div Step) * 100;
  Result.Black := (tW * tH) - Length(ATarget.MeanMap);
end;

function TBitmap8.FindPattern8(ARect: Trect; APattern: TBitmap8; out AThresholds: RBitColorOverlap;
  AFindBest: boolean = false): boolean;
var
  xs, ys, xp, yp    : integer;
  sColor, pColor, lC: byte;
  ColorCnt, BlackCnt: word;
  patternLength     : word;
  Overlaped         : boolean;
  inColors, sIsColor: boolean;
  Zcolor, Zblack    : double;
  aColor, aBlack    : double;
  aDisp, aMiss      : double;
begin
  Result := false;
  aColor := APattern.PatternThresholds.Color;
  aBlack := APattern.PatternThresholds.Black;
  aDisp  := APattern.PatternThresholds.Dispersion;
  aMiss  := APattern.PatternThresholds.Miss;

  if (ARect.Height < APattern.Height) or (ARect.Bottom >= Self.Height) or (ARect.Width < APattern.Width) or
    (ARect.Right >= Self.Width) then
    exit;

  ZeroMemory(@AThresholds, SizeOf(AThresholds));
  AThresholds.Miss := aMiss; // % промахов

  patternLength := Length(APattern.FData);
  for yp        := 0 to APattern.Height - 1 do // вычитаем неважные точки паттерна
  begin
    for xp := 0 to APattern.Width - 1 do
    begin
      pColor := APattern.FData[yp * APattern.Width + xp];
      if pColor = bcGreen then
        dec(patternLength);
    end;
  end;

  Overlaped := false;
  for ys    := 0 to ARect.Height - APattern.Height + 1 do // пробегаем по пр€моугольнику поиска
  begin
    for xs := 0 to ARect.Width - APattern.Width + 1 do
    begin
      Overlaped := false;
      ColorCnt  := 0;
      BlackCnt  := 0;

      for yp := 0 to APattern.Height - 1 do // пробегаем по паттерну
      begin
        for xp := 0 to APattern.Width - 1 do
        begin
          pColor := APattern.FData[yp * APattern.Width + xp];
          if pColor = bcGreen then
            continue;

          sColor := Self.FData[(ys + yp + ARect.Top) * Self.Width + (xs + xp + ARect.Left)];

          // тест считывани€ убрать
          // if sColor = wcBlack then
          // self.FData[(ys + yp + ARect.Top) * self.Width + (xs + xp + ARect.Left)] := byte(wcYellow);

          sIsColor := false;
          if sColor <> bcBlack then             // исходный цвет черный
            for lC in APattern.PatternColors do // проверка исходного цвета на мусор
              if sColor = lC then
              begin
                sIsColor := true; // исходный цвет значимый
                break;
              end;

          // if sColor = wcBlack then // исходный цвет черный
          // sIsColor := false      // флаг исходный цвет Ч черный
          // else
          // begin
          // inColors := false;
          // for lC in APattern.PatternColors do // проверка исходного цвета на мусор
          // if sColor = lC then
          // begin
          // inColors := true; // исходный цвет значимый
          // break;
          // end;
          //
          // sIsColor := inColors; // флаг исходный цвет Ч черный или значимый
          // end;

          case pColor of
            bcRed: // цвет паттерна (красный) Ч значимый
              if sIsColor then
                inc(ColorCnt)
              else; // промах
            bcBlue: // цвет паттерна (синий) Ч средний
              if sIsColor then
                inc(ColorCnt)
              else
                inc(BlackCnt);
            bcGreen: // такого быть не должно, но мало ли ошибс€ =)
              if sIsColor then
                inc(ColorCnt)
              else
                inc(BlackCnt);
          else                 // цвет паттерна (черный) Ч несовпадение
            if aBlack = 0 then // не учитывать вес черного
              inc(BlackCnt)
            else if not sIsColor then // если черные совпадают
              inc(BlackCnt)
            else; // промах
          end;

          // if not sIsColor then // исходный цвет черный
          // case pColor of
          // wcBlack: // цвет паттерна Ч черный
          // inc(lBlack);
          // wcBlue: // цвет паттерна (синий) Ч средний
          // inc(lBlack);
          // else                   // цвет паттерна значимый Ч несовпадение
          // if patternB = 0 then // учитывать вес черного и весомого цветов вместе
          // inc(lBlack);
          // end
          // else begin
          // case pColor of
          // wcRed: // цвет паттерна (красный) Ч значимый
          // inc(lColor);
          // wcBlue: // цвет паттерна (синий) Ч средний
          // inc(lColor);
          // else // цвет паттерна (черный) Ч несовпадение
          // // inc(lMiss)
          // end
          // end;
        end;
      end;

      Zcolor := ColorCnt / patternLength * 100;
      Zblack := BlackCnt / patternLength * 100;
      if AFindBest then
        if (Zcolor + aDisp >= aColor) and (Zblack + aDisp >= aBlack) and (100 - Zcolor - Zblack <= AThresholds.Miss)
        then
        begin
          AThresholds.X          := ARect.Left + xs;       // X координата ARect совпадени€
          AThresholds.Y          := ARect.Top + ys;        // Y координата ARect совпадени€
          AThresholds.Color      := Zcolor;                // % весомого цвета
          AThresholds.Black      := Zblack;                // % черного цвета
          AThresholds.Miss       := 100 - Zcolor - Zblack; // % промахов
          AThresholds.Dispersion := patternLength;         // длина паттерна
        end
        else
          continue
      else if (Zcolor + aDisp >= aColor) and (Zblack + aDisp >= aBlack) and (100 - Zcolor - Zblack <= aMiss) then
      begin
        AThresholds.X          := ARect.Left + xs;       // X координата ARect совпадени€
        AThresholds.Y          := ARect.Top + ys;        // Y координата ARect совпадени€
        AThresholds.Color      := Zcolor;                // % весомого цвета
        AThresholds.Black      := Zblack;                // % черного цвета
        AThresholds.Miss       := 100 - Zcolor - Zblack; // % промахов
        AThresholds.Dispersion := patternLength;         // длина паттерна

        Overlaped := true;
        break;
      end;
    end;

    if Overlaped then
      break;
  end;

  if AFindBest then
    if (AThresholds.Color + aDisp >= aColor) and (AThresholds.Black + aDisp >= aBlack) and
      (100 - AThresholds.Color - AThresholds.Black <= aMiss) then
      Overlaped := true
    else
      Overlaped := false
  else if not Overlaped then
  begin
    AThresholds.X          := maxword;
    AThresholds.Y          := maxword;
    AThresholds.Dispersion := patternLength; // длина паттерна
  end;

  Result := Overlaped;
end;

function TBitmap8.FindPattern8(APattern: TBitmap8; out AThresholds: RBitColorOverlap;
  AFindBest: boolean = false): boolean;
begin
  if APattern.PatternRect.Right = 0 then
    APattern.PatternRect := TrectF.create(0, 0, 100, 100);

  Result := FindPattern8(Trect.create(Round(Self.Width * APattern.PatternRect.Left),
    Round(Self.Height * APattern.PatternRect.Top), Round(Self.Width * APattern.PatternRect.Right) - 1,
    Round(Self.Height * APattern.PatternRect.Bottom) - 1), APattern, AThresholds, AFindBest);
end;

procedure TBitmap8.GetBitmap(oBitmap: FMX.Graphics.TBitmap);
begin
  GetBitmap(oBitmap, Trect.create(0, 0, Self.Width, Self.Height));
end;

procedure TBitmap8.GetBitmap(oBitmap: FMX.Graphics.TBitmap; ARect: Trect);
var
  LData: TBitmapData;
  X, Y : integer;
  W, H : integer;
begin
  LData := TBitmapData.create(ARect.Width, ARect.Height, TPixelFormat.RGBA);
  oBitmap.SetSize(TSize.create(ARect.Width, ARect.Height));
  oBitmap.Map(TMapAccess.Write, LData);

  W := Min(ARect.Right + 1, Self.FWidth) - ARect.Left;
  H := Min(ARect.Bottom + 1, Self.FHeight) - ARect.Top;
  try
    for X   := 0 to W - 1 do
      for Y := 0 to H - 1 do
        LData.SetPixel(X, Y, Self.GetPixelAlpha(ARect.Left + X, ARect.Top + Y));
  finally
    oBitmap.Unmap(LData);
  end;
end;

function TBitmap8.GetBitmap8(out oBitmap8: TBitmap8; ARect: Trect): boolean;
begin
  Result := GetBitmap8(oBitmap8, ARect, Self.Fringe);
end;

function TBitmap8.GetBitmap8(out oBitmap8: TBitmap8; ARect: Trect; aFringe: RFringe): boolean;
var
  X, Y      : integer;
  SX, SY    : word;
  W, H      : integer;
  BC        : byte;
  NeedRaster: boolean;
begin
  Result := false;
  try
    W := Min(Self.Width, ARect.Right + 1) - max(0, ARect.Left);
    H := Min(Self.Height, ARect.Bottom + 1) - max(0, ARect.Top);
    if (W < 1) or (H < 1) then
      exit;

    SX := max(0, ARect.Left);
    SY := max(0, ARect.Top);

    oBitmap8 := TBitmap8.create(W, H, aFringe);

    if (aFringe.isR <> Self.Fringe.isR) or (aFringe.isG <> Self.Fringe.isG) or (aFringe.isB <> Self.Fringe.isB) then
      NeedRaster := true
    else
      NeedRaster := false;

    for X   := 0 to W - 1 do
      for Y := 0 to H - 1 do
      begin
        if NeedRaster then
          BC := RasterBitColor(Self.GetPixel(SX + X, SY + Y), aFringe)
        else
          BC := Self.GetPixel(SX + X, SY + Y);

        oBitmap8.FData[Y * oBitmap8.FWidth + X] := BC;
      end;
  except
    if Assigned(oBitmap8) then
      freeandnil(oBitmap8)
    else
      oBitmap8 := nil;
    exit;
  end;

  SetLength(oBitmap8.FPatternColors, Length(Self.PatternColors));
  for X                       := 0 to Length(Self.PatternColors) - 1 do
    oBitmap8.PatternColors[X] := Self.PatternColors[X];

  CopyMemory(@oBitmap8.PatternRect, @Self.PatternRect, SizeOf(Self.PatternRect));

  oBitmap8.PatternThresholds.SetThresholds(Self.PatternThresholds.Color, Self.PatternThresholds.Black,
    Self.PatternThresholds.Dispersion, Self.PatternThresholds.Miss);

  try
    oBitmap8.MakeMeanFromMap;
  except
    if Assigned(oBitmap8) then
      freeandnil(oBitmap8)
    else
      oBitmap8 := nil;
    exit;
  end;

  Result := true;
end;

function TBitmap8.GetColorRect(aColors: TDataArray): RBitColorRect;
begin
  Result := GetColorRect(aColors, Trect.create(0, 0, Self.Width, Self.Height));
end;

function TBitmap8.GetColorRect(aColors: TDataArray; ARect: Trect): RBitColorRect;
var
  i, pc         : integer;
  minx, miny    : word;
  maxx, maxy    : word;
  x1, y1        : word;
  Color, lC     : byte;
  cx, cw, cy, ch: word;
begin
  minx := High(word);
  miny := High(word);
  maxx := Low(word);
  maxy := Low(word);

  i  := 0;
  cx := max(0, ARect.Left);
  cw := Min(Self.Width, ARect.Right);
  cy := max(0, ARect.Top);
  ch := Min(Self.Height, ARect.Bottom);

  for y1 := cy to ch - 1 do
  begin
    for x1 := cx to cw - 1 do
    begin
      Color := Self.FData[y1 * Self.Width + x1];

      for lC in aColors do
        if Color = lC then
        begin
          if minx > x1 then
            minx := x1;
          if maxx < x1 then
            maxx := x1;

          if miny > y1 then
            miny := y1;
          if maxy < y1 then
            maxy := y1;

          inc(i);

          break;
        end;
    end;
  end;
  pc := (maxx - minx + 1) * (maxy - miny + 1);
  // pc := Trunc(i / pc * 100);

  Result.Colors   := aColors;
  Result.Quantity := i;
  Result.Pixels   := pc;
  Result.Rect     := Trect.create(minx, miny, maxx, maxy);
end;

function TBitmap8.GetPixel(aX, aY: cardinal): byte;
begin
  Result := FData[aY * Self.Width + aX];
end;

function TBitmap8.GetPixelAlpha(aX, aY: cardinal): TAlphaColor;
begin
  Result := ColorToAlpha(FData[aY * Self.Width + aX]);
end;

function TBitmap8.LoadFromFMX(aBitmap: FMX.Graphics.TBitmap): boolean;
var
  BData: TBitmapData;
  R    : RBitColorPoint;
  X, Y : cardinal;
  iMean: cardinal;
begin
  Result := false;

  if not Assigned(aBitmap) then
    exit;

  if (aBitmap.Width = 1) or (aBitmap.Height < 1) then
    exit;

  try
    if not Assigned(FSourceBitmap) then
      FSourceBitmap := FMX.Graphics.TBitmap.create;

    FSourceBitmap.Assign(aBitmap);
  except
    if Assigned(FSourceBitmap) then
      freeandnil(FSourceBitmap);
    exit;
  end;

  FWidth  := FSourceBitmap.Width;
  FHeight := FSourceBitmap.Height;

  SetLength(FData, FHeight * FWidth);
  SetLength(FMeanMap, FHeight * FWidth);

  iMean := 0;
  aBitmap.Map(TMapAccess.read, BData);
  try
    for Y   := 0 to FHeight - 1 do
      for X := 0 to FWidth - 1 do
      begin
        R.X                   := X;
        R.Y                   := Y;
        FData[Y * FWidth + X] := RasterAlphaColor(FFRinge, BData.GetPixel(X, Y), true);
        R.C                   := FData[Y * FWidth + X];

        if R.C <> bcBlack then
        begin
          FMeanMap[iMean] := R;
          inc(iMean);
        end;
      end;
    SetLength(FMeanMap, iMean);
    Result := true;
  finally
    aBitmap.Unmap(BData);
  end;
end;

function TBitmap8.LoadFromVCL(aBitmap: VCL.Graphics.TBitmap): boolean;
var
  R, G, B: byte;
  P      : pointer;
  BC     : RBitColorPoint;
  X, Y   : cardinal;
  iMean  : cardinal;
begin
  Result := false;

  if not Assigned(aBitmap) then
    exit;
  if aBitmap.Empty then
    exit;

  FWidth  := aBitmap.Width;
  FHeight := aBitmap.Height;

  SetLength(FData, FHeight * FWidth);
  SetLength(FMeanMap, FHeight * FWidth);

  iMean := 0;
  try
    for Y := 0 to aBitmap.Height - 1 do
    begin
      P := aBitmap.ScanLine[Y];

      for X := 0 to aBitmap.Width - 1 do
      begin
        BC.X := X;
        BC.Y := Y;
        R    := pByteArray(P)^[4 * X + 1];
        G    := pByteArray(P)^[4 * X + 2];
        B    := pByteArray(P)^[4 * X + 3];

        FData[Y * FWidth + X] := RasterByteColors(FFRinge, R, G, B, true);

        BC.C := FData[Y * FWidth + X];
        if BC.C <> bcBlack then
        begin
          FMeanMap[iMean] := BC;
          inc(iMean);
        end;
      end;
    end;
  except
    Self.Empty;
    exit;
  end;
  SetLength(FMeanMap, iMean);
  Result := true;
end;

procedure TBitmap8.MakeMeanFromMap(aAlphaColor: byte = bcBlack);
var
  X, Y : word;
  W, H : word;
  R    : RBitColorPoint;
  iMean: cardinal;
begin
  FRedCount   := 0;
  FGreenCount := 0;
  FBlueCount  := 0;
  SetLength(Self.FMeanMap, Length(Self.FData));
  X     := 0;
  Y     := 0;
  W     := Self.FWidth;
  H     := Self.FHeight;
  iMean := 0;
  while (X < W) and (Y < H) do
  begin
    R.X := X;
    R.Y := Y;
    R.C := Self.FData[Y * W + X];

    if R.C <> aAlphaColor then
    begin
      inc(iMean);
      FMeanMap[iMean - 1] := R;

      if (R.C AND $30 > 0) then
        inc(FRedCount);
      if (R.C AND $0C > 0) then
        inc(FGreenCount);
      if (R.C AND $03 > 0) then
        inc(FBlueCount);

      // if EWColor(R.C) in [wcRed, wcWhite, wcYellow, wcMagenta] then
      // inc(FRedCount);
      // if EWColor(R.C) in [wcGreen, wcWhite, wcYellow, wcCyan] then
      // inc(FGreenCount);
      // if EWColor(R.C) in [wcBlue, wcWhite, wcMagenta, wcCyan] then
      // inc(FBlueCount);
    end;

    inc(X);
    if X = FWidth then
    begin
      X := 0;
      inc(Y);
    end;
  end;
  SetLength(FMeanMap, iMean);
end;

function TBitmap8.RasterAlphaColor(aFringe: RFringe; const aColor: TAlphaColor; IncCounter: boolean = false): byte;
var
  // Color     : byte;
  R, G, B: byte;
  C      : cardinal;
  // cR, cG, cB: cardinal;
begin
  R := 0;
  G := 0;
  B := 0;
  // cR := 0;
  // cG := 0;
  // cB := 0;

  if aFringe.isR then
  begin
    C := (aColor and $00FF0000) SHR 16;
    if (C >= aFringe.redLowFringe) and (C <= aFringe.redHiFringe) then
    begin
      R := $30; // Bx110000
      // cR := $FF0000;
      if IncCounter then
        inc(FRedCount);
    end;
  end;

  if aFringe.isG then
  begin
    C := (aColor and $0000FF00) SHR 8;
    if (C >= aFringe.greenLowFringe) and (C <= aFringe.greenHiFringe) then
    begin
      G := $0C; // Bx1100
      // cG := $FF00;
      if IncCounter then
        inc(FGreenCount);
    end
  end;

  if aFringe.isB then
  begin
    C := (aColor and $000000FF);
    if (C >= aFringe.blueLowFringe) and (C <= aFringe.blueHiFringe) then
    begin
      B := 3; // Bx11
      // cB := $FF;
      if IncCounter then
        inc(FBlueCount);
    end
  end;

  // C := $FF000000 + cR + cG + cB;
  // if Color = byte(wcWhite) then
  // Color := byte(wcWhite);

  Result := R + G + B;
end;

function TBitmap8.RasterByteColors(aFringe: RFringe; const aR, aG, aB: byte; IncCounter: boolean): byte;
var
  C: cardinal;
begin
  C      := $FF000000 + aR SHL 16 + aG SHL 8 + aB;
  Result := RasterAlphaColor(aFringe, C, IncCounter);
end;

function TBitmap8.RasterBitColor(aSourceColor: byte; aR: RFringe): byte;
var
  BC: byte;
begin
  BC := 0;
  if aR.isR then
    BC := $30 OR BC;
  if aR.isG then
    BC := $0C OR BC;
  if aR.isB then
    BC := $3 OR BC;

  Result := aSourceColor and BC;
end;

function TBitmap8.SaveMeanToFile(AFName: String): integer;
var
  aF: TFileStream;
  l : integer;
begin
  try
    Result := -1;
    aF     := TFileStream.create(AFName, fmCreate);
    aF.Write(Width, 2);
    aF.Write(Height, 2);

    l := Length(FMeanMap);
    aF.Write(l, 4);
    aF.Write(FFRinge, SizeOf(FFRinge));
    Result := aF.Write(FMeanMap[0], Length(FMeanMap) * SizeOf(RBitColorPoint));
  finally
    freeandnil(aF);
  end;
end;

procedure TBitmap8.SetPixel(aX, aY: cardinal; aColor: byte);
begin
  FData[aY * Self.Width + aX] := aColor;
end;

procedure TBitmap8.SetPixelAlpha(aX, aY: cardinal; aColor: TAlphaColor);
begin
  SetPixel(aX, aY, AlphaToColor(aColor));
end;

{ RColorMap }

function RBitColorPoint.A: TAlphaColor;
begin
  case C of
    bcWhite:
      Result := TAlphaColor(acWhite);
    bcRed:
      Result := TAlphaColor(acRed);
    bcGreen:
      Result := TAlphaColor(acGreen);
    bcBlue:
      Result := TAlphaColor(acBlue);
    bcYellow:
      Result := TAlphaColor(acYellow);
    bcMagenta:
      Result := TAlphaColor(acMagenta);
    bcCyan:
      Result := TAlphaColor(acCyan);
  else
    Result := TAlphaColor(acBlack);
  end;
end;

{ RFringe }

constructor RFringe.create(aLF, aHF: byte; aR, aG, aB: boolean);
begin
  isR := aR;
  isG := aG;
  isB := aB;

  if isR then
  begin
    redLowFringe := aLF;
    redHiFringe  := aHF;
  end else begin
    redLowFringe := 255;
    redHiFringe  := 255;
  end;

  if isG then
  begin
    greenLowFringe := aLF;
    greenHiFringe  := aHF;
  end else begin
    greenLowFringe := 255;
    greenHiFringe  := 255;
  end;

  if isB then
  begin
    blueLowFringe := aLF;
    blueHiFringe  := aHF;
  end else begin
    blueLowFringe := 255;
    blueHiFringe  := 255;
  end;
end;

constructor RFringe.create(aRLF, aRHF, aGLF, aGHF, aBLF, aBHF: byte; aR, aG, aB: boolean);
begin
  isR := aR;
  isG := aG;
  isB := aB;

  if isR then
  begin
    redLowFringe := aRLF;
    redHiFringe  := aRHF;
  end else begin
    redLowFringe := 255;
    redHiFringe  := 255;
  end;

  if isG then
  begin
    greenLowFringe := aGLF;
    greenHiFringe  := aGHF;
  end else begin
    greenLowFringe := 255;
    greenHiFringe  := 255;
  end;

  if isB then
  begin
    blueLowFringe := aBLF;
    blueHiFringe  := aBHF;
  end else begin
    blueLowFringe := 255;
    blueHiFringe  := 255;
  end;
end;

{ RWColor }

{ RBitColorOverlap }

procedure RBitColorOverlap.SetThresholds(aColor, aBlack, aDisp, aMiss: double);
begin
  X          := maxword;
  Y          := maxword;
  Miss       := aMiss;
  Color      := aColor;
  Black      := aBlack;
  Dispersion := aDisp;
end;

end.
