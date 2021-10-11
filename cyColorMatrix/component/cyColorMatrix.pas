unit cyColorMatrix;
{   Component(s):
    tcyColorMatrix

    Description:
    It's a color matrix representation !!!
    This component allow best performance than TcyColorGrid
    despite of non published ColorList property ...

    Author: Mauricio
    mail: mauricio_box@yahoo.com

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    Copyrights:
    You can use and distribute this component freely but you can' t remove
    this header
}

interface

uses Classes, Types, Controls, Graphics, Messages;

type
  TBoxStyle = (bsRectangle, bsRoundRect, bsCircle, bsHCrystal, Crystal);
  TProcOnPaintBox = procedure (Sender: TObject; aRect: TRect; aRow: integer; aCol: integer; aColor: TColor) of object;
  TProcOnBoxClick = procedure (Sender: TObject; aRow: integer; aCol: integer; aColor: TColor) of object;

  TcyColorMatrix = class(TGraphicControl)
  private
    FColorList: Array of Array of TColor;
    FptMouseDown: TPoint;
    FBackground: TColor;
    FBoxWidth: integer;
    FBoxHeight: integer;
    FBoxRows: integer;
    FBoxCols: integer;
    FBoxIntervalX: integer;
    FBoxIntervalY: integer;
    FBoxFrameWidth: integer;
    FBoxFrameColor: TColor;
    FOnCustomDrawBox: TProcOnPaintBox;
    FOnBeforePaintBoxes: TNotifyEvent;
    FOnAfterPaintBoxes: TNotifyEvent;
    FTransparent: Boolean;
    FOnBoxClick: TProcOnBoxClick;
    FDefaultColor: TColor;
    procedure SetBackground(const Value: TColor);
    procedure SetBoxWidth(const Value: integer);
    procedure SetBoxHeight(const Value: integer);
    procedure SetBoxRows(const Value: integer);
    procedure SetBoxCols(const Value: integer);
    procedure SetBoxIntervalX(const Value: integer);
    procedure SetBoxIntervalY(const Value: integer);
    procedure SetBoxFrameWidth(const Value: integer);
    procedure SetBoxFrameColor(const Value: TColor);
    procedure SetTransparent(const Value: Boolean);
    procedure PaintBoxes(AllColors: Boolean; paramColor: TColor);
    procedure PaintBox(var BoxRect: TRect; BoxColor: TColor; Row, Column: Integer; CustomDrawBox: Boolean);
    procedure InitializeColors(fromRow, fromCol: Integer);
    procedure SetDefaultColor(const Value: TColor);
  protected
    procedure Click; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override; // Call OnMouseDown procedure ...
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure SetAutoSize(Value: Boolean); override;
    procedure Paint; override;
    function  PartialPaint: Boolean; virtual;
    procedure DefaultDrawBox(var aRect: TRect; aRow, aCol: integer; aColor: TColor); virtual;
    procedure RedefineSize; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  GetBoxAtPos(aPoint: TPoint; var aRow: Integer; var aCol: Integer; ExactPos: Boolean): Boolean;
    function  GetColorGrid(aRow: Integer; aCol: integer): TColor; virtual;
    procedure SetColorGrid(aRow: Integer; aCol: integer; aColor: TColor; FullRepaint: Boolean);
    procedure SubstColor(old, New: TColor; FullRepaint: Boolean); virtual;
  published
    property Align;
    property Autosize;
    property Anchors;
    property Canvas;
    property Constraints;
    property Enabled;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property Background: TColor read FBackground write SetBackground;
    property BoxHeight: integer read FBoxHeight write SetBoxHeight;
    property BoxWidth: integer read FBoxWidth write SetBoxWidth;
    property BoxRows: integer read FBoxRows write SetBoxRows;
    property BoxCols: integer read FBoxCols write SetBoxCols;
    property BoxFrameColor: TColor read FBoxFrameColor write SetBoxFrameColor;
    property BoxFrameWidth: integer read FBoxFrameWidth write SetBoxFrameWidth default 0;
    property BoxIntervalX: integer read FBoxIntervalX write SetBoxIntervalX default 1;
    property BoxIntervalY: integer read FBoxIntervalY write SetBoxIntervalY default 1;
    property DefaultColor: TColor read FDefaultColor write SetDefaultColor;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property OnCustomDrawBox: TProcOnPaintBox read FOnCustomDrawBox write FOnCustomDrawBox;
    property OnBeforePaintBoxes: TNotifyEvent read FOnBeforePaintBoxes write FOnBeforePaintBoxes;
    property OnAfterPaintBoxes: TNotifyEvent read FOnAfterPaintBoxes write FOnAfterPaintBoxes;
    property OnBoxClick: TProcOnBoxClick read FOnBoxClick write FOnBoxClick;
  end;

procedure Register;
implementation

{ TcyColorMatrix }

constructor TcyColorMatrix.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackground := clBlack;
  FDefaultColor := clGreen;
  FBoxHeight := 5;
  FBoxWidth := 5;
  FBoxRows := 30;
  FBoxCols := 30;
  FBoxIntervalX := 1;
  FBoxIntervalY := 1;
  FBoxFrameWidth := 0;
  FBoxFrameColor := clGray;
  FTransparent := false;
  SetLength(FColorList, FBoxCols, FBoxRows);
  InitializeColors(0, 0);
  Autosize:= true;
end;

destructor TcyColorMatrix.Destroy;
begin
  inherited Destroy;
end;

procedure TcyColorMatrix.Loaded;
begin
  Inherited;
  SetLength(FColorList, FBoxCols, FBoxRows);
  InitializeColors(0, 0);
end;

procedure TcyColorMatrix.InitializeColors(fromRow, fromCol: Integer);
var r, c: Integer;
begin
  for c := fromCol to FBoxCols - 1 do
    for r := fromRow to FBoxRows - 1 do
      FColorList[c, r] := FDefaultColor;
end;

procedure TcyColorMatrix.Paint;
begin
  // Draw background:
  if not FTransparent
  then begin
    Canvas.Brush.Color := FBackground;
    Canvas.FillRect(ClientRect);
  end;

  if Assigned(FOnBeforePaintBoxes) then FOnBeforePaintBoxes(Self);
  PaintBoxes(true, 0);
  if Assigned(FOnAfterPaintBoxes) then FOnAfterPaintBoxes(Self);
end;

procedure TcyColorMatrix.PaintBoxes(AllColors: Boolean; paramColor: TColor);
var
  r, c, xPos, yPos: integer;
  CurColor: TColor;
  BoxRect: TRect;
  CustomDrawBox: Boolean;

    function PaintThisOne: Boolean;
    begin
      if AllColors
      then
        RESULT := true
      else
        RESULT := CurColor = paramColor;
    end;

begin
  CustomDrawBox := Assigned(FOnCustomDrawBox);
  yPos := FBoxIntervalY;

  for r := 0 to FBoxRows -1 do
  begin
    xPos := FBoxIntervalX;

    for c := 0 to FBoxCols -1 do
    begin
      CurColor := FColorList[c, r];

      if PaintThisOne
      then begin
        BoxRect := classes.Rect(xPos, yPos, xPos + FBoxWidth, yPos + FBoxHeight);
        PaintBox(BoxRect, CurColor, r, c, CustomDrawBox);
      end;

      inc(xPos, FBoxWidth + FBoxIntervalX);
    end;

    inc(yPos, FBoxHeight + FBoxIntervalY);
  end;
end;

procedure TcyColorMatrix.PaintBox(var BoxRect: TRect; BoxColor: TColor; Row, Column: Integer; CustomDrawBox: Boolean);
begin
  if CustomDrawBox
  then FOnCustomDrawBox(self, BoxRect, Row, Column, BoxColor)
  else DefaultDrawBox(BoxRect, Row, Column, BoxColor);
end;

function TcyColorMatrix.PartialPaint: Boolean;
begin
  RESULT := false;

  if not Assigned(FOnBeforePaintBoxes)
  then
    if not Assigned(FOnAfterPaintBoxes)
    then RESULT := true;
end;

procedure TcyColorMatrix.SetAutoSize(Value: Boolean);
begin
  if AutoSize <> Value then
  begin
    inherited SetAutoSize(Value);  // Change Autosize value ...
    RedefineSize;
  end;
end;

procedure TcyColorMatrix.RedefineSize;
begin
  if AutoSize
  then begin
    Width  := FBoxIntervalX + FBoxCols * (FBoxWidth + FBoxIntervalX);
    Height := FBoxIntervalY + FBoxRows * (FBoxHeight + FBoxIntervalY);
  end;
end;

procedure TcyColorMatrix.SetBackground(const Value: TColor);
begin
  FBackground := Value;
  Invalidate;
end;

procedure TcyColorMatrix.SetBoxCols(const Value: integer);
var
  oldCols: Integer;
begin
  oldCols := FBoxCols;
  FBoxCols := Value;
  SetLength(FColorList, FBoxCols, FBoxRows);
  InitializeColors(0, oldCols);
  RedefineSize;
  Invalidate;
end;

procedure TcyColorMatrix.SetBoxRows(const Value: integer);
var oldRows: Integer;
begin
  oldRows := FBoxRows;
  FBoxRows := Value;
  SetLength(FColorList, FBoxCols, FBoxRows);
  InitializeColors(oldRows, 0);
  RedefineSize;
  Invalidate;
end;

procedure TcyColorMatrix.SetBoxFrameColor(const Value: TColor);
begin
  FBoxFrameColor := Value;
  Invalidate;
end;

procedure TcyColorMatrix.SetBoxFrameWidth(const Value: integer);
begin
  FBoxFrameWidth := Value;
  Invalidate;
end;

procedure TcyColorMatrix.SetBoxHeight(const Value: integer);
begin
  FBoxHeight := Value;
  RedefineSize;
  Invalidate;
end;

procedure TcyColorMatrix.SetBoxIntervalX(const Value: integer);
begin
  FBoxIntervalX := Value;
  RedefineSize;
  Invalidate;
end;

procedure TcyColorMatrix.SetBoxIntervalY(const Value: integer);
begin
  FBoxIntervalY := Value;
  RedefineSize;
  Invalidate;
end;

procedure TcyColorMatrix.SetBoxWidth(const Value: integer);
begin
  FBoxWidth := Value;
  RedefineSize;
  Invalidate;
end;

procedure TcyColorMatrix.SetDefaultColor(const Value: TColor);
begin
  SubstColor(FDefaultColor, Value, true);
  FDefaultColor := Value;
  Invalidate;
end;

procedure TcyColorMatrix.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

function  TcyColorMatrix.GetBoxAtPos(aPoint: TPoint; var aRow: Integer; var aCol: Integer; ExactPos: Boolean): Boolean;

    function VerifyIntervals: Boolean;
    begin
      RESULT := false;

      if (aCol >= 0) and (aCol < FBoxCols)
      then
        if (aRow >= 0) and (aRow < FBoxRows)
        then RESULT := true;
    end;

begin
  RESULT := false;
  aCol := (aPoint.X - FBoxIntervalX div 2) div (FBoxWidth + FBoxIntervalX);
  aRow := (aPoint.Y - FBoxIntervalY div 2) div (FBoxHeight + FBoxIntervalY);

  if ExactPos
  then begin
    if aPoint.X >= (FBoxWidth + FBoxIntervalX) * aCol + FBoxIntervalX
    then
      if aPoint.X <= (FBoxWidth + FBoxIntervalX) * (aCol + 1)
      then
        if aPoint.Y >= (FBoxHeight + FBoxIntervalY) * aRow + FBoxIntervalY
        then
          if aPoint.Y <= (FBoxHeight + FBoxIntervalY) * (aRow + 1)
          then RESULT := VerifyIntervals;
  end
  else
    RESULT := VerifyIntervals;
end;

function  TcyColorMatrix.GetColorGrid(aRow: Integer; aCol: integer): TColor;
begin
  RESULT := FColorList[aCol, aRow];
end;

procedure TcyColorMatrix.SetColorGrid(aRow: Integer; aCol: integer; aColor: TColor; FullRepaint: Boolean);
var
  BoxRect: TRect;
begin
  FColorList[aCol, aRow] := aColor;

  if FullRepaint
  then              // More rapid but with blink effect if not doublebuffered ...
    Invalidate
  else begin        // Avoid full repaint but with blinking effect ...
    BoxRect := classes.Rect(FBoxIntervalX + (FBoxWidth+FBoxIntervalX)*aCol,
                            FBoxIntervalY + (FBoxHeight+FBoxIntervalY)*aRow,
                            FBoxIntervalX + (FBoxWidth+FBoxIntervalX)*aCol + FBoxWidth,
                            FBoxIntervalY + (FBoxHeight+FBoxIntervalY)*aRow + FBoxHeight);

    PaintBox(BoxRect, aColor, aRow, aCol, Assigned(FOnCustomDrawBox));
  end;
end;

procedure TcyColorMatrix.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TcyColorMatrix.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FptMouseDown := Point(X, Y);
  inherited;
end;

procedure TcyColorMatrix.Click;
var
  aRow, aCol: integer;
begin
  if Assigned(FOnBoxClick)
  then
    if GetBoxAtPos(FptMouseDown, aRow, aCol, true)
    then
      FOnBoxClick(Self, aRow, aCol, FColorList[aCol, aRow]);

  Inherited;
end;

procedure TcyColorMatrix.SubstColor(old, New: TColor; FullRepaint: Boolean);
var
  BoxRect: TRect;
  r, c: Integer;
begin
  if FullRepaint          // More rapid but with blink effect if not doublebuffered ...
  then begin
    for c := 0 to FBoxCols - 1 do
      for r := 0 to FBoxRows - 1 do
        if FColorList[c, r] = Old
        then FColorList[c, r] := New;

    Invalidate;
  end
  else begin              // Avoid full repaint but with blinking effect ...
    for c := 0 to FBoxCols - 1 do
      for r := 0 to FBoxRows - 1 do
        if FColorList[c, r] = Old
        then begin
          FColorList[c, r] := New;

          BoxRect := classes.Rect(FBoxIntervalX + (FBoxWidth+FBoxIntervalX) * c,
                                  FBoxIntervalY + (FBoxHeight+FBoxIntervalY) * r,
                                  FBoxIntervalX + (FBoxWidth+FBoxIntervalX) * c + FBoxWidth,
                                  FBoxIntervalY + (FBoxHeight+FBoxIntervalY) * r + FBoxHeight);

          PaintBox(BoxRect, New, r, c, Assigned(FOnCustomDrawBox));
        end;
  end;
end;

procedure TcyColorMatrix.DefaultDrawBox(var aRect: TRect; aRow, aCol: integer; aColor: TColor);
begin
  if FBoxHeight = 1
  then begin
    if FBoxWidth = 1
    then begin
      // Draw a point :
      Canvas.Pixels[aRect.Left, aRect.Top] := aColor;
    end
    else begin
      // Draw horizontal line :
      Canvas.Brush.Color := aColor;
      Canvas.FillRect(aRect);
    end;
  end
  else begin
    if FBoxWidth = 1
    then begin
      // Draw vertical line :
      Canvas.Brush.Color := aColor;
      Canvas.FillRect(aRect);
    end
    else  
      case FBoxFrameWidth of
        0 : begin
              Canvas.Brush.Color := aColor;
              Canvas.FillRect(aRect);      // FillRect() is more rapid than rectangle()
            end;

        1 : begin
              Canvas.Brush.Color := aColor;
              Canvas.Pen.Width := FBoxFrameWidth;
              Canvas.Pen.Color := FBoxFrameColor;
              Canvas.Rectangle(aRect);
            end;

            else begin
              Canvas.Brush.Color := FBoxFrameColor;
              Canvas.FillRect(aRect);

              // Draw rectangle inside aRect :
              aRect := classes.Rect(aRect.Left + FBoxFrameWidth,
                                 aRect.Top + FBoxFrameWidth,
                                 aRect.Right - FBoxFrameWidth,
                                 aRect.Bottom - FBoxFrameWidth);

              Canvas.Brush.Color := aColor;
              Canvas.FillRect(aRect);
            end;
      end;
  end;
end;

procedure Register;
begin
  RegisterComponents('Cindy', [TcyColorMatrix]);
end;

end.
