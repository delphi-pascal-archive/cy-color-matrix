unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cyColorMatrix, ExtCtrls, StdCtrls, Buttons;

type
  TForm1 = class(TForm)
    LblTickCount: TLabel;
    cyColorMatrix1: TcyColorMatrix;
    LblMoyenne: TLabel;
    BtnClose: TBitBtn;
    CBPause: TCheckBox;
    SBChangeColor: TSpeedButton;
    CBFullRepaint: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure CBPauseClick(Sender: TObject);
    procedure SBChangeColorClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
// Try to activate this    DoubleBuffered := true;
end;

procedure TForm1.FormActivate(Sender: TObject);
var
  Depart, Temps, Occurences, TotMill: Cardinal;
  c, r: integer;
  Niveau: integer;
  Couleur, Actuelle: TColor;
begin
  Occurences := 0;
  TotMill := 0;

  while (not CBPause.Checked) and (not Application.Terminated) do
  begin
    Depart := GetTickCount;

    for c := 0 to cyColorMatrix1.BoxCols -1 do
    begin
      Niveau := Random(cyColorMatrix1.BoxRows-1);

      for r := 0 to cyColorMatrix1.BoxRows-1 do
      begin
        if r < Niveau
        then
          Couleur := clBlack + 100
        else
          Case r of
            0..20: Couleur  := clRed;
            21..40: Couleur := clYellow;
            41..99: Couleur := clLime;
            100..200: Couleur := $0000DF00; // verde
          end;

        Actuelle := cyColorMatrix1.GetColorGrid(r, c);

        if Actuelle <> Couleur
        then cyColorMatrix1.SetColorGrid(r, c, Couleur, false);
      end;
    end;

    inc(Occurences, 1);
    Temps := GetTickCount - Depart;         
    inc(TotMill, Temps);

    LblTickCount.Caption := IntToStr(Temps)+' milliseconds par actualisation';
    LblMoyenne.Caption := IntToStr(TotMill div Occurences)+' millisecondes de moyenne generale';
    Application.ProcessMessages;
  end;
end;

procedure TForm1.CBPauseClick(Sender: TObject);
begin
  if not CBPause.Checked
  then FormActivate(nil);

  SBChangeColor.Enabled := CBPause.Checked;
end;

procedure TForm1.SBChangeColorClick(Sender: TObject);
var i: Integer;
begin
  // Execute 100 times to compare speed :
  for i := 0 to 99 do
  begin                  
    cyColorMatrix1.SubstColor(clWhite, $0000DF00, CBFullRepaint.Checked);
    cyColorMatrix1.SubstColor($0000DF00, clWhite, CBFullRepaint.Checked);
  end;
end;

procedure TForm1.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
