unit Form.Main;
{ Photo credits:
  * Yosemite Valley by Joshua Earle (https://unsplash.com/photos/zIg3DiAwFD4)
  * Canyon by Andre Iv (https://unsplash.com/photos/HFtz1LKWvxU) }

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Objects,
  FMX.TabControl,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Ani,
  FMX.Filter.Effects,
   FMX.Platform,
  FMX.Colors, BlurBehindControl;

type


  TFormMain = class(TForm)
    TabControl: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Image1: TImage;
    Label1: TLabel;
    Image2: TImage;
    Label2: TLabel;
    Container: TRectangle;
    BlurBehindControl: TBlurBehindControl;
    FloatAnimationPosX: TFloatAnimation;
    ToolBar: TToolBar;
    SwitchAnimate: TSwitch;
    LabelAnimate: TLabel;
    FloatAnimationPosY: TFloatAnimation;
    LayoutSwitch: TLayout;
    LayoutBlurAmount: TLayout;
    TrackBarBlurAmount: TTrackBar;
    LabelBlurAmount: TLabel;
    ImageCodeRage: TImage;
    Layout1: TLayout;
    ComboColorBoxBlend: TComboColorBox;
    Label3: TLabel;
    SwitchBlend: TSwitch;
    cbHasRoundCorners: TCheckBox;
    btnEffect: TButton;
    faEffect: TFloatAnimation;
    Timer1: TTimer;
    procedure SwitchAnimateSwitch(Sender: TObject);
    procedure TabControlResize(Sender: TObject);
    procedure TrackBarBlurAmountChange(Sender: TObject);
    procedure SwitchBlendSwitch(Sender: TObject);
    procedure ComboColorBoxBlendChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbHasRoundCornersChange(Sender: TObject);
    procedure faEffectProcess(Sender: TObject);
    procedure btnEffectClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
  	FRenderTime: Single;
    FFrameCount: Integer;
    FUpdateRects: array of TRectF;
    FTimerService: IFMXTimerService;
    FFps: Single;
  protected
    procedure PaintRects(const UpdateRects: array of TRectF); override;
  public
  end;

var
  FormMain: TFormMain;

Implementation
uses diagnostics;

{$R *.fmx}

procedure TFormMain.faEffectProcess(Sender: TObject);
begin
  BlurBehindControl.Scale.Y := BlurBehindControl.Scale.X;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
	TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService);
  BlurBehindControl.BlendColor := ComboColorBoxBlend.Color;
  BlurBehindControl.Corners := AllCorners;
end;

procedure TFormMain.PaintRects(const UpdateRects: array of TRectF);
var
  C: Double;
begin
  if Length(UpdateRects) > 0 then
  begin
    SetLength(FUpdateRects, Length(UpdateRects));
    Move(UpdateRects[0], FUpdateRects[0], SizeOf(TRectF) * Length(UpdateRects));
    C := FTimerService.GetTick;
    inherited;
    FRenderTime := FRenderTime + (FTimerService.GetTick - C);
    FFrameCount := FFrameCount + 1;
  end;
end;


procedure TFormMain.SwitchAnimateSwitch(Sender: TObject);
begin
  if (SwitchAnimate.IsChecked) then
    BlurBehindControl.Align := TAlignLayout.None;

  FloatAnimationPosX.Enabled := SwitchAnimate.IsChecked;
  FloatAnimationPosY.Enabled := SwitchAnimate.IsChecked;

  if (not SwitchAnimate.IsChecked) then
    BlurBehindControl.Align := TAlignLayout.Center;
end;

procedure TFormMain.TabControlResize(Sender: TObject);
begin
  BlurBehindControl.Width := TabControl.Width * 0.6;
  BlurBehindControl.Height := TabControl.Height * 0.4;
  FloatAnimationPosX.StopValue := TabControl.Width - BlurBehindControl.Width;
  FloatAnimationPosY.StopValue := TabControl.Height - BlurBehindControl.Height;
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
    if FFrameCount > 0 then
  begin
    FFps := 1 / (FRenderTime / FFrameCount);
    Caption := Round(FFps).ToString + ' FPS';
    FFrameCount := 0;
    FRenderTime := 0;
  end;
end;

procedure TFormMain.TrackBarBlurAmountChange(Sender: TObject);
begin
  LabelBlurAmount.Text := Format('%.1f', [TrackBarBlurAmount.Value]);
  BlurBehindControl.BlurAmount := TrackBarBlurAmount.Value;
end;

procedure TFormMain.SwitchBlendSwitch(Sender: TObject);
begin
	BlurBehindControl.BlendColorEnabled := SwitchBlend.IsChecked;
end;

procedure TFormMain.btnEffectClick(Sender: TObject);
begin
	faEffect.Start;
end;

procedure TFormMain.cbHasRoundCornersChange(Sender: TObject);
begin
	if cbHasRoundCorners.IsChecked then
		BlurBehindControl.Corners := AllCorners
  else
  	BlurBehindControl.Corners := [];
end;

procedure TFormMain.ComboColorBoxBlendChange(Sender: TObject);
begin
	BlurBehindControl.BlendColor := ComboColorBoxBlend.Color;
end;



end.
