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
  BlurBehindControl, FMX.Colors;

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
    procedure SwitchAnimateSwitch(Sender: TObject);
    procedure TabControlResize(Sender: TObject);
    procedure TrackBarBlurAmountChange(Sender: TObject);
    procedure SwitchBlendSwitch(Sender: TObject);
    procedure ComboColorBoxBlendChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbHasRoundCornersChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  BlurBehindControl.BlendColor := ComboColorBoxBlend.Color;
  BlurBehindControl.Corners := AllCorners;
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

procedure TFormMain.TrackBarBlurAmountChange(Sender: TObject);
begin
  LabelBlurAmount.Text := Format('%.1f', [TrackBarBlurAmount.Value]);
  BlurBehindControl.BlurAmount := TrackBarBlurAmount.Value;
end;

procedure TFormMain.SwitchBlendSwitch(Sender: TObject);
begin
	BlurBehindControl.BlendColorEnabled := SwitchBlend.IsChecked;
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
