unit BlurBehindControl;

interface

uses
	System.Classes,
	FMX.Types,
	FMX.Controls,
	FMX.Graphics,
	FMX.Filter.Effects,
	FMX.Objects,
	System.UITypes;

type
	{ A control that blurs whatever is behind it. }
	TBlurBehindControl = class(TRectangle)
{$REGION 'Internal Declarations'}
	private
		FBitmapControlBehind, FBitmapBlend, FBitmapBlurred: TBitmap;

		FGaussianBlurEffect: TGaussianBlurEffect;
		FBlurAmount: Single;
		FBlendEffect: TNormalBlendEffect;
		FBlendColor: TAlphaColor;
		FBlendColorEnabled: Boolean;

		procedure SetBlurAmount(const AValue: Single);
		procedure SetBlendColor(const AValue: TAlphaColor);
		procedure SetBlendColorEnabled(const AValue: Boolean);

	private
		procedure UpdateBitmapOfControlBehind;
		procedure UpdateBitmapBlurred;
		procedure UpdateBitmapBlend;
	protected
		procedure ParentChanged; override;
		procedure Paint; override;
{$ENDREGION 'Internal Declarations'}
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	published
		{ The blur amount. Defaults to 1.5 }
		property BlurAmount: Single read FBlurAmount write SetBlurAmount;

		property BlendColor: TAlphaColor read FBlendColor write SetBlendColor;
		property BlendColorEnabled: Boolean read FBlendColorEnabled write SetBlendColorEnabled;

		property Align;
		property Anchors;
		property ClipChildren;
		property ClipParent;
		property Cursor;
		property DragMode;
		property EnableDragHighlight;
		property Enabled;
		property Locked;
		property Height;
		property HitTest default False;
		property Padding;
		property Opacity;
		property Margins;
		property PopupMenu;
		property Position;
		property RotationAngle;
		property RotationCenter;
		property Scale;
		property Size;
		property TouchTargetExpansion;
		property Visible;
		property Width;
		property TabOrder;
		property TabStop;

		property OnPainting;
		property OnPaint;
		property OnResize;
		property OnResized;
		property OnDragEnter;
		property OnDragLeave;
		property OnDragOver;
		property OnDragDrop;
		property OnDragEnd;
		property OnClick;
		property OnDblClick;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnMouseWheel;
		property OnMouseEnter;
		property OnMouseLeave;
	end;

procedure Register;

implementation

uses
	System.Types;

procedure Register;
begin
	RegisterComponents('Grijjy', [TBlurBehindControl]);
end;

{ TBlurBehindControl }

constructor TBlurBehindControl.Create(AOwner: TComponent);
begin
	inherited;
	HitTest := False;
	// FBitmapBlurred := TBitmap.Create;
	FBitmapControlBehind := TBitmap.Create;

	FGaussianBlurEffect := TGaussianBlurEffect.Create(Self);
	FBlurAmount := 1.5;

	FBlendColorEnabled := False;
	FBlendEffect := TNormalBlendEffect.Create(Self);
	FBitmapBlend := TBitmap.Create;

	Fill.Kind := TBrushKind.Bitmap;
	Fill.Bitmap.WrapMode := TWrapmode.TileOriginal;

	Stroke.Kind := TBrushKind.None;
	XRadius := 15;
	YRadius := 15;

end;

destructor TBlurBehindControl.Destroy;
begin
	// FBitmapBlurred.Free;
	FBitmapBlend.Free;
	FBitmapControlBehind.Free;
	inherited;
end;

procedure TBlurBehindControl.ParentChanged;
begin
	inherited;
	{ Our code only works if the parent is a TControl (or nil).
		So you cannot place this control directly on a form. }
	if (Parent <> nil) and (not(Parent is TControl)) then
		raise EInvalidOperation.Create('A TBlurBehindControl can only be placed inside another control');
end;

procedure TBlurBehindControl.SetBlurAmount(const AValue: Single);
begin
	if (AValue <> FBlurAmount) then
	begin
		FBlurAmount := AValue;
		Repaint;
	end;
end;

procedure TBlurBehindControl.SetBlendColor(const AValue: TAlphaColor);
begin
	if (AValue <> FBlendColor) then
	begin
		FBlendColor := AValue;
		Repaint;
	end;
end;

procedure TBlurBehindControl.SetBlendColorEnabled(const AValue: Boolean);
begin
	if (AValue <> FBlendColorEnabled) then
	begin
		FBlendColorEnabled := AValue;
		Repaint;
	end;
end;

procedure TBlurBehindControl.Paint;
begin

	{ Copy the visual of the parent control to FBitmapOfControlBehind. }
	UpdateBitmapOfControlBehind;

	{ Resize the bitmap to a smaller size before image effect processing to reduce processing time. }
	FBitmapBlurred := FBitmapControlBehind.CreateThumbnail(round(FBitmapControlBehind.Width / 2), round(FBitmapControlBehind.Height / 2));

	UpdateBitmapBlend;

	UpdateBitmapBlurred;

	{ Resize the bitmap to the original size for painting. }
	var bitmapforPainting := FBitmapBlurred.CreateThumbnail(FBitmapControlBehind.Width, FBitmapControlBehind.Height);

	try

		Fill.Bitmap.Bitmap := bitmapforPainting;
		inherited Paint;

	finally
		bitmapforPainting.Free;
		FBitmapBlurred.Free;
	end;

end;

procedure TBlurBehindControl.UpdateBitmapBlend();
begin

	if (not FBlendColorEnabled) then
		Exit;

	FBitmapBlend.SetSize(FBitmapBlurred.Size);
	FBitmapBlend.Clear(FBlendColor);
	FBlendEffect.Target := FBitmapBlend;

	FBlendEffect.ProcessEffect(
		{ Canvas. Not used for GPU accelerated effects. }
		nil,

		{ Bitmap to apply effect to. }
		FBitmapBlurred,

		{ Any data to pass to effect. Not used. }
		0);
end;

procedure TBlurBehindControl.UpdateBitmapBlurred;
begin

	{ Apply blur }
	FGaussianBlurEffect.BlurAmount := FBlurAmount;
	FGaussianBlurEffect.ProcessEffect(
		{ Canvas. Not used for GPU accelerated effects. }
		nil,

		{ Bitmap to apply effect to. }
		FBitmapBlurred,

		{ Any data to pass to effect. Not used. }
		0);

	{ Blur a second time at a lower amount to reduce the "box blur" effect. }
	FGaussianBlurEffect.BlurAmount := FBlurAmount * 0.4;
	FGaussianBlurEffect.ProcessEffect(nil, FBitmapBlurred, 0);
end;

procedure TBlurBehindControl.UpdateBitmapOfControlBehind;
var
	ControlBehind: TControl;
	TargetWidth, TargetHeight: Integer;

	procedure PaintPartToBitmap(const Control: TControl; const SourceRect, TargetRect: TRect; const Bitmap: TBitmap);
	var ClipRects: TClipRects;
		X, Y: Single;
	begin
		ClipRects := [TRectF.Create(TargetRect)];
		if (Bitmap.Canvas.BeginScene(@ClipRects)) then
			try
				FDisablePaint := True;
				X := TargetRect.Left - SourceRect.Left;
				Y := TargetRect.Top - SourceRect.Top;
				Control.PaintTo(Bitmap.Canvas, RectF(X, Y, X + Control.Width, Y + Control.Height));
			finally
				FDisablePaint := False;
				Bitmap.Canvas.EndScene;
			end;
	end;

begin

	{ The parent should be a TControl. This is checked in ParentChanged. }
	Assert(Parent is TControl);
	ControlBehind := TControl(Parent);

	TargetWidth := round(Width);
	TargetHeight := round(Height);
	FBitmapControlBehind.SetSize(TargetWidth, TargetHeight);

	PaintPartToBitmap(ControlBehind, BoundsRect.round, TRect.Create(0, 0, TargetWidth, TargetHeight), FBitmapControlBehind);

end;

end.
