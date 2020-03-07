program BlurBehindDemo;

uses
  System.StartUpCopy,
  FMX.Types,
  FMX.Forms,
  Form.Main in 'Form.Main.pas' {FormMain},
  BlurBehindControl in '..\BlurBehindControl.pas';

{$R *.res}

begin

ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
