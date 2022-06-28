unit view.main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons;

type

  { TfmPrincipal }

  TfmPrincipal = class(TForm)
    BtnLocalizar: TBitBtn;
    BtnSeIbExpert: TBitBtn;
    edtLocalizacao: TEdit;
    gbDatabase: TGroupBox;
    gb_fb_versao: TRadioGroup;
    lblStatus: TLabel;
    lblTitulo: TLabel;
    memoExplica: TMemo;
    OpenDialog1: TOpenDialog;
    pnlSave: TPanel;
    pnlTitulo: TPanel;
    procedure BtnLocalizarClick(Sender: TObject);
    procedure BtnSeIbExpertClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FConfigFile: String;
    FCurDir: String;
    FdoMessage: String;
    procedure ReadConfig;
    procedure SetdoMessage(AValue: String);
    procedure WriteConfig;
  public
    property CurDir:String read FCurDir write FCurDir;
    property ConfigFile:String read FConfigFile write FConfigFile;
    property doMessage:String read FdoMessage write SetdoMessage;
  end;

var
  fmPrincipal: TfmPrincipal;

implementation
uses
  inifiles;

{$R *.lfm}

{ TfmPrincipal }

const
  IBEXPERT_AUTOCONFIG_NAME='ibexpert_autoconfig.reg';

procedure TfmPrincipal.FormCreate(Sender: TObject);
begin
  FCurDir:=ExtractFilePath(Application.ExeName);
  FConfigFile:=FCurDir+PathDelim+ChangeFileExt(ExtractFileName(Application.ExeName),'')+'.ini';
  edtLocalizacao.ReadOnly:=true;
  edtLocalizacao.Text:=FCurDir;
  lblSTatus.Caption:=emptyStr;

end;

procedure TfmPrincipal.FormShow(Sender: TObject);
begin
  ReadConfig;
end;

procedure TfmPrincipal.ReadConfig;
var
  MyIni:TiniFile;
begin
  if FileExists(ConfigFile) then
  begin
    myIni:=TInifile.Create(ConfigFile);
    try
      edtLocalizacao.Text:=Myini.ReadString('Main',edtLocalizacao.name,edtLocalizacao.Text);

    finally
    end;
    myIni.Free;
  end;
end;

procedure TfmPrincipal.SetdoMessage(AValue: String);
begin
  if FdoMessage=AValue then Exit;
  FdoMessage:=AValue;
  lblStatus.Caption:=FdoMessage;
end;

procedure TfmPrincipal.WriteConfig;
var
  MyIni:TiniFile;
begin
  myIni:=TInifile.Create(ConfigFile);
  try
    Myini.WriteString('Main',edtLocalizacao.name,edtLocalizacao.Text);
  finally
  end;
  myIni.Free;
end;

procedure TfmPrincipal.BtnLocalizarClick(Sender: TObject);
begin
  OpenDialog1.InitialDir:=ExtractFilePath(edtLocalizacao.Text);
  if OpenDialog1.Execute then
  begin
    edtLocalizacao.Text:=OpenDialog1.FileName;
  end;
end;

procedure TfmPrincipal.BtnSeIbExpertClick(Sender: TObject);
var
  L:TStringList;
  sUDBConnectString:String;
  sErrorMsg:String;
  sSaveAs:String;
begin
  sErrorMsg:=emptyStr;
  if gb_fb_versao.ItemIndex<0 then
  begin
    if gb_fb_versao.CanFocus then
    begin
      gb_fb_versao.SetFocus;
      sErrorMsg:='Escolha a versão do Firebird';
    end;
  end;
  if not FileExists(edtLocalizacao.Text) then
  begin
    if edtLocalizacao.CanFocus then
    begin
      edtLocalizacao.SetFocus;
      sErrorMsg:='Arquivo de configuração do IbExpert não existe: '+edtLocalizacao.Text+sLineBreak+
        'Neste caso, faça a configuração manualmente no IBExpert indicando um arquivo válido e ele mesmo criará para você um.';
    end;
  end;
  if sErrorMsg=emptyStr then
  begin
    WriteConfig;
    sSaveAs:=ExtractFilePath(edtLocalizacao.Text)+IBEXPERT_AUTOCONFIG_NAME;
    sUDBConnectString:=edtLocalizacao.Text;
    sUDBConnectString:=StringReplace(sUDBConnectString,PathDelim,PathDelim+PathDelim, [rfReplaceAll]);
    L:=TStringList.Create;
    try
      L.Add('Windows Registry Editor Version 5.00');
      L.Add('');
      L.Add('[HKEY_CURRENT_USER\SOFTWARE\HK Software]');
      L.Add('');
      L.Add('[HKEY_CURRENT_USER\SOFTWARE\HK Software\IBExpert]');
      L.Add('');
      L.Add('[HKEY_CURRENT_USER\SOFTWARE\HK Software\IBExpert\CurrentData]');
      L.Add('"MultipleInstances"=dword:00000001');
      //L.Add('"LastCompressDate"=hex:eb,59,24,8d,ac,98,e5,40');
      L.Add('"Interface"="MDI"');
      //L.Add('"MainFormH"="394488"');
      L.Add('"AllowUDB"=dword:00000001');
      //L.Add('"UDBConnectString"="D:\\Dev\\FirebirdSQL\\IbExpert-2020.04\\IBEXPERT_FB3.FDB"');
      L.Add('"UDBConnectString"="'+sUDBConnectString+'"');
      L.Add('"UDBUserName"="SYSDBA"');
      L.Add('"UDBPassword"="masterkey"');
      L.Add('"UDBClientLib"="fbclient.dll"');
      L.Add('"NoSplash"=dword:00000001');
      L.Add('');
      //L.Add('[HKEY_CURRENT_USER\SOFTWARE\HK Software\IBExpert\RunDir]');
      //L.Add('@="D:\\Dev\\FirebirdSQL\\IbExpert-2020.04\\IBEXPERT_FB3.FDB"');
      L.SaveToFile(sSaveAs);
      doMessage:='Arquivo gerado com sucesso:'+sLineBreak+sSaveAs;
    except
      on e:exception do sErrorMsg:=e.message;
    end;
  end;
  L.Free;
  if sErrorMsg<>emptyStr then
  begin
    doMessage:='Erro:'+sLineBreak+sErrorMsg;
  end;
end;

end.

