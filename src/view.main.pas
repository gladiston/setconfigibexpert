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
    BtnLocalizar1: TBitBtn;
    BtnSeIbExpert: TBitBtn;
    UDBConnectString: TEdit;
    UDBClientLib: TEdit;
    gb_UDBConnectString: TGroupBox;
    gb_UDBClientLib: TGroupBox;
    gb_fb_versao: TRadioGroup;
    gb_Interface: TRadioGroup;
    gb_splash: TRadioGroup;
    lblStatus: TLabel;
    memoExplica: TMemo;
    OpenDialog1: TOpenDialog;
    pnlSave: TPanel;
    pnlTitulo: TPanel;
    pnl_area1: TPanel;
    procedure BtnLocalizar1Click(Sender: TObject);
    procedure BtnLocalizarClick(Sender: TObject);
    procedure BtnSeIbExpertClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gb_fb_versaoClick(Sender: TObject);
  private
    FConfigFile: String;
    FCurDir: String;
    FdoMessage: String;
    procedure ReadConfig;
    procedure SetdoMessage(AValue: String);
    procedure WriteConfig;
    function SaveInRegistry: String;
    function ExportToReg(ARegFile: String): String;
  public
    property CurDir:String read FCurDir write FCurDir;
    property ConfigFile:String read FConfigFile write FConfigFile;
    property doMessage:String read FdoMessage write SetdoMessage;
  end;

var
  fmPrincipal: TfmPrincipal;

implementation
uses
  inifiles,
  registry;

{$R *.lfm}

{ TfmPrincipal }

const
  IBEXPERT_AUTOCONFIG_NAME='ibexpert_autoconfig.reg';
  IBEXPERT_CurrentData='\SOFTWARE\HK Software\IBExpert\CurrentData';

procedure TfmPrincipal.FormCreate(Sender: TObject);
begin
  FCurDir:=ExtractFilePath(Application.ExeName);
  FConfigFile:=FCurDir+PathDelim+ChangeFileExt(ExtractFileName(Application.ExeName),'')+'.ini';

  UDBConnectString.ReadOnly:=true;
  UDBConnectString.Text:=FCurDir;
  lblSTatus.Caption:=emptyStr;
  Application.Title:='Autoconfigurador do IBExpert';
  Caption:=Application.Title;

  with MemoExplica.Lines do
  begin
    Clear;
    Add('Restaura as configura????es do IBExpert num ??nico clique');
    Add('');
    Add(
      'Se voc?? usa o banco de dados FirebirdSQL e usa o aplicativo IBExpert para '+
      'gerenci??-lo, talvez este programa seja para voc??. Mas antes de usar este '+
      'programa voc?? deve seguir este passo a passo: ');
    Add(
      '(1) Voc?? deve configurar o IBExpert exatametne como deseja, com '+
      'todos ajustes, conex??es, pastas...tudo como deveria ser. ');
    Add(
      '(2) Ap??s a configura????o inicial e ajustes, v?? no menu '+
      'Options|Envoroment Options e selecionar a guia "IBExpert User Database" e '+
      'ent??o em "User Database Connection String" definir um banco de dados local '+
      'ser?? onde o IBExpert guardar?? todos esses ajustes, por exemplo, '+
      'C:\MyDatabases\IbExpert_FB3.fdb. ');
    Add(
      '(3) Ainda na guia "IBExpert User Database", caso deseje tamb??m que '+
      'seus projetos de banco de dados sejam salvos ent??o marcar a op????o '+
      '"Store Project View Data in User Database" ');
    Add(
      '(4) Ainda na guia "IBExpert User Database" clique em "OK" e o IBExpert '+
      'criar?? este banco com todas as suas defini????es atuais e a armazenar?? neste '+
      'banco. Como dito, o IBExpert armazenar?? toda sua configura????o, pastas, '+
      'conex??es, hist??ricos,... nele e a atualizar?? a cada nova interatividade '+
      'com bancos e/ou projetos novos.');
    Add('');
    Add(
      'Uma vez que tem um arquivo de dados de configura????o do IBExpert, voc?? pode '+
      'levar o IBExpert de um local para outro e todos os seus ajustes estar??o '+
      'onde voc?? for, mas ter?? de repetir os passos 2 at?? o 4. '+
      '?? aqui que o programa "setconfigibexpert" entra, ele repetir?? os passos 2 '+
      'a 4 por voc??, basta dizer onde est?? este banco de dados que guarda as '+
      'defini????es do IBExpert e ele far?? o ajuste necess??rio de que precisa, '+
      'economizando tempo.');
  end;

end;

procedure TfmPrincipal.FormShow(Sender: TObject);
begin
  ReadConfig;
end;

procedure TfmPrincipal.gb_fb_versaoClick(Sender: TObject);
begin
  if gb_fb_versao.ItemIndex=0 then
  begin
    // fb3
    gb_UDBConnectString.Caption:='Localiza????o do UserDatabase do IBExpert(no formato FB3):';
    gb_UDBClientLib.Caption:='fbclient do FB3(32bits) para o IBExpert acessar UserDatabase:';
  end
  else
  begin
    gb_UDBConnectString.Caption:='Localiza????o do UserDatabase do IBExpert(no formato FB4):';
    gb_UDBClientLib.Caption:='fbclient do FB4(32bits) para o IBExpert acessar UserDatabase:';
  end;
end;

procedure TfmPrincipal.ReadConfig;
var
  MyIni:TiniFile;
  Reg:TRegistry;
  S:String;
  i:Integer;
  openResult:Boolean;
begin
  if FileExists(ConfigFile) then
  begin
    myIni:=TInifile.Create(ConfigFile);
    try
      UDBConnectString.Text:=Myini.ReadString('Main',UDBConnectString.name,UDBConnectString.Text);
    finally
    end;
    myIni.Free;
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      reg.Access := KEY_READ;
      if (reg.KeyExists(IBEXPERT_CurrentData)) then
      begin
        openResult := Reg.OpenKey(IBEXPERT_CurrentData+'\',False);
        S:=Reg.ReadString('UDBConnectString');
        if S<>emptyStr then
        begin
          S:=StringReplace(S, PathDelim+PathDelim,PathDelim, [rfReplaceAll]);
          UDBConnectString.Text:=S;
        end;
        S:=Reg.ReadString('Interface');
        if SameText(S,'MDI')
          then gb_Interface.ItemIndex:=0
          else gb_Interface.ItemIndex:=1;
        i:=Reg.ReadInteger('NoSplash');
        if i=0 then
          gb_Splash.ItemIndex:=1;
        S:=Reg.ReadString('UDBClientLib');
        if S<>emptyStr then
        begin
          S:=StringReplace(S, PathDelim+PathDelim,PathDelim, [rfReplaceAll]);
          UDBClientLib.Text:=S;
        end;
      end;
    finally
      Reg.Free;
    end;
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
    Myini.WriteString('Main',UDBConnectString.name,UDBConnectString.Text);
  finally
  end;
  myIni.Free;
end;

function TfmPrincipal.SaveInRegistry: String;
var
  Reg: TRegistry;
  openResult : Boolean;
  sUDBUserName:String;
  sUDBPassword:String;
  sUDBClientLib:String;
  sUDBConnectString:String;
begin
  Result:=emptyStr;
  sUDBUserName:='SYSDBA';
  sUDBPassword:='masterkey';
  sUDBClientLib:=UDBClientLib.Text;
  sUDBClientLib:=StringReplace(sUDBClientLib,PathDelim,PathDelim+PathDelim, [rfReplaceAll]);
  sUDBConnectString:=UDBConnectString.Text;
  sUDBConnectString:=StringReplace(sUDBConnectString,PathDelim,PathDelim+PathDelim, [rfReplaceAll]);
  Reg := TRegistry.Create(KEY_READ or KEY_WOW64_64KEY);
  try
    // Indo para a chave:
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.Access := KEY_WRITE;
    openResult := Reg.OpenKey(IBEXPERT_CurrentData+'\',True);
    Reg.WriteInteger('MultipleInstances',00000001);  // dword
    if gb_Interface.ItemIndex=0
      then Reg.WriteString('Interface','MDI')  // string
      else Reg.WriteString('Interface','SDI');  // string;
    Reg.WriteInteger('AllowUDB',00000001);  // dword
    Reg.WriteString('UDBConnectString',sUDBConnectString);  // string
    Reg.WriteString('UDBUserName',sUDBUserName);  // string
    Reg.WriteString('UDBPassword',sUDBPassword);  // string
    Reg.WriteString('UDBClientLib',sUDBClientLib);  // string
    if gb_Splash.ItemIndex=0
      then Reg.WriteInteger('NoSplash',00000001)  // dword
      else Reg.WriteInteger('NoSplash',00000000);  // dword
    Reg.CloseKey;
  except
    on e:exception do Result:=e.message;
  end;
  Reg.Free;  // In non-Windows operating systems this flushes the Reg.xml file to disk
end;

function TfmPrincipal.ExportToReg(ARegFile: String): String;
var
  sUDBConnectString:String;
  sUDBClientLib:String;
  L:TStringList;
begin
  Result:=emptyStr;
  sUDBConnectString:=UDBConnectString.Text;
  sUDBConnectString:=StringReplace(sUDBConnectString,PathDelim,PathDelim+PathDelim, [rfReplaceAll]);
  sUDBClientLib:=UDBClientLib.Text;
  sUDBClientLib:=StringReplace(sUDBClientLib,PathDelim,PathDelim+PathDelim, [rfReplaceAll]);

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
    L.Add('"Interface"="MDI"');
    L.Add('"AllowUDB"=dword:00000001');
    L.Add('"UDBConnectString"="'+sUDBConnectString+'"');
    L.Add('"UDBUserName"="SYSDBA"');
    L.Add('"UDBPassword"="masterkey"');
    L.Add('"UDBClientLib"="'+sUDBClientLib+'"');
    L.Add('"NoSplash"=dword:00000001');
    L.Add('');

    L.SaveToFile(ARegFile);
    if not FileExists(ARegFile) then
      raise exception.create('Arquivo n??o pode ser gerado: '+ARegFile);
  except
    on e:exception do Result:=e.message;
  end;
  L.Free;
end;

procedure TfmPrincipal.BtnLocalizarClick(Sender: TObject);
begin
  OpenDialog1.InitialDir:=ExtractFilePath(UDBConnectString.Text);
  OpenDialog1.Filter:='Firebird Databases|*.fdb|Todos|*.*';
  if OpenDialog1.Execute then
  begin
    UDBConnectString.Text:=OpenDialog1.FileName;
  end;
end;

procedure TfmPrincipal.BtnLocalizar1Click(Sender: TObject);
var
  sDef_Path:String;
begin
  sDef_Path:=ExtractFilePath(Application.Exename);
  if gb_fb_versao.ItemIndex<>0 then //fb4+
  begin
    if DirectoryExists('C:\Program Files\Firebird\Firebird_4_0\WOW64') then
      sDef_Path:='C:\Program Files\Firebird\Firebird_4_0\WOW64';
  end;
  if gb_fb_versao.ItemIndex=0 then //fb3
  begin
    if DirectoryExists('C:\Program Files\Firebird\Firebird_3_0\WOW64') then
      sDef_Path:='C:\Program Files\Firebird\Firebird_3_0\WOW64';
  end;
  OpenDialog1.InitialDir:=sDef_Path;
  OpenDialog1.Filter:='Firebird Client(fbclient.dll)|*.dll|Todos|*.*';
  if OpenDialog1.Execute then
  begin
    UDBClientLib.Text:=OpenDialog1.FileName;
  end;
end;

procedure TfmPrincipal.BtnSeIbExpertClick(Sender: TObject);
var
  Registry: TRegistry;
  sErrorMsg:String;
  sSaveRegAs:String;
begin
  sErrorMsg:=emptyStr;
  if gb_fb_versao.ItemIndex<0 then
  begin
    if gb_fb_versao.CanFocus then
    begin
      gb_fb_versao.SetFocus;
      sErrorMsg:='Escolha a vers??o do Firebird';
    end;
  end;
  if not FileExists(UDBConnectString.Text) then
  begin
    if UDBConnectString.CanFocus then
    begin
      UDBConnectString.SetFocus;
      sErrorMsg:='Arquivo de configura????o do IbExpert n??o existe: '+UDBConnectString.Text+sLineBreak+
        'Neste caso, fa??a a configura????o manualmente no IBExpert indicando um arquivo v??lido e ele mesmo criar?? para voc?? um.';
    end;
  end;
  if sErrorMsg=emptyStr then
  begin
    WriteConfig;
    sSaveRegAs:=ExtractFilePath(UDBConnectString.Text)+IBEXPERT_AUTOCONFIG_NAME;
    //sErrorMsg:=ExportToReg(sSaveRegAs);
    sErrorMsg:=SaveInRegistry;
  end;

  if sErrorMsg<>emptyStr then
  begin
    doMessage:='Erro:'+sLineBreak+sErrorMsg;
  end;
end;

end.

