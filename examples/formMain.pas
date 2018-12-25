unit formMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, db, IpHtml, Forms, Controls, Graphics, Dialogs,
  StdCtrls, LCLIntf, ExtCtrls, Buttons, DbCtrls, ComCtrls, FileUtil;

type
  TfrmMain = class(TForm)
    btnGenerate : TBitBtn;
    cbUseCache : TCheckBox;
    DBCheckBox1 : TDBCheckBox;
    DBEdit1 : TDBEdit;
    DBEdit2 : TDBEdit;
    dsData : TBufDataset;
    dsHead : TBufDataset;
    dsrcHead : TDataSource;
    GroupBox1 : TGroupBox;
    HtmlPanel : TIpHtmlPanel;
    Label1 : TLabel;
    Label2 : TLabel;
    Panel1 : TPanel;
    Panel2 : TPanel;
    Splitter1 : TSplitter;
    procedure btnGenerateClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
  private

    procedure SetupDatasets;

    function TagCallback(const tagParam: string): string;
    procedure DisplayHTML(AStream : TStream);

  public

  end;

var
  frmMain : TfrmMain;

implementation

uses math, steProcessor, steCache;

{$R *.lfm}

const
  TemplateName = 'template.thtml';

var
  Cache : TSTECache;
  FProcessor : TSTEProcessor;


procedure TfrmMain.btnGenerateClick(Sender : TObject);
var
  OutStream : TMemoryStream;
  AFile : string;
begin
  dsHead.CheckBrowseMode;
  OutStream := TMemoryStream.Create;
  try

    // set variables
    FProcessor.SetValue('gentime', FormatDateTime('dddddd tt',Now));
    FProcessor.SetValue('if_condition', (Random(2) >= 1) );

    FProcessor.AddTagCallback('mytag', @TagCallback);

    FProcessor.AddDataset('head', dsHead);
    dsData.First;
    FProcessor.AddDataset('data', dsData);

    FProcessor.SetOutput(OutStream);

    if not cbUseCache.Checked then begin
      AFile := Cache.BaseDirectory + TemplateName;
      if not FileExists(AFile) then
        raise Exception.CreateFmt('Template file %s does not exist', [AFile]);
      FProcessor.Generate(ReadFileToString(AFile));

    end else begin
      FProcessor.Template := Cache.Get(TemplateName);
      try
        FProcessor.Generate;
      finally
        Cache.Release(FProcessor.Template);
        FProcessor.Template := nil;
      end;
    end;

    //OutStream.SaveToFile('result.html');
    DisplayHTML(OutStream);
  finally
    OutStream.Free;
  end;
end;


procedure TfrmMain.FormCreate(Sender : TObject);
begin
  SetupDatasets;
end;


procedure TfrmMain.SetupDatasets;
var
  i : Integer;
begin
  with dsHead do begin
    CreateDataset;
    AppendRecord( [RandomRange(100, 200), 'Text Value', false] );
  end;

  with dsData do begin
    CreateDataset;
    for i := 0 to 10 do
      AppendRecord( [i, 'String ' + IntToStr(i), Random] );
  end;
end;

function TfrmMain.TagCallback(const tagParam : string) : string;
begin
  Result := 'This function will be called only for this tag, parameters: "' + tagParam + '"';
end;

procedure TfrmMain.DisplayHTML(AStream : TStream);
begin
  AStream.Position := 0; //rewind required for HtmlPanel
  HtmlPanel.SetHtmlFromStream(AStream);
end;


initialization
  FProcessor := TSTEProcessor.Create;
  Cache := TSTERefreshableCache.Create('./templates', '*.thtml');
  //Cache := TSTECache.Create('./templates', '*.thtml');

finalization;
  FProcessor.Free;
  Cache.Free;


end.

