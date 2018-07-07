unit formMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, db, FileUtil, IpHtml, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLIntf, ExtCtrls, Buttons, DbCtrls;

type
  TfrmMain = class(TForm)
    btnGenerate : TBitBtn;
    DBCheckBox1 : TDBCheckBox;
    DBEdit1 : TDBEdit;
    DBEdit2 : TDBEdit;
    dsrcHead : TDataSource;
    dsData : TBufDataset;
    dsHead : TBufDataset;
    GroupBox1 : TGroupBox;
    HtmlPanel : TIpHtmlPanel;
    Label1 : TLabel;
    Label2 : TLabel;
    Panel1 : TPanel;
    procedure btnGenerateClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
  private

    function TagCallback(const tagParam: string): string;
    procedure SetupDatasets;
    procedure DisplayHTML(AStream : TStream);

  public

  end;

var
  frmMain : TfrmMain;

implementation

uses math, steProcessor;

{$R *.lfm}


procedure TfrmMain.btnGenerateClick(Sender : TObject);
var
  OutStream : TMemoryStream;
  AProcessor : TSTEProcessor;
begin
  dsHead.CheckBrowseMode;

  OutStream := TMemoryStream.Create;
  try

    AProcessor := TSTEProcessor.Create;
    try

      // set variables
      AProcessor.SetValue('gentime', FormatDateTime('dddddd tt',Now));
      AProcessor.SetValue('if_condition', (Random(2) >= 1) );
      AProcessor.AddTagCallback('callback', @TagCallback);

      AProcessor.AddDataset('head', dsHead);
      dsData.First;
      AProcessor.AddDataset('data', dsData);

      AProcessor.SetOutput(OutStream);
      AProcessor.Generate(ReadFileToString('./template.thtml'));

    finally
      AProcessor.Free;
    end;

    DisplayHTML(OutStream);
  finally
    OutStream.Free;
  end;

end;

procedure TfrmMain.FormCreate(Sender : TObject);
begin
  SetupDatasets;
end;

function TfrmMain.TagCallback(const tagParam : string) : string;
begin
  Result := 'This function will be called only for this tag, params: "' + tagParam + '"';
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

procedure TfrmMain.DisplayHTML(AStream : TStream);
begin
  AStream.Position := 0; //rewind required for HtmlPanel
  HtmlPanel.SetHtmlFromStream(AStream);
end;


end.

