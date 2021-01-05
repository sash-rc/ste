unit steProcessor;

(*
+--------------------------+
|  ┏━┓╺┳╸┏━╸ Simple          |
|  ┗━┓ ┃ ┣╸  Template        |
|    ┃ ┃ ┃   Engine          |
|  ┗━┛ ╹ ┗━╸ for Free Pascal |
+ -------------------------+
*)

{$mode objfpc}{$H+}


// generate output with previously prepared template

interface

uses
  Classes, SysUtils, steParser, db;

type
  TSTEExpandTagProc = function (const tagParam: string): string of object;
  TSTEEvaluateCondProc = function (const ACondName: string): boolean of object;

  TSTEExpandTagCallbackInfo = record
    Tag : string;
    Proc : TSTEExpandTagProc;
  end;

  TSTEConditionCallbackInfo = record
    ConditionName : string;
    Proc : TSTEEvaluateCondProc;
  end;

type
  TSTEProcessor = class
  private
    FCurrentToken : integer;
    FGenerating : boolean; // for handle recursion
    FUseDataSetPrefix : boolean; // ise prefixes for dataset names
    FDSNamespaceLength : integer;
    procedure DisposeValues;
    function GetOwnsDatasets : boolean;
    procedure SetOwnsDatasets(AValue : boolean);

  protected
    FOutput : TStream; // not owned
    FTemplate : TSTEParsedTemplateData; // template being generated, not owned

    FDatasets : TStringList;
    FValues : TStringList; //todo: replace with TFPStringHashTable

    FExpandTagCallbacks : array of TSTEExpandTagCallbackInfo;
    FConditionalCallbacks : array of TSTEConditionCallbackInfo;

    procedure ProcessTextToken(token : PSTEParserToken);
    procedure ProcessTokens(iEnd : integer);
    procedure ProcessIfBlock(token : PSTEParserToken);
    procedure ProcessForBlock(token : PSTEParserToken);
    procedure ProcessSetBlock(token : PSTEParserToken);


    function CheckDSNamespaceDataset(const PrefixedDsName : string; out PureDsName : string) : boolean;
    function GetDataset(const dsName : string) : TDataset;

    function GetFieldValue(const dsName, fieldName : string; out Value : string) : boolean;
    function GetFieldValue(const dsName, fieldName : string; out Value : boolean) : boolean;

    function ExpandTag(const tagName, tagParam: string): string; virtual;
    function EvaluateIf(const Param : string) : boolean; virtual;
    function BeginFor(const Param: string; out data : pointer) : boolean; virtual; // should return true to begin loop
    function EndFor(data : pointer) : boolean; virtual; // should return true to stop loop (at EOF)

    function CheckTagCallbacks(const tagName, tagParam: string; var Value : string) : boolean;
    function CheckConditionalCallbacks(const ACondName: string; var Value : boolean) : boolean;
  public

    DataSetPrefix : string; // prefix for datasets (for more readable distinction) // set to empty to disable feature
    // callback should return false, if tag not handled
    OnExpandTag : function (const tagName, tagParam: string; out Value : string) : boolean of object;

    UnkownTagFeedback : boolean;  // When true (default) all unknown tags echoed to output using UnkownTagFeedbackFormat
                                  // You may want to disable this feature for production
    UnkownTagFeedbackFormat : string;

    property OwnsDatasets : boolean read GetOwnsDatasets write SetOwnsDatasets;
    procedure AddDataset(const dsName : string; ds : TDataset);


    //===== todo ? : should be values stored as variant ?
    procedure SetValue(const key, value : string);
    procedure SetValue(const key : string; const value : boolean);
    procedure SetValue(const key : string; const value : integer);
    procedure SetValue(const key : string; const value : single);
    procedure SetValue(const key : string; const value : double);

    procedure AddTagCallback(const tagName : string; ACallbackProc : TSTEExpandTagProc);
    procedure AddConditionCallback(const ACondName : string; ACallbackProc : TSTEEvaluateCondProc);

    property Output : TStream read FOutput write FOutput;
    property Template : TSTEParsedTemplateData read FTemplate write FTemplate;

    procedure Generate(ATemplate : TSTEParsedTemplateData; AStream : TStream);
    procedure Generate; // to/from prevoiously assigned external OutputStream and prepared Template

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses strutils;


const
  steNumericFieldTypes = [ftInteger, ftSmallint, ftWord, ftFloat, ftAutoInc, ftBCD, ftLargeint];


function TSTEProcessor.ExpandTag(const tagName, tagParam: string): string;
var
  i : integer;
  dsname : string;
begin

  if Assigned(OnExpandTag) then
    if OnExpandTag(tagName, tagParam, Result) then
      Exit;


  if CheckDSNamespaceDataset(tagName, dsname) then begin // try if this is a dataset field
    if GetFieldValue(dsname, tagParam, Result) then
      Exit;
  end else begin

    //try callbacks
    if CheckTagCallbacks(tagName, tagParam, Result) then
      Exit;

    //try plain values
    if (tagParam = '') then begin
      i := FValues.IndexOf(tagName);
      if i <> -1 then begin
        Result := Pstring(FValues.Objects[i])^;
        Exit;
      end;
    end;

  end;

  // nothing found
  if UnkownTagFeedback then
    Result := format( UnkownTagFeedbackFormat, [tagName] ) //echo tag --- for debug purposes
  else
   Result := '';
end;

function TSTEProcessor.EvaluateIf(const Param: string): boolean;
var
  i: Integer;
  fldName, dsName : string;
  Value : boolean;
const
  psep : TSysCharSet = [' '];
begin
  Result := false;

  //try callbacks
  if CheckConditionalCallbacks(Param, Value) then begin
    Result := Value;
    Exit;
  end;

  i := FValues.IndexOf(Param);
  if i <> -1 then begin
    Result := ( PString(FValues.Objects[i])^ <> '' );
  end else begin
    //------- try dataset
    if CheckDSNamespaceDataset(ExtractWord(1, Param, psep), dsName) then begin
      fldName := ExtractWord(2, Param, psep);
      if fldName = '' then
        Exit;
      if GetFieldValue(dsName, fldName, Value) then
        Result := Value;
    end;
  end;
end;

function TSTEProcessor.BeginFor(const Param: string; out data: pointer): boolean;
var
  ds : TDataSet;
  dsname : string;
begin
  if CheckDSNamespaceDataset(Param, dsname) then begin
    ds := GetDataset(dsname);
    Result := ( (ds <> nil) and ds.Active and (not ds.EOF));
    data := ds;
  end else
    Result := false;
end;

function TSTEProcessor.EndFor(data: pointer): boolean;
var
  ds : TDataSet;
begin
  Result := true;
  ds := TDataset(data);
  if ds <> nil then begin
    ds.Next;
    Result := ds.EOF;
  end;
  //WriteLn('ENDFOR: ', Result);
end;

function TSTEProcessor.CheckTagCallbacks(const tagName, tagParam: string; var Value: string): boolean;
var
  i : integer;
begin
  Result := false;
  for i := Low(FExpandTagCallbacks) to High(FExpandTagCallbacks) do
    with FExpandTagCallbacks[i] do begin
      if tagName = Tag then begin
        Value := Proc(tagParam);
        Result := true;
        Break;
      end;
    end;
end;

function TSTEProcessor.CheckConditionalCallbacks(const ACondName : string; var Value : boolean) : boolean;
var
  i : integer;
begin
  Result := false;
  for i := Low(FConditionalCallbacks) to High(FConditionalCallbacks) do
    with FConditionalCallbacks[i] do begin
      if ACondName = ConditionName then begin
        Value := Proc(ACondName);
        Result := true;
        Break;
      end;
    end;
end;

procedure TSTEProcessor.ProcessTextToken(token: PSTEParserToken);
var
  customTagText : string;
  ln : integer;
begin
  if token^.Kind = tkPlainText then
    FOutput.Write( FTemplate.Source[token^.StartPos], token^.Size )
  else
  if token^.Kind = tkCustomTag then begin
    customTagText := ExpandTag(token^.Tag, token^.Param);
    ln := Length(customTagText);
    if ln > 0 then
      FOutput.Write( customTagText[1], ln);
  end;
end;

procedure TSTEProcessor.ProcessTokens(iEnd: integer);
var
  token : PSTEParserToken;
begin
  if iEnd > (FTemplate.Tokens.Count-1) then
    Exit;
  while (FCurrentToken <= iEnd) do begin
    token := PSTEParserToken(FTemplate.Tokens.Items[FCurrentToken]);
    //Writeln('Processing token ', FCurrentToken, '-->', STETokenTagNames[token^.Kind], ' line: ', GetSourceLineNumber(FTemplate.Source, token^.StartPos) );

    case token^.Kind of
    tkPlainText, tkCustomTag :  ProcessTextToken(token);

    tkIf :
      begin
        ProcessIfBlock(token);
        Continue;
      end;
    tkFor : ProcessForBlock(token);
    tkSet: ProcessSetBlock(token);


    end; // case
    inc(FCurrentToken);
  end;
end;

procedure TSTEProcessor.ProcessIfBlock(token: PSTEParserToken);
var
  linked: PSTEParserToken;
  hasElse : boolean;
begin
  linked := PSTEParserToken(token^.LinkedToken);
  hasElse := linked^.Kind = tkElse;
  //WriteLn('IF block, hasElse -- ', hasElse, ' -- line: ', GetSourceLineNumber(FTemplate.Source, token^.StartPos) );
  if EvaluateIf(token^.Param) then begin
    //WriteLn('IF evaluated as true');
    inc(FCurrentToken);
    ProcessTokens(linked^.Index);
    if hasElse then begin// there is an else block -- skip it
      //WriteLn('skipping else block');
      linked := PSTEParserToken(linked^.LinkedToken);
      FCurrentToken := linked^.Index; // so skip to endif
    end;
  end else begin
    if hasElse then begin // execute else block
      FCurrentToken := linked^.Index;
      linked := PSTEParserToken(linked^.LinkedToken); //endif
      ProcessTokens(linked^.Index);
    end else
      FCurrentToken := linked^.Index;
  end;
end;

procedure TSTEProcessor.ProcessForBlock(token: PSTEParserToken);
var
  linked: PSTEParserToken;
  data : pointer;
begin
  linked := PSTEParserToken(token^.LinkedToken);
  //Writeln('FOR, FCurrentToken', FCurrentToken);
  if BeginFor(token^.Param, data) then begin
    repeat
      FCurrentToken := token^.Index+1;
      ProcessTokens(linked^.Index);
    until EndFor(data);
  end;
  FCurrentToken := linked^.Index;
end;

procedure TSTEProcessor.ProcessSetBlock(token : PSTEParserToken);
var
  textToken: PSTEParserToken;
begin
  textToken := PSTEParserToken(FTemplate.Tokens.Items[ token^.Index+1 ]);
  SetValue(token^.Param, Copy(FTemplate.Source, textToken^.StartPos, textToken^.Size) );
  FCurrentToken := PSTEParserToken(token^.LinkedToken)^.Index;
end;

procedure TSTEProcessor.DisposeValues;
var
  i : integer;
begin
  for i := 0 to FValues.Count-1 do
    Dispose( pstring(FValues.Objects[i]) );
  FValues.Free;
end;

function TSTEProcessor.GetOwnsDatasets : boolean;
begin
  Result := FDatasets.OwnsObjects;
end;

procedure TSTEProcessor.SetOwnsDatasets(AValue : boolean);
begin
  FDatasets.OwnsObjects := AValue;
end;


function TSTEProcessor.CheckDSNamespaceDataset(const PrefixedDsName: string; out PureDsName: string): boolean;
begin
  Result := ( FUseDataSetPrefix and (Pos(DataSetPrefix, PrefixedDsName) = 1) ); // try if this is a dataset field
  if Result then
    PureDsName := Copy(PrefixedDsName, FDSNamespaceLength + 1, Length(PrefixedDsName)-FDSNamespaceLength);
end;

function TSTEProcessor.GetDataset(const dsName: string): TDataset;
var
  i : integer;
begin
  Result := nil;
  i := FDatasets.IndexOf(dsName);
  if i <> -1 then
    Result := TDataset(FDatasets.Objects[i]);
end;

function TSTEProcessor.GetFieldValue(const dsName, fieldName: string; out Value: string): boolean;
var
  ds : TDataSet;
  fld : TField;
begin
  Result := false;

  ds := GetDataset(dsName);
  if ds <> nil then begin
    fld := ds.FindField(fieldName);
    if fld <> nil then begin
      if fld.IsBlob then
        Value := fld.AsString // Display actual text instead of (MEMO)
      else
        Value := fld.DisplayText;
      Result := true;
    end;
  end;

end;

function TSTEProcessor.GetFieldValue(const dsName, fieldName: string; out Value: boolean): boolean;
var
  fld : TField;
  ds : TDataset;
begin
  Result := false;
  ds := GetDataset(dsName);
  if ds = nil then
    Exit;

  fld := ds.FindField( fieldName );
  if fld = nil then
    Exit;

  Value := false;
  Result := true;

  if fld.DataType in steNumericFieldTypes then
    Value := fld.Value > 0
  else
  if fld.DataType = ftBoolean then
   Value := fld.AsBoolean
  else
    Value := (fld.AsString <> '');

end;


procedure TSTEProcessor.AddDataset(const dsName: string; ds: TDataset);
begin
  FDatasets.AddObject(dsName, TObject(ds));
end;

procedure TSTEProcessor.SetValue(const key, value: string);
var
  pValue : PString;
  idx : integer;
begin
  idx := FValues.IndexOf(key);
  if idx = -1 then begin
    New(pValue);
    pValue^ := value;
    FValues.AddObject(key, TObject(pValue) );
  end else begin
    PString(FValues.Objects[idx])^ := value;
  end;
end;

procedure TSTEProcessor.SetValue(const key: string; const value: boolean);
begin
  if value then
    SetValue(key, '1' )
  else
    SetValue(key, '' );
end;

procedure TSTEProcessor.SetValue(const key : string; const value : integer);
begin
  SetValue(key, intToStr(value) );
end;

procedure TSTEProcessor.SetValue(const key : string; const value : single);
begin
  SetValue(key, floatToStr(value) );
end;

procedure TSTEProcessor.SetValue(const key : string; const value : double);
begin
  SetValue(key, floatToStr(value) );
end;

procedure TSTEProcessor.AddTagCallback(const tagName : string; ACallbackProc : TSTEExpandTagProc);
var
  i : integer;
begin
  if ACallbackProc = nil then
    raise Exception.CreateFmt('%s: AddTagCallback: unassigned tag callback', [Self.ClassName]);
  i := Length(FExpandTagCallbacks);
  SetLength(FExpandTagCallbacks, i+1);
  FExpandTagCallbacks[i].Tag := tagName;
  FExpandTagCallbacks[i].Proc := ACallbackProc;
end;

procedure TSTEProcessor.AddConditionCallback(const ACondName : string; ACallbackProc : TSTEEvaluateCondProc);
var
  i : integer;
begin
  if ACallbackProc = nil then
    raise Exception.CreateFmt('%s: AddTagCallback: unassigned tag callback', [Self.ClassName]);
  i := Length(FConditionalCallbacks);
  SetLength(FConditionalCallbacks, i+1);
  FConditionalCallbacks[i].ConditionName := ACondName;
  FConditionalCallbacks[i].Proc := ACallbackProc;
end;

procedure TSTEProcessor.Generate;
begin
  if FGenerating then Exit;
  FGenerating := true;
  try
    FUseDataSetPrefix := (DataSetPrefix <> '');
    FDSNamespaceLength := Length(DataSetPrefix);
    FCurrentToken := 0;
    ProcessTokens(FTemplate.Tokens.Count-1);
  finally
    FGenerating := false;
  end;
end;

procedure TSTEProcessor.Generate(ATemplate : TSTEParsedTemplateData; AStream : TStream);
begin
  FTemplate := ATemplate;
  FOutput := AStream;
  Generate;
end;

constructor TSTEProcessor.Create;
begin
  FGenerating := false;
  SetLength(FExpandTagCallbacks, 0);
  SetLength(FConditionalCallbacks, 0);
  FOutput := nil;
  FTemplate := nil;
  OnExpandTag := nil;
  FDatasets := TStringList.Create;
  FValues := TStringList.Create;

  UnkownTagFeedback := true;
  UnkownTagFeedbackFormat := '[{%s}]';
  DataSetPrefix := 'ds:';
end;

destructor TSTEProcessor.Destroy;
begin
  SetLength(FExpandTagCallbacks, 0);
  SetLength(FConditionalCallbacks, 0);
  FDatasets.Free;
  DisposeValues;
  inherited Destroy;
end;

end.
