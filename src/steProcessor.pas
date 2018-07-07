unit steProcessor;

//
// Simple Templating Engine for FreePascal
//

{$mode objfpc}{$H+}

// generate output with previously prepared template

interface

uses
  Classes, SysUtils, steParser, contnrs, db;

type
  TSTEExpandTagProc = function (const tagParam: string): string of object;

  TSPTEExpandTagCallbackInfo = record
    Tag : string;
    Proc : TSTEExpandTagProc;
  end;


type
  TSTEProcessor = class
  private
    FOutput : TStream;
    FCurrentToken : integer;
    FGenerating : boolean; // for handle recursion
    FUseDSNamespace : boolean; // dataset-namespace prefixed name
    FDSNamespaceLength : integer;
    procedure CheckOutput;
    procedure DisposeValues;
    function GetOwnsDatasets : boolean;
    procedure SetOwnsDatasets(AValue : boolean);

  protected
    FDatasets : TStringList;
    FValues : TStringList; //todo: replace with TFPStringHashTable

    FExpandTagCallbacks : array of TSPTEExpandTagCallbackInfo;

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
  public
    DSNamespace : string; // namespace (prefix) for datasets
    Template : TSTEParsedTemplateData;

    // callback should return false, if tag not handled
    OnExpandTag : function (const tagName, tagParam: string; out Value : string) : boolean of object;

    UnkownTagFeedback : boolean;  // When true (default) all unknown tags echoed to output using UnkownTagFeedbackFormat
                                  // You may want to disable this feature for production
    UnkownTagFeedbackFormat : string;

    property OwnsDatasets : boolean read GetOwnsDatasets write SetOwnsDatasets;

    procedure SetOutput(AStream : TStream);

    procedure AddDataset(const dsName : string; ds : TDataset);


    //===== todo ? : should be values stored as variant ?

    procedure SetValue(const key, value : string);
    procedure SetValue(const key : string; const value : boolean);
    procedure SetValue(const key : string; const value : integer);
    procedure SetValue(const key : string; const value : single);

    procedure AddTagCallback(const tagName : string; ACallbackProc : TSTEExpandTagProc);

    // generate and parse internally
    procedure Generate(const ASource : string); // to Output Stream
    function GenerateToString(const ASource : string) : string; // as Result string

    // from external prepared template
    procedure Generate; // to Output Stream
    function GenerateToString : string;

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
  ps : TSysCharSet = [' '];
begin
  Result := false;
  i := FValues.IndexOf(Param);
  if i <> -1 then begin
    Result := ( PString(FValues.Objects[i])^ <> '' );
  end else begin //------- try dataset field
    fldName := ExtractWord(2, Param, ps);
    if fldName = '' then
      Exit;

    if CheckDSNamespaceDataset(ExtractWord(1, Param, ps), dsName) then begin
      Result := GetFieldValue(dsName, fldName, Value);
      if Result then
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

procedure TSTEProcessor.CheckOutput;
begin
  if (FOutput = nil) then
    raise Exception.CreateFmt('%s: unassigned Output stream', [Self.ClassName]);
end;

procedure TSTEProcessor.ProcessTextToken(token: PSTEParserToken);
var
  customTagText : string;
  ln : integer;
begin
  if token^.Kind = tkPlainText then
    FOutput.Write( Template.Source[token^.StartPos], token^.Size )
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
  if iEnd > (Template.Tokens.Count-1) then
    Exit;
  while (FCurrentToken <= iEnd) do begin
    token := PSTEParserToken(Template.Tokens.Items[FCurrentToken]);
    //Writeln('Processing token ', FCurrentToken, '-->', STETokenTagNames[token^.Kind], ' line: ', GetSourceLineNumber(Template.Source, token^.StartPos) );

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
  //WriteLn('IF block, hasElse -- ', hasElse, ' -- line: ', GetSourceLineNumber(Template.Source, token^.StartPos) );
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
  textToken := PSTEParserToken(Template.Tokens.Items[ token^.Index+1 ]);
  SetValue(token^.Param, Copy(Template.Source, textToken^.StartPos, textToken^.Size) );
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
  Result := ( FUseDSNamespace and (Pos(DSNamespace, PrefixedDsName) = 1) ); // try if this is a dataset field
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

procedure TSTEProcessor.SetOutput(AStream : TStream);
begin
  FOutput := AStream;
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

procedure TSTEProcessor.Generate(const ASource : string);
var
  tp : TSTEParser;
  tpl : TSTEParsedTemplateData;
begin
  CheckOutput;
  tp := TSTEParser.Create;
  try
    tpl := tp.Prepare(ASource);
    try
      Template := tpl;
      Generate;
    finally
      Template := nil;
      tpl.Free;
    end;
  finally
    tp.Free;
  end;
end;

procedure TSTEProcessor.Generate;
begin
  if FGenerating then Exit;
  FGenerating := true;
  try
    if Template = nil then
      raise Exception.CreateFmt('%s: unassigned template', [Self.ClassName]);

    CheckOutput;

    FUseDSNamespace := (DSNamespace <> '');
    FDSNamespaceLength := Length(DSNamespace);

    FCurrentToken := 0;
    ProcessTokens(Template.Tokens.Count-1);
  finally
    FGenerating := false;
  end;
end;

function TSTEProcessor.GenerateToString(const ASource : string) : string;
var
  ms: TMemoryStream;
  tp : TSTEParser;
  tpl : TSTEParsedTemplateData;
begin
  tp := TSTEParser.Create;
  try
    tpl := tp.Prepare(ASource);
    try
      Template := tpl;

      ms := TMemoryStream.Create;
      try
        SetOutput(ms);
        Generate;
        ms.Position := 0;
        SetLength(Result, ms.Size);
        ms.Read(Result[1], ms.Size);
      finally
        ms.Free;
      end;

    finally
      Template := nil;
      tpl.Free;
    end;
  finally
    tp.Free;
  end;
end;

function TSTEProcessor.GenerateToString: string;
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    SetOutput(ms);
    Generate;
    ms.Position := 0;
    SetLength(Result, ms.Size);
    ms.Read(Result[1], ms.Size);
  finally
    ms.Free;
  end;
end;

constructor TSTEProcessor.Create;
begin
  FGenerating := false;
  SetLength(FExpandTagCallbacks, 0);
  FOutput := nil;
  Template := nil;
  OnExpandTag := nil;
  FDatasets := TStringList.Create;
  FValues := TStringList.Create;

  UnkownTagFeedback := true;
  UnkownTagFeedbackFormat := '[{%s}]';
  DSNamespace := 'ds:';
end;

destructor TSTEProcessor.Destroy;
begin
  SetLength(FExpandTagCallbacks, 0);
  FDatasets.Free;
  DisposeValues;
  inherited Destroy;
end;

end.
