unit steXmlPreProcessor;

{$mode objfpc}{$H+}

(*

  Preprocessor markup structure:

    {{#-03   inner-tag-with-params }}
     ^  ^  ^   ^                   ^
     |  |  |   |                   |
     |  |  |   |                   +-----> Preprocessor's TagCloseToken
     |  |  |   +-------------------------> A inner tag, usable by TSTEParser, without enclosing
     |  |  +-----------------------------> Any space char(s)
     |  +--------------------------------> LevelIndent, integer (with sign), should be valid for StrToInt()
     +-----------------------------------> Preprocessor's TagOpenToken

  LevelIndent is a (signed) number of generations (parents) you need to move tag accros xml tree.
  Positive inserts tag before (grand...grand) parent node, negative inserts after (closing part of) it.

  Example:

  <table>
    <headers>
      <cell1></cell1>
      <cell2></cell2>
      <cell3></cell3>
    </headers>

    <row>
      <cell1>[[#-2 before row]]</cell1>
      <cell2>[[#3 after table]]</cell2>
      <cell3></cell3>
    </row>

  </table>


  When moving through xml-tree, inner-tag-with-params will be enclosed by associated TSTEParser tokens.
  So pairs of enclosing tokens for TSTEXMLPreProcessor and TSTEParser should differ (at least for TagOpenToken)

  Multiple preprocesor tags within a single node is not (yet?) supported. Preprocesor tag + regualr template tags is OK.

*)


interface

uses
  Classes, SysUtils,
  laz2_DOM, laz2_XMLRead, laz2_XMLWrite,
  steParser;


type
  TSTEXMLPreProcessor = class
    private
      FTagCloseToken : string;
      FTagOpenToken : string;
      FTagOpenTokenLen, FTagCloseTokenLen : integer;

      FDoc : TXMLDocument;

      procedure SetTagCloseToken(AValue : string);
      procedure SetTagOpenToken(AValue : string);

      procedure ProcessNode(ANode : TDOMNode; ALevel : integer);
      procedure CheckNode(ANode : TDOMNode);

      function ParseTag(const ANodeSourceText : string; out AInnerTagText, ANodeText : string; out AIndentLevel : integer) : boolean;
      procedure CreateIndentedNode(ARefNode : TDOMNode; AIndentLevel : integer; const AInnerTagText : string);


    public
      // when replaced, tag will be enclosed by these two
      ParserTagOpenToken, ParserTagCloseToken : string;

      property TagOpenToken : string read FTagOpenToken write SetTagOpenToken;
      property TagCloseToken : string read FTagCloseToken write SetTagCloseToken;

      procedure Process(ASource, AResult : TStream);
      function Process(ASource : TStream) : string; // Result as string

      constructor Create(const ATagOpenToken : string = '[[#'; const ATagCloseToken : string = ']]');

  end;


  // preprocess & prepare oneliner
  function PrepareXMLTemplate(ASource : TStream) : TSTEParsedTemplateData;


implementation

uses strutils, math, steCommon{, LazLogger};

{$define DEBUG_STE_XMLPARSER}

function PrepareXMLTemplate(ASource : TStream) : TSTEParsedTemplateData;
var
  parser : TSTEParser;
  {$ifdef DEBUG_STE_XMLPARSER}
    dumpFS : TFileStream;
    dump : string;
  {$endif}
begin

  with TSTEXMLPreProcessor.Create do
    try

      parser := TSTEParser.Create;
      try
        parser.TagOpenToken := ParserTagOpenToken;
        parser.TagCloseToken := ParserTagCloseToken;

        {$ifdef DEBUG_STE_XMLPARSER}
          dump := Process(ASource);
          dumpFS := TFileStream.Create('preprocessed.xml', fmCreate);
          try
            dumpFS.Write(dump[1], Length(dump));
            Result := parser.Prepare( dump );
          finally
            dumpFS.Free;
          end;
         {$else}

         Result := parser.Prepare( Process(ASource) );

         {$endif}
      finally
        parser.Free;
      end;

    finally
      Free;
    end;
end;

procedure TSTEXMLPreProcessor.SetTagCloseToken(AValue : string);
begin
  if FTagCloseToken = AValue then Exit;
  FTagCloseToken := AValue;
  FTagCloseTokenLen := Length(FTagCloseToken);
end;

procedure TSTEXMLPreProcessor.SetTagOpenToken(AValue : string);
begin
  if FTagOpenToken = AValue then Exit;
  FTagOpenToken := AValue;
  FTagOpenTokenLen := Length(FTagOpenToken);
end;

procedure TSTEXMLPreProcessor.ProcessNode(ANode : TDOMNode; ALevel : integer);
var
  i : Integer;
begin
  CheckNode(ANode);
  with ANode.ChildNodes do
    try
      for i := 0 to (Count - 1) do begin
        ANode := Item[i];
        ProcessNode(ANode, ALevel+1);
      end;
    finally
      Free;
    end;
end;

procedure TSTEXMLPreProcessor.CheckNode(ANode : TDOMNode);
var
  innerTagText, nodeReplacementText : string;
  AIndentLevel : integer;
begin

  if ANode.NodeType = TEXT_NODE then begin

    if ParseTag(ANode.NodeValue, innerTagText, nodeReplacementText, AIndentLevel) then begin
      if AIndentLevel = 0 then
        raise ESTEParserError.CreateFmt('%s: Invalid indent token `%s`', [ Self.ClassName, ANode.NodeValue ]);

      CreateIndentedNode(ANode, AIndentLevel, innerTagText);
      ANode.NodeValue := nodeReplacementText; // replace text

    end;
  end;

end;

procedure TSTEXMLPreProcessor.CreateIndentedNode(ARefNode : TDOMNode; AIndentLevel : integer; const AInnerTagText : string);
var
  i, absLvl : integer;
  AIndentNode, ANewNode : TDOMNode;
begin

  absLvl := abs(AIndentLevel);

  AIndentNode := ARefNode;
  for i := 1 to absLvl do begin
    AIndentNode := AIndentNode.ParentNode;
    if AIndentNode = nil then
      raise ESTEParserError.CreateFmt('%s: Invalid indent level %d (no parent) of %d in `%s`', [ Self.ClassName, i, AIndentLevel, ARefNode.NodeValue ]);
  end;

  ANewNode := FDoc.CreateTextNode( ParserTagOpenToken + AInnerTagText + ParserTagCloseToken);
  //ANewNode := FDoc.CreateComment( ParserTagOpenToken + AInnerTagText + ParserTagCloseToken);

  if Sign(AIndentLevel) < 0 then begin
    // before parent
    AIndentNode.ParentNode.InsertBefore(ANewNode, AIndentNode);
  end else
  if Sign(AIndentLevel) > 0 then begin

    if AIndentNode.NextSibling <> nil then begin// before next sibling
      AIndentNode.ParentNode.InsertBefore(ANewNode, AIndentNode.NextSibling);
    end else // append (last) child
      AIndentNode.ParentNode.AppendChild(ANewNode);
  end;

end;

function TSTEXMLPreProcessor.ParseTag(const ANodeSourceText : string; out AInnerTagText, ANodeText : string; out AIndentLevel : integer) : boolean;
var
  i, openTagPos, closeTagPos : integer;
  AIndentText, tagText : string;
begin
  Result := false;

  openTagPos := Pos(TagOpenToken, ANodeSourceText);
  if openTagPos > 0 then begin
    i := openTagPos + FTagOpenTokenLen;
    closeTagPos := PosEx(TagCloseToken, ANodeSourceText, i);
    if closeTagPos > 0 then begin
      // Inner tag text
      tagText := Trim( Copy(ANodeSourceText, i, closeTagPos - i) );

      // copy NodeText without TagText
      ANodeText := Copy(ANodeSourceText, 1, openTagPos-1);
      i := Length(ANodeSourceText);
      ANodeText += Copy(ANodeSourceText, closeTagPos + FTagCloseTokenLen, i - (closeTagPos + FTagCloseTokenLen) + 1 );

      // Indent
      AIndentText := ExtractWordPos(1, tagText, steWhiteSpaceChars, i);
      AIndentLevel := StrToIntDef(AIndentText, 0);

      i += Length(AIndentText);
      AInnerTagText := Trim( Copy(tagText, i, Length(tagText)-i+1 ) );
      Result := true;
    end;
  end;
end;

procedure TSTEXMLPreProcessor.Process(ASource, AResult : TStream);
var
  ANode : TDOMNode;
begin
  ReadXMLFile(FDoc, ASource);
  try

    ANode := FDoc.DocumentElement{.FirstChild};
    while Assigned(ANode) do begin
      ProcessNode(ANode, 0);
      ANode := ANode.NextSibling;
    end;

    // write results
    WriteXML(FDoc, AResult);
  finally
    FreeAndNil(FDoc);
  end;

end;

function TSTEXMLPreProcessor.Process(ASource : TStream) : string;
var
  ms : TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    Process(ASource, ms);
    ms.Position := 0;
    SetLength(Result, ms.Size);
    ms.Read(Result[1], ms.Size);
  finally
    ms.Free;
  end;
end;

constructor TSTEXMLPreProcessor.Create(const ATagOpenToken : string; const ATagCloseToken : string);
begin
  // set defaults
  TagOpenToken := ATagOpenToken;
  TagCloseToken := ATagCloseToken;

  ParserTagOpenToken := '{{';
  ParserTagCloseToken := '}}';
end;

end.


