unit steParser;

//
// Simple Templating Engine for FreePascal
//

{$mode objfpc}{$H+}


// load, tokenize and check template

interface

uses
  Classes, SysUtils;

// Tokens are both plain text and tags.

type
  TSTEParserTokenKind = (tkPlainText, tkCustomTag, tkFor, tkEndFor, tkIf, tkEndIf, tkElse, tkSet, tkEndSet);

type
  TSTEParserToken  = record
    Kind        : TSTEParserTokenKind;
    Tag         : string;
    Param       : string;
    StartPos    : integer;
    Size        : integer;
    LinkedToken : pointer; //for<->endfor; if->(endif/else); else->endif; endif->if; set<->endset
    Index       : integer;
  end;
  PSTEParserToken = ^TSTEParserToken;

type
  TSTEParsedTemplateData = class
  protected
    function NewToken(AStartPos : integer; AKind : TSTEParserTokenKind = tkPlainText) : PSTEParserToken;
  public
    Source : string;
    Tokens : TList;

    function TokenKindName(tokenIndex : integer) : string;
    constructor Create; virtual;
    destructor Destroy; override;
  end;


type
  TSTEParser = class
  private
    FSourceLen : integer;
    FCurrentPos : integer;
    FTagCloseToken : string;
    FTagOpenToken : string;
    FTagOpenTokenLen, FTagCloseTokenLen : integer;

    procedure SetTagCloseToken(AValue : string);
    procedure SetTagOpenToken(AValue : string);

  protected
    class var DefaultTagOpenToken : string;
    class var DefaultTagCloseToken : string;

    procedure AddTextToken(template : TSTEParsedTemplateData; aStart, aSize : integer);
    function GetNextToken(template : TSTEParsedTemplateData) : boolean;
    procedure ParseToken(token : PSTEParserToken; const tokenText : string);
    procedure BuildSyntaxTree(template : TSTEParsedTemplateData);
  public
    //TagParamSeparator : string;
    TrimTagLines : boolean; //trim empty line endings after tags - postponed

    property TagOpenToken : string read FTagOpenToken write SetTagOpenToken;
    property TagCloseToken : string read FTagCloseToken write SetTagCloseToken;


    class procedure SetDefaultTokenEnclosing(const AOpen, AClose : string);

    function Prepare(const ASource : string) : TSTEParsedTemplateData;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  strutils, contnrs, steCommon;

const
  STETokenTagNames : array[TSTEParserTokenKind] of string =
    ('text', 'custom', 'for', 'endfor', 'if', 'endif', 'else', 'set', 'endset');


function TSTEParsedTemplateData.NewToken(AStartPos: integer; AKind: TSTEParserTokenKind): PSTEParserToken;
begin
  New(Result);
  with Result^ do begin
    Kind := AKind;
    Tag := '';
    Param := '';
    StartPos := AStartPos;
    Size := -1;
    LinkedToken := nil;
    Index := -1;
  end;
  Tokens.Add(Result);
end;

function TSTEParsedTemplateData.TokenKindName(tokenIndex: integer): string;
begin
  if (tokenIndex > -1) and (tokenIndex < Tokens.Count) then
    Result := STETokenTagNames[ PSTEParserToken(Tokens.Items[tokenIndex])^.Kind ]
  else
    Result := 'invalid token index';
end;

constructor TSTEParsedTemplateData.Create;
begin
  Tokens := TList.Create;
end;

destructor TSTEParsedTemplateData.Destroy;
var
  i: integer;
begin
  for i := 0 to Tokens.Count-1 do
    Dispose(PSTEParserToken(Tokens.Items[i]));
  Tokens.Free;
  inherited Destroy;
end;

procedure TSTEParser.SetTagOpenToken(AValue : string);
begin
  if FTagOpenToken = AValue then Exit;
  FTagOpenToken := AValue;
  FTagOpenTokenLen := Length(FTagOpenToken);
end;

procedure TSTEParser.SetTagCloseToken(AValue : string);
begin
  if FTagCloseToken = AValue then Exit;
  FTagCloseToken := AValue;
  FTagCloseTokenLen := Length(FTagCloseToken);
end;

procedure TSTEParser.AddTextToken(template: TSTEParsedTemplateData; aStart, aSize: integer);
begin
  with template.NewToken(aStart, tkPlainText)^ do begin
    Size := aSize;
    Tag := '';
    Param := '';
  end;
end;

function TSTEParser.GetNextToken(template: TSTEParsedTemplateData): boolean;
var
  iStart, openTagPos, closeTagPos : integer;
  tokenText : string;
  token : PSTEParserToken;
begin
  Result := false;
  iStart := FCurrentPos;
  openTagPos := PosEx(TagOpenToken, template.Source, FCurrentPos);
  if openTagPos > 0 then begin // found tag opening
    FCurrentPos := openTagPos + FTagOpenTokenLen-1;  // ------ ???? todo: recheck indicies
    // search for tag closing
    closeTagPos := PosEx(TagCloseToken, template.Source, FCurrentPos);
    if closeTagPos > 0 then begin
      tokenText := Copy(template.Source, FCurrentPos+1, closeTagPos - FCurrentPos-1);
      FCurrentPos := closeTagPos + FTagCloseTokenLen;
      Result := true;
      // Some text from start - make it text token
      if openTagPos > iStart then
        AddTextToken(template, iStart, openTagPos-iStart);

      // ---- Append new tag token
      token := template.NewToken(openTagPos, tkCustomTag);
      ParseToken(token, Trim(tokenText));

      {
      if TrimTagLines then
        if not ( token^.Kind in [tkPlainText, tkCustomTag] ) then
          TrimLine(template);}

    end;
  end;

  if not Result then begin // no more tags until end of file
    AddTextToken(template, iStart, FSourceLen-FCurrentPos+1);
  end;

end;

procedure TSTEParser.ParseToken(token: PSTEParserToken; const tokenText: string);
var
  spos : integer = 0;
  s : string;
  iKind : TSTEParserTokenKind;
begin
  with token^ do begin
    if Kind <> tkPlainText then begin

      s := ExtractWordPos(1, tokenText, steWhiteSpaceChars , spos);
      if spos > 0 then begin
        Tag := s;
        Param := Trim(Copy(tokenText, spos + Length(Tag), Length(tokenText)-spos));
      end else begin
        Tag := tokenText;
        Param := '';
      end;
      Kind := tkCustomTag;
      s := LowerCase(Tag);

      for iKind := tkFor to tkEndSet do begin // ------------ check supported tags
        if s = STETokenTagNames[iKind] then begin
          Kind := iKind;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TSTEParser.BuildSyntaxTree(template: TSTEParsedTemplateData);
var
  i: integer;
  linked, token, elseToken : PSTEParserToken;
  setTokens, forTokens, ifTokens : TStack;
begin
  setTokens := TStack.Create;
  forTokens := TStack.Create;
  ifTokens := TStack.Create;
  try
    for i := 0 to template.Tokens.Count-1 do begin
      token := PSTEParserToken(template.Tokens.Items[i]);
      token^.Index := i;
      case token^.Kind of
      tkFor :
        begin
          if Trim(token^.Param) = '' then
            raise ESTEParserError.CreateFmt('FOR without parameter, line %d',
              [ STEGetSourceLineNumber(template.Source, token^.StartPos) ]);
          forTokens.Push(token);
        end;

      tkSet :
        begin
          if Trim(token^.Param) = '' then
            raise ESTEParserError.CreateFmt('SET without parameter, line %d',
              [ STEGetSourceLineNumber(template.Source, token^.StartPos) ]);
          setTokens.Push(token);
        end;

      tkIf :
        begin
          if Trim(token^.Param) = '' then
            raise ESTEParserError.CreateFmt('IF without parameter, line %d',
              [ STEGetSourceLineNumber(template.Source, token^.StartPos) ]);
          ifTokens.Push(token);
        end;

      tkElse :
        begin
          if ifTokens.Count > 0 then begin
            linked := PSTEParserToken(ifTokens.Peek);
            if linked^.LinkedToken <> nil then
              raise ESTEParserError.CreateFmt('ELSE mismatch, line %d',
                [ STEGetSourceLineNumber(template.Source, token^.StartPos) ]);
            linked^.LinkedToken := token; // if->else
          end else
            raise ESTEParserError.CreateFmt('ELSE mismatch, line %d',
              [ STEGetSourceLineNumber(template.Source, token^.StartPos) ]);
        end;

      tkEndIf : // if->(endif/else); else->endif; endif->if
        begin
          if ifTokens.Count > 0 then begin
            linked := PSTEParserToken(ifTokens.Pop);
            if linked^.LinkedToken = nil then // no else
              linked^.LinkedToken := token
            else begin // linked^.LinkedToken is else tag
              elseToken := PSTEParserToken(linked^.LinkedToken);
              if elseToken^.Kind <> tkElse then // check it
                raise ESTEParserError.CreateFmt('ELSE mismatch, line %d',
                  [ STEGetSourceLineNumber(template.Source, token^.StartPos) ]);
              elseToken^.LinkedToken := token; //else->endif;
              token^.LinkedToken := linked; // endif->if
            end;
          end else
            raise ESTEParserError.CreateFmt('ENDIF mismatch, line %d',
              [ STEGetSourceLineNumber(template.Source, token^.StartPos) ]);
        end;

      tkEndFor :
        begin
          if forTokens.Count > 0 then begin
            linked := PSTEParserToken(forTokens.Pop);
            linked^.LinkedToken := token;
            token^.LinkedToken := linked;
          end else
            raise ESTEParserError.CreateFmt('ENDFOR mismatch, line %d',
              [ STEGetSourceLineNumber(template.Source, token^.StartPos) ]);
        end;

      tkEndSet :
        begin
          if setTokens.Count > 0 then begin
            linked := PSTEParserToken(setTokens.Pop);
            linked^.LinkedToken := token;
            token^.LinkedToken := linked;
            if token^.Index <> (linked^.Index + 2) then
              raise ESTEParserError.CreateFmt('SET block: only single text token allowed, line %d',
                [ STEGetSourceLineNumber(template.Source, token^.StartPos) ]);
            linked := PSTEParserToken( template.Tokens.Items[ linked^.Index + 1 ] ); // set to contained text
            if linked^.Kind <> tkPlainText then
              raise ESTEParserError.CreateFmt('SET block: must not contain other tokens, line %d',
                [ STEGetSourceLineNumber(template.Source, token^.StartPos) ]);
          end else
            raise ESTEParserError.CreateFmt('ENDSET mismatch, line %d',
              [ STEGetSourceLineNumber(template.Source, token^.StartPos) ]);
        end;

      end; // case
    end; // for
    // Check for unpaired tags
    if forTokens.Count > 0 then
      raise ESTEParserError.CreateFmt('ENDFOR mismatch, line %d',
        [ STEGetSourceLineNumber(template.Source, PSTEParserToken(forTokens.Peek)^.StartPos) ]);
    if ifTokens.Count > 0 then
      raise ESTEParserError.CreateFmt('ENDIF mismatch, line %d',
        [ STEGetSourceLineNumber(template.Source, PSTEParserToken(ifTokens.Peek)^.StartPos) ]);
    if setTokens.Count > 0 then
      raise ESTEParserError.CreateFmt('ENDSET mismatch, line %d',
        [ STEGetSourceLineNumber(template.Source, PSTEParserToken(setTokens.Peek)^.StartPos) ]);

    // todo: add second pass to check correct LinkedToken references

  finally
    setTokens.Free;
    ifTokens.Free;
    forTokens.Free;
  end;
end;

class procedure TSTEParser.SetDefaultTokenEnclosing(const AOpen, AClose : string);
begin
  DefaultTagOpenToken := AOpen;
  DefaultTagCloseToken := AClose;
end;

function TSTEParser.Prepare(const ASource : string) : TSTEParsedTemplateData;
var
  moreTokens : boolean;
begin
  try
    Result := TSTEParsedTemplateData.Create;
    Result.Source := ASource;
    FSourceLen := Length(ASource);
    FCurrentPos := 1;
    // tokenize
    repeat
      moreTokens := GetNextToken(Result);
    until moreTokens = false;
    BuildSyntaxTree(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

constructor TSTEParser.Create;
begin
  TagOpenToken := DefaultTagOpenToken;
  TagCloseToken := DefaultTagCloseToken;
  TrimTagLines := true;
end;

destructor TSTEParser.Destroy;
begin
  inherited Destroy;
end;


initialization
  TSTEParser.SetDefaultTokenEnclosing('<?', '?>'); // defaults



end.

