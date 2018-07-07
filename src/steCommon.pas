unit steCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;



type
  ESTEParserError = class(Exception);

const
    steWhiteSpaceChars = [ #0..' ' ];
    steSCR = #13;
    steSLF = #10;


function STEGetSourceLineNumber(const source : string; srcPos: integer): integer;

implementation

function STEGetSourceLineNumber(const source : string; srcPos: integer): integer;
var
  i, maxPos : integer;
  lnEnd : char;
begin
  lnEnd := char(string(LineEnding)[1]);

  Result := 1;
  maxPos := Length( source );
  if srcPos > maxPos then
    Exit
  else
    maxPos := srcPos;
  for i := 1 to maxPos do begin
    if (char(source[i]) = lnEnd) then begin
      inc(Result);
    end;
  end;
end;


end.

