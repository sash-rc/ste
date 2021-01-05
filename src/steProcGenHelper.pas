unit steProcGenHelper;

{$mode objfpc}{$H+}

interface

// I/O Helper for output generation

uses
  Classes, SysUtils, steProcessor;

type
  TSTEProcGenHelper = class helper for TSTEProcessor
    function GenerateToString : string;
    procedure GenerateToFile(const AFileName : string);

    // generate and parse internally
    procedure GenerateFromString(const ASource : string); // to Output
  end;

implementation

uses
  steParser;


function TSTEProcGenHelper.GenerateToString : string;
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    FOutput := ms;
    Generate;
    ms.Position := 0;
    SetLength(Result, ms.Size);
    ms.Read(Result[1], ms.Size);
  finally
    ms.Free;
  end;
end;

procedure TSTEProcGenHelper.GenerateToFile(const AFileName : string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    FOutput := fs;
    Generate;
  finally
    fs.Free;
  end;
end;

procedure TSTEProcGenHelper.GenerateFromString(const ASource : string);
var
  tp : TSTEParser;
  tpl : TSTEParsedTemplateData;
begin
  tp := TSTEParser.Create;
  try
    tpl := tp.Prepare(ASource);
    try
      FTemplate := tpl;
      Generate;
    finally
      FTemplate := nil;
      tpl.Free;
    end;
  finally
    tp.Free;
  end;
end;


end.

