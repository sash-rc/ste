unit steCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$ifndef STE_CACHE_NO_THREADS}
  syncobjs,
  {$endif}
  steParser;

type
  TSTECachePolicy = (stecpPreload, stecpLazy);

type
  TSTESharedTemplate = class(TSTEParsedTemplateData)
    protected
      FReadersCount : integer;
    public
      procedure Aquire;
      procedure Release;
      constructor Create; override;
  end;


type
  TSTECachedItem = record
    Name : string;
    Template : TSTESharedTemplate;
    LastChangeDate : TDateTime;
    //{$ifndef STE_CACHE_NO_THREADS}
    //ReadersCount : integer;
    //{$endif}
  end;


type
  TSTECache = class
    protected
      {$ifndef STE_CACHE_NO_THREADS}
      FLock : TMultiReadExclusiveWriteSynchronizer;
      {$endif}
      FCache : array of TSTECachedItem;
      FParser : TSTEParser;

      function InternalGet(const AnItemName : string) : TSTESharedTemplate;
      function Prepare(const AFileName : string) : TSTEParsedTemplateData;

    public
      BaseDirectory : string;
      FileNameMask : string;
      Refreshable : boolean;

      procedure Clear;
      function Get(const AnItemName : string) : TSTEParsedTemplateData;

      constructor Create;
      destructor Destroy; override;
  end;

implementation

uses
  LazFileUtils, FileUtil;

procedure TSTESharedTemplate.Aquire;
begin
  inc(FReadersCount);
end;

procedure TSTESharedTemplate.Release;
begin
  dec(FReadersCount);
end;

constructor TSTESharedTemplate.Create;
begin
  inherited Create;
  FReadersCount := 0;
end;

function TSTECache.InternalGet(const AnItemName : string) : TSTESharedTemplate;
var
  maxIdx, i: Integer;
begin
  Result := nil;

  // find item
  maxIdx := Length(FCache)-1;
  for i := 0 to maxIdx do begin
    if SameText( FCache[i].Name, AnItemName ) then begin // ------ hit
      Result := FCache[i].Template;

      if Refreshable then begin





      end;
      Result.Aquire;
      Exit;
    end;
  end;

  // cache miss


end;

function TSTECache.Prepare(const AFileName : string) : TSTEParsedTemplateData;
var
  content : string;
begin
  if FileExistsUTF8(AFileName) then
    content := ReadFileToString(AFileName)
  else
    content := format('*** template not found: %s ***', [AFileName]);
  Result := FParser.Prepare( content );
end;

procedure TSTECache.Clear;
var
  item : TSTECachedItem;
begin
  {$ifndef STE_CACHE_NO_THREADS}
  FLock.BeginWrite;
  try
  {$endif}

    for item in FCache do
      item.Template.Free;
    SetLength(FCache, 0);

  {$ifndef STE_CACHE_NO_THREADS}
  finally
    FLock.EndWrite;
  end;
  {$endif}
end;

function TSTECache.Get(const AnItemName : string) : TSTEParsedTemplateData;
begin
  Result := nil;

  {$ifndef STE_CACHE_NO_THREADS}
  FLock.BeginRead;
  try
  {$endif}
    Result := InternalGet(AnItemName);
  {$ifndef STE_CACHE_NO_THREADS}
  finally
    FLock.EndRead;
  end;
  {$endif}
end;

constructor TSTECache.Create;
begin
  Refreshable := false;
  FParser := TSTEParser.Create;
  {$ifndef STE_CACHE_NO_THREADS}
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
  {$endif}
end;

destructor TSTECache.Destroy;
begin
  Clear;
  {$ifndef STE_CACHE_NO_THREADS}
  FParser.Free;
  FLock.Free;
  {$endif}
  inherited Destroy;
end;

end.

