unit steCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, steParser;


// faster cache without locks, everyting prealoaded at startup
// intended to be used at production

type
  TSTECache = class
    protected
      FCache : TStringList;
      FParser : TSTEParser;
      FTemplateClass : TSTETemplateClass;

      FBaseDirectory : string;
      FFileNameMask : string;

      procedure Preload;
      function Prepare(const AFullFileName : string; DoCheckFileExists : boolean = true) : TSTEParsedTemplateData; virtual;

      procedure AfterConstruction; override;

    public

      property BaseDirectory : string read FBaseDirectory;

      constructor Create(const ABaseDir, AFileNameMask: string); virtual;
      destructor Destroy; override;

      function Get(const AFileName : string) : TSTEParsedTemplateData; virtual;
      procedure Release({%H-}ATemplate : TSTEParsedTemplateData); virtual;

  end;


// Lazy loaded and refreshable (by file change date)
// intended to be used for development / debug

type
  TSTERefreshableCache = class(TSTECache)
    protected
      FLock : TCriticalSection;
      FGarbageItems : TFPList;

      procedure ClearGarbage;
      function Prepare(const AFullFileName : string; DoCheckFileExists : boolean = true) : TSTEParsedTemplateData; override;

    public
      LoadOnDemand : boolean;

      function Get(const AFileName : string) : TSTEParsedTemplateData; override;
      procedure Release(ATemplate : TSTEParsedTemplateData); override;

      constructor Create(const ABaseDir, AFileNameMask: string); override;
      destructor Destroy; override;
  end;

implementation

uses
  LazFileUtils, FileUtil;

type
  TSTESharedTemplate = class(TSTEParsedTemplateData)
    protected
      FReadersCount : integer;
      FLastChangeDate : TDateTime;
      FIsGarbage : boolean;
    public
      constructor Create; override;
  end;

procedure TSTECache.Preload;
var
  files : TStringList;
  i : integer;
  relPath : string;
begin
  files := FindAllFiles(FBaseDirectory, FFileNameMask);
  try

    for i := 0 to files.Count-1 do begin
      relPath := LazFileUtils.CreateRelativePath(files.Strings[i], FBaseDirectory);
      FCache.AddObject(relPath, Prepare(files.Strings[i], true) );
    end;

  finally
    //files.SaveToFile('full-path-list.txt');
    //FCache.SaveToFile('cache-list.txt');
    files.Free;
  end;
end;

function TSTECache.Prepare(const AFullFileName : string; DoCheckFileExists : boolean) : TSTEParsedTemplateData;
var
  content : string;
begin
  if DoCheckFileExists and ( not FileExistsUTF8(AFullFileName) ) then
    content := format('*** template not found: %s ***', [AFullFileName])
  else
    content := ReadFileToString(AFullFileName);

  Result := FTemplateClass.Create;
  try
    FParser.PrepareTemplate(content, Result );
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TSTECache.AfterConstruction;
begin
  if FFileNameMask <> '' then
    Preload;
end;

constructor TSTECache.Create(const ABaseDir, AFileNameMask : string);
begin
  FBaseDirectory := ABaseDir;
  if not FilenameIsAbsolute(FBaseDirectory) then
    FBaseDirectory := ExpandFileNameUTF8
    (
      FBaseDirectory,
      IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)))
    );
  FBaseDirectory := IncludeTrailingPathDelimiter( FBaseDirectory );

  FFileNameMask := AFileNameMask;
  FTemplateClass := TSTEParsedTemplateData;
  FParser := TSTEParser.Create;
  FCache := TStringList.Create;
  FCache.OwnsObjects := true;
end;

destructor TSTECache.Destroy;
begin
  FCache.Free;
  FParser.Free;
  inherited Destroy;
end;

function TSTECache.Get(const AFileName : string) : TSTEParsedTemplateData;
var
  idx : integer;
begin
  Result := nil;
  idx := FCache.IndexOf(AFileName);
  if idx <> -1 then
    Result := TSTEParsedTemplateData(FCache.Objects[idx]);
end;

procedure TSTECache.Release(ATemplate : TSTEParsedTemplateData);
begin
  // do nothing
end;

constructor TSTESharedTemplate.Create;
begin
  inherited Create;
  FReadersCount := 0;
  FIsGarbage := false;
end;

function TSTERefreshableCache.Prepare(const AFullFileName : string; DoCheckFileExists : boolean) : TSTEParsedTemplateData;
begin
  Result := inherited Prepare(AFullFileName, DoCheckFileExists);
  TSTESharedTemplate(Result).FLastChangeDate := FileDateToDateTime( FileAge(AFullFileName) );
end;

function TSTERefreshableCache.Get(const AFileName : string) : TSTEParsedTemplateData;
var
  idx : integer;
  tpl : TSTESharedTemplate;
  ChangedAt : TDateTime;
begin
  Result := nil;
  FLock.Enter;
  try
    // find item
    idx := FCache.IndexOf(AFileName);

    if idx = -1 then begin // -------------- miss
      if LoadOnDemand then begin
        tpl := TSTESharedTemplate( Prepare( FBaseDirectory + AFileName) );
        FCache.AddObject(AFileName, tpl);
      end;

    end else begin // -------------- hit
      tpl := TSTESharedTemplate( FCache.Objects[idx] );
      ChangedAt := FileDateToDateTime(FileAge(FBaseDirectory + AFileName));
      if ( ChangedAt > tpl.FLastChangeDate) then begin // need to refresh ???
        // move old version to garabge
        tpl.FIsGarbage := true;
        FGarbageItems.Add(tpl);

        // create new
        tpl := nil;
        tpl := TSTESharedTemplate( Prepare( FBaseDirectory + AFileName) );
        FCache.Objects[idx] := tpl;
      end;
    end;

    if tpl <> nil then
      inc(tpl.FReadersCount);

    Result := tpl;
  finally
    FLock.Leave;
  end;
end;

procedure TSTERefreshableCache.ClearGarbage;
var
  i : Integer;
begin
  FLock.Enter;
  try
    for i := 0 to FGarbageItems.Count-1 do
      TSTESharedTemplate(FCache.Objects[i]).Free;
    FGarbageItems.Clear;
  finally
    FLock.Leave;
  end;
end;

procedure TSTERefreshableCache.Release(ATemplate : TSTEParsedTemplateData);
var
  tpl : TSTESharedTemplate;
begin
  tpl := TSTESharedTemplate(ATemplate);
  FLock.Enter;
  try
    dec(tpl.FReadersCount);
    if (tpl.FReadersCount < 1) and tpl.FIsGarbage then begin
      FGarbageItems.Remove(tpl);
      tpl.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

constructor TSTERefreshableCache.Create(const ABaseDir, AFileNameMask : string);
begin
  inherited Create(ABaseDir, AFileNameMask);
  FTemplateClass := TSTESharedTemplate;
  LoadOnDemand := true;
  FLock := TCriticalSection.Create;
  FGarbageItems := TFPList.Create;
end;

destructor TSTERefreshableCache.Destroy;
begin
  ClearGarbage;
  FGarbageItems.Free;
  FLock.Free;
  inherited Destroy;
end;

end.

