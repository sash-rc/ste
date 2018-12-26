unit steCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, steParser;


// fast in-memory cache without locks, everyting prealoaded at startup (no refresh)
// intended to be used at production

type
  TSTECache = class
    protected
      FCache : TStringList;
      FParser : TSTEParser;

      FBaseDirectory : string;
      FFileNameMask : string;

      procedure Preload;
      function Prepare(const AFullFileName : string; DoCheckFileExists : boolean = true; AChangeDateTime : TDateTime = 0) : TSTEParsedTemplateData; virtual;

    public

      property BaseDirectory : string read FBaseDirectory;

      constructor Create(const ABaseDir, AFileNameMask: string); virtual;
      destructor Destroy; override;

      function Get(const AFileName : string) : TSTEParsedTemplateData; virtual;

  end;


// Refreshable and lazy-loaded (by file change date) variant
// intended to be used for development / debug

type
  TSTERefreshableCache = class(TSTECache)
    protected
      FLock : TCriticalSection;

    public
      LoadOnDemand : boolean;

      function Get(const AFileName : string) : TSTEParsedTemplateData; override;

      constructor Create(const ABaseDir, AFileNameMask: string); override;
      destructor Destroy; override;
  end;

implementation

uses
  LazFileUtils, FileUtil;

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

function TSTECache.Prepare(const AFullFileName : string; DoCheckFileExists : boolean; AChangeDateTime : TDateTime) : TSTEParsedTemplateData;
var
  content : string;
begin
  if DoCheckFileExists and ( not FileExistsUTF8(AFullFileName) ) then
    content := format('*** template not found: %s ***', [AFullFileName])
  else
    content := ReadFileToString(AFullFileName);

  Result := TSTEParsedTemplateData.Create;
  try
    FParser.PrepareTemplate(content, Result );
    if AChangeDateTime <> 0 then
      Result.ChangeDateTime := FileDateToDateTime(FileAge(AFullFileName))
  except
    FreeAndNil(Result);
    raise;
  end;
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
  FParser := TSTEParser.Create;
  FCache := TStringList.Create;
  FCache.OwnsObjects := true;

  if FFileNameMask <> '' then
    Preload;
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

function TSTERefreshableCache.Get(const AFileName : string) : TSTEParsedTemplateData;
var
  idx : integer;
  ChangedAt : TDateTime;
begin
  Result := nil;
  FLock.Enter;
  try
    // find item
    idx := FCache.IndexOf(AFileName);

    if idx = -1 then begin // -------------- miss

      if LoadOnDemand then begin
        Result := Prepare(FBaseDirectory + AFileName, true);
        FCache.AddObject(AFileName, Result);
      end;

    end else begin // -------------- hit
      Result := TSTEParsedTemplateData( FCache.Objects[idx] );
      ChangedAt := FileDateToDateTime(FileAge(FBaseDirectory + AFileName));
      if ( ChangedAt > Result.ChangeDateTime) then begin // need to refresh ???
        Result.Free;
        // create new
        Result := Prepare(FBaseDirectory + AFileName, false, ChangedAt);
        FCache.Objects[idx] := Result;
      end;
    end;

  finally
    FLock.Leave;
  end;
end;

constructor TSTERefreshableCache.Create(const ABaseDir, AFileNameMask : string);
begin
  inherited Create(ABaseDir, AFileNameMask);
  LoadOnDemand := true;
  FLock := TCriticalSection.Create;
end;

destructor TSTERefreshableCache.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;


end.

