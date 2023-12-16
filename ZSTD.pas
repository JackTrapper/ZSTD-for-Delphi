unit ZSTD;

interface

uses
	Windows, Classes,
	ZSTDLib;

type
	TZSTDCompressOptions = record
		CompressionLevel: Integer; (* Update all compression parameters according to pre-defined cLevel table
										* Default level is ZSTD_CLEVEL_DEFAULT==3.
										* Special: value 0 means default, which is controlled by ZSTD_CLEVEL_DEFAULT.
										* Note 1 : it's possible to pass a negative compression level.
										* Note 2 : setting a level sets all default values of other compression parameters *)
		WindowLog: Integer;      (* Maximum allowed back-reference distance, expressed as power of 2.
										* Must be clamped between ZSTD_WINDOWLOG_MIN and ZSTD_WINDOWLOG_MAX.
										* Special: value 0 means "use default windowLog".
										* Note: Using a windowLog greater than ZSTD_WINDOWLOG_LIMIT_DEFAULT
										*       requires explicitly allowing such window size at decompression stage if using streaming. *)
		HashLog: Integer;        (* Size of the initial probe table, as a power of 2.
										* Resulting memory usage is (1 << (hashLog+2)).
										* Must be clamped between ZSTD_HASHLOG_MIN and ZSTD_HASHLOG_MAX.
										* Larger tables improve compression ratio of strategies <= dFast,
										* and improve speed of strategies > dFast.
										* Special: value 0 means "use default hashLog". *)
		ChainLog: Integer;       (* Size of the multi-probe search table, as a power of 2.
										* Resulting memory usage is (1 << (chainLog+2)).
										* Must be clamped between ZSTD_CHAINLOG_MIN and ZSTD_CHAINLOG_MAX.
										* Larger tables result in better and slower compression.
										* This parameter is useless when using "fast" strategy.
										* It's still useful when using "dfast" strategy,
										* in which case it defines a secondary probe table.
										* Special: value 0 means "use default chainLog". *)
		SearchLog: Integer;      (* Number of search attempts, as a power of 2.
										* More attempts result in better and slower compression.
										* This parameter is useless when using "fast" and "dFast" strategies.
										* Special: value 0 means "use default searchLog". *)
		MinMatch: Integer;       (* Minimum size of searched matches.
										* Note that Zstandard can still find matches of smaller size,
										* it just tweaks its search algorithm to look for this size and larger.
										* Larger values increase compression and decompression speed, but decrease ratio.
										* Must be clamped between ZSTD_MINMATCH_MIN and ZSTD_MINMATCH_MAX.
										* Note that currently, for all strategies < btopt, effective minimum is 4.
										*                    , for all strategies > fast, effective maximum is 6.
										* Special: value 0 means "use default minMatchLength". *)
		TargetLength: Integer;   (* Impact of this field depends on strategy.
										* For strategies btopt, btultra & btultra2:
										*     Length of Match considered "good enough" to stop search.
										*     Larger values make compression stronger, and slower.
										* For strategy fast:
										*     Distance between match sampling.
										*     Larger values make compression faster, and weaker.
										* Special: value 0 means "use default targetLength". *)
		Strategy: Integer;       (* See ZSTD_strategy enum definition.
										* The higher the value of selected strategy, the more complex it is,
										* resulting in stronger and slower compression.
										* Special: value 0 means "use default strategy". *)

		(* LDM mode parameters *)
		EnableLongDistanceMatching: Boolean; (* Enable long distance matching.
												 * This parameter is designed to improve compression ratio
												 * for large inputs, by finding large matches at long distance.
												 * It increases memory usage and window size.
												 * Note: enabling this parameter increases default ZSTD_c_windowLog to 128 MB
												 * except when expressly set to a different value. *)
		LdmHashLog: Integer;     (* Size of the table for long distance matching, as a power of 2.
										* Larger values increase memory usage and compression ratio,
										* but decrease compression speed.
										* Must be clamped between ZSTD_HASHLOG_MIN and ZSTD_HASHLOG_MAX
										* default: windowlog - 7.
										* Special: value 0 means "automatically determine hashlog". *)
		LdmMinMatch: Integer;    (* Minimum match size for long distance matcher.
										* Larger/too small values usually decrease compression ratio.
										* Must be clamped between ZSTD_LDM_MINMATCH_MIN and ZSTD_LDM_MINMATCH_MAX.
										* Special: value 0 means "use default value" (default: 64). *)
		LdmBucketSizeLog: Integer; (* Log size of each bucket in the LDM hash table for collision resolution.
										* Larger values improve collision resolution but decrease compression speed.
										* The maximum value is ZSTD_LDM_BUCKETSIZELOG_MAX.
										* Special: value 0 means "use default value" (default: 3). *)
		LdmHashRateLog: Integer; (* Frequency of inserting/looking up entries into the LDM hash table.
										* Must be clamped between 0 and (ZSTD_WINDOWLOG_MAX - ZSTD_HASHLOG_MIN).
										* Default is MAX(0, (windowLog - ldmHashLog)), optimizing hash table usage.
										* Larger values improve compression speed.
										* Deviating far from default value will likely result in a compression ratio decrease.
										* Special: value 0 means "automatically determine hashRateLog". *)

		(* frame parameters *)
		ContentSizeFlag: Boolean; (* Content size will be written into frame header _whenever known_ (default:1)
										* Content size must be known at the beginning of compression.
										* This is automatically the case when using ZSTD_compress2(),
										* For streaming variants, content size must be provided with ZSTD_CCtx_setPledgedSrcSize() *)
		ChecksumFlag: Boolean;   (* A 32-bits checksum of content is written at end of frame (default:0) *)
		DictIDFlag: Boolean;     (* When applicable, dictionary's ID is written into frame header (default:1) *)

		(* multi-threading parameters *)
		(* These parameters are only useful if multi-threading is enabled (compiled with build macro ZSTD_MULTITHREAD).
		 * They return an error otherwise. *)
		Workers: Integer;        (* Select how many threads will be spawned to compress in parallel.
										* When nbWorkers >= 1, triggers asynchronous mode when used with ZSTD_compressStream*() :
										* ZSTD_compressStream*() consumes input and flush output if possible, but immediately gives back control to caller,
										* while compression work is performed in parallel, within worker threads.
										* (note : a strong exception to this rule is when first invocation of ZSTD_compressStream2() sets ZSTD_e_end :
										*  in which case, ZSTD_compressStream2() delegates to ZSTD_compress2(), which is always a blocking call).
										* More workers improve speed, but also increase memory usage.
										* Default value is `0`, aka "single-threaded mode" : no worker is spawned, compression is performed inside Caller's thread, all invocations are blocking *)
		JobSize: Integer;        (* Size of a compression job. This value is enforced only when nbWorkers >= 1.
										* Each compression job is completed in parallel, so this value can indirectly impact the nb of active threads.
										* 0 means default, which is dynamically determined based on compression parameters.
										* Job size must be a minimum of overlap size, or 1 MB, whichever is largest.
										* The minimum size is automatically and transparently enforced *)
		OverlapLog: Integer;     (* Control the overlap size, as a fraction of window size.
										* The overlap size is an amount of data reloaded from previous job at the beginning of a new job.
										* It helps preserve compression ratio, while each job is compressed in parallel.
										* This value is enforced only when nbWorkers >= 1.
										* Larger values increase compression ratio, but decrease speed.
										* Possible values range from 0 to 9 :
										* - 0 means "default" : value will be determined by the library, depending on strategy
										* - 1 means "no overlap"
										* - 9 means "full overlap", using a full window size.
										* Each intermediate rank increases/decreases load size by a factor 2 :
										* 9: full window;  8: w/2;  7: w/4;  6: w/8;  5:w/16;  4: w/32;  3:w/64;  2:w/128;  1:no overlap;  0:default
										* default value varies between 6 and 9, depending on strategy *)
	end;

	TZSTDDecompressOptions = record
		WindowLog: Integer;       (* Select a size limit (in power of 2) beyond which
										* the streaming API will refuse to allocate memory buffer
										* in order to protect the host from unreasonable memory requirements.
										* This parameter is only useful in streaming mode, since no internal buffer is allocated in single-pass mode.
										* By default, a decompression context accepts window sizes <= (1 << ZSTD_WINDOWLOG_LIMIT_DEFAULT).
										* Special: value 0 means "use default maximum windowLog". *)
	end;

type
	// Compress data as it is written out to a destination stream
	TZSTDCompressStream = class(TStream)
	private
		FDest: TStream;
		FCompressionOptions: TZSTDCompressOptions;
		FStreamOutBufferSize: size_t;
		FStreamOutBuffer: Pointer;
		FStream: ZSTD_CStream;
		procedure ValidateCompressionOptions(const Options: TZSTDCompressOptions);
	public
		constructor Create(ADest: TStream); overload;
		constructor Create(ADest: TStream; ALevel: Integer; AThreadCount: Integer = 0); overload; // ALevel in range 1-MaxLevel
		destructor Destroy; override;

		class function MaxLevel: Integer;

		function Write(const ABuffer; ACount: Longint): Longint; override;
		function Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64; override; // Do not use
		function Read(var ABuffer; ACount: Longint): Longint; override;             // Do not use

		property CompressionOptions: TZSTDCompressOptions read FCompressionOptions;
	end;

	// Decompress as data is read from a source stream
	TZSTDDecompressStream = class(TStream)
	private
		FSource: TStream;
		FOptions: TZSTDDecompressOptions;
		FPosition: Int64;
		FStreamInBufferSize: size_t;
		FStreamInBuffer: Pointer;
		FInput: ZSTD_inBuffer;
		FStreamOutBufferSize: size_t;
		FStreamOutBuffer: Pointer;
		FStreamOutBufferSizePos: size_t;
		FOutput: ZSTD_outBuffer;
		FStream: ZSTD_DStream;
		FSeekBuffer: Pointer;

		procedure LoadEmbeddedDictionary(ASourceStream: TStream; DecompressionStream: ZSTD_DStream);
	public
		constructor Create(ASource: TStream); overload;
		constructor Create(ASource: TStream; const AOption: TZSTDDecompressOptions); overload;
		destructor Destroy; override;

		function Read(var ABuffer; ACount: Longint): Longint; override;
		function Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64; override;	// you may only seek forward from current position
		function Write(const ABuffer; ACount: Longint): Longint; override;				// do not use
	end;

	// Helper functions

	procedure ZSTDCompressStream(ASource, ADest: TStream; ACount: Int64=0);

	procedure ZSTDDecompressStream(ASource, ADest: TStream; ACount: Int64=0);

	procedure ZSTDDecompressFile(SourceFilename: string; DecompressedFilename: string);

	function ZSTDDecompressBytes(SourceBuffer: AnsiString): AnsiString;


implementation

uses
	SysUtils, Math;

const
	COPY_BUFFER_SIZE = 65536;

procedure ZSTDCompressStream(ASource, ADest: TStream; ACount: Int64=0);
var
	buffer: Pointer;
	compressStream: TStream;
	fullStream: Boolean;
	read: Integer;
begin
	GetMem({var}buffer, COPY_BUFFER_SIZE);
	try
		compressStream := TZSTDCompressStream.Create(ADest);
		try
			fullStream := (ACount = 0);
			while True do
			begin
				if fullStream then
				begin
					read := ASource.Read(Buffer^, COPY_BUFFER_SIZE);
					if read = 0 then
						Break;
					compressStream.WriteBuffer(buffer^, read);
				end
			else
			begin
				if ACount > COPY_BUFFER_SIZE then
					Read := COPY_BUFFER_SIZE
				else
					Read := ACount;
				ASource.ReadBuffer(Buffer^, Read);
				CompressStream.WriteBuffer(Buffer^, Read);
				Dec(ACount, Read);
				if ACount = 0 then
					Break;
				end;
			end;
		finally
			compressStream.Free;
		end;
	finally
		FreeMem(buffer);
	end;
end;

procedure ZSTDDecompressStream(ASource, ADest: TStream; ACount: Int64 = 0);
var
	buffer: Pointer;
	decompressStream: TStream;
	fullStream: Boolean;
	read: Integer;
begin
	if ASource = nil then
		raise Exception.Create('ASource is nil');
	if ADest = nil then
		raise Exception.Create('ADest is nil');
	if ACount < 0 then
		raise Exception.Create('ACount must be positive, or zero (read all)');

	GetMem(buffer, COPY_BUFFER_SIZE);
	try
		decompressStream := TZSTDDecompressStream.Create(ASource);
		try
			fullStream := (ACount = 0);
			while True do
			begin
				if fullStream then
				begin
					read := decompressStream.Read(buffer^, COPY_BUFFER_SIZE);
					if read = 0 then
						Break;
					ADest.WriteBuffer(buffer^, read);
				end
				else
				begin
					if ACount > COPY_BUFFER_SIZE then
						read := COPY_BUFFER_SIZE
					else
						read := ACount;
					decompressStream.ReadBuffer(buffer^, read);
					ADest.WriteBuffer(buffer^, read);
					Dec(ACount, read);
					if ACount = 0 then
						Break;
				end;
			end;
		finally
			decompressStream.Free;
		end;
	finally
		FreeMem(buffer);
	end;
end;

procedure ZSTDDecompressFile(SourceFilename: string; DecompressedFilename: string);
var
	fsSource: TFileStream;
	fsDest: TFileStream;
begin
	if SourceFilename = '' then
		raise Exception.Create('[ZSTDDecompressFile] SourceFilename is required');
	if DecompressedFilename = '' then
		raise Exception.Create('[ZSTDDecompressFile] DecompressedFilename is required');


	fsSource := TFileStream.Create(SourceFilename, fmOpenRead or fmShareDenyNone);
	try
		fsDest := TFileStream.Create(DecompressedFilename, fmCreate or fmShareDenyNone);
		try
			ZSTDDecompressStream(fsSource, fsDest);
		finally
			FreeAndNil(fsDest);
		end;
	finally
		FreeAndNil(fsSource);
	end;
end;

function ZSTDDecompressBytes(SourceBuffer: AnsiString): AnsiString;
var
	res: size_t;
begin
	SetLength(Result, 0);

	// Try to get an estimate of the decompressed size
	res := ZSTD_getFrameContentSize(Pointer(SourceBuffer), Length(SourceBuffer));
	ZSTDCheck(SZSTD_getFrameContentSize, res);
	SetLength(Result, res);

	// Decompress the source buffer into the destination buffer
	res := ZSTD_decompress(Pointer(Result), Length(Result), Pointer(SourceBuffer), Length(SourceBuffer));
	ZSTDCheck(SZSTD_decompress, res);
	SetLength(Result, res);
end;

//**************************************************************************************************
// TZSTDCompressStream
//**************************************************************************************************

class function TZSTDCompressStream.MaxLevel: Integer;
begin
	Result := ZSTDLib.ZSTD_maxCLevel();
end;

constructor TZSTDCompressStream.Create(ADest: TStream; ALevel: Integer; AThreadCount: Integer = 0);
begin
	Self.Create(ADest);

	FCompressionOptions.CompressionLevel := ALevel;
	FCompressionOptions.Workers := AThreadCount;
end;

constructor TZSTDCompressStream.Create(ADest: TStream);
const
	Default_TZSTDCompressOptions: TZSTDCompressOptions = ();
begin
	inherited Create;

	FCompressionOptions := Default_TZSTDCompressOptions;
	FCompressionOptions.CompressionLevel := ZSTD_CLEVEL_DEFAULT;
	FCompressionOptions.DictIDFlag := True;
end;

destructor TZSTDCompressStream.Destroy;
var
	input: ZSTD_inBuffer;
	output: ZSTD_outBuffer;
	r: size_t;
begin
	try
		if Assigned(FStream) then
		try
			Input.src := nil;
			Input.size := 0;
			Input.pos := 0;
			while True do
			begin
				Output.dst := FStreamOutBuffer;
				Output.size := FStreamOutBufferSize;
				Output.pos := 0;
				R := ZSTDCheck(sZSTD_compressStream2, ZSTD_compressStream2(FStream, Output, Input, ZSTD_e_end));
				if Output.pos > 0 then
					FDest.WriteBuffer(FStreamOutBuffer^, Output.pos);
				if R = 0 then
					Break;
			end;
		finally
			ZSTD_freeCStream(FStream);
		end;
	finally
		if Assigned(FStreamOutBuffer) then
			FreeMem(FStreamOutBuffer);
	end;

	inherited Destroy;
end;

function TZSTDCompressStream.Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64;
begin
	//You cannot seek a compression stream
	raise Exception.Create('[TZSTDCompressStream.Seek] Cannot seek a compression stream');
end;

function TZSTDCompressStream.Read(var ABuffer; ACount: Longint): Longint;
begin
	//You cannot read from a compression stream
	raise Exception.Create('[TZSTDCompressStream.Read] Cannot read from a compression stream');
end;

function TZSTDCompressStream.Write(const ABuffer; ACount: Longint): Longint;
const
	Bools: array[Boolean] of Integer = (0, 1);
var
	input: ZSTD_inBuffer;
	output: ZSTD_outBuffer;
begin
	Result := ACount;
	if ACount = 0 then
		Exit;

	if not Assigned(FStreamOutBuffer) then
	begin
		FStreamOutBufferSize := ZSTD_CStreamOutSize;
		GetMem(FStreamOutBuffer, FStreamOutBufferSize);
	end;

	if not Assigned(FStream) then
	begin
		FStream := ZSTD_createCStream;
		if not Assigned(FStream) then
			raise EOutOfMemory.Create('');
		//ZSTDCheck(sZSTD_initCStream, ZSTD_initCStream(FStream, FLevel));
		ZSTDCheck(sZSTD_CCtx_reset, ZSTD_CCtx_reset(FStream, ZSTD_reset_session_and_parameters));
		ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_compressionLevel,	FCompressionOptions.CompressionLevel));
		ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_enableLongDistanceMatching, Bools[FCompressionOptions.EnableLongDistanceMatching]));
		ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_windowLog, 			FCompressionOptions.WindowLog));
		ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_hashLog, 				FCompressionOptions.HashLog));
		ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_chainLog, 			FCompressionOptions.ChainLog));
		ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_searchLog, 			FCompressionOptions.SearchLog));
		ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_minMatch, 			FCompressionOptions.MinMatch));
		ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_targetLength, 		FCompressionOptions.TargetLength));
		ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_strategy, 			FCompressionOptions.Strategy));
		ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_ldmHashLog, 			FCompressionOptions.LdmHashLog));
		ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_ldmMinMatch, 		FCompressionOptions.LdmMinMatch));
		ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_ldmBucketSizeLog, 	FCompressionOptions.LdmBucketSizeLog));
		ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_ldmHashRateLog, 	FCompressionOptions.LdmHashRateLog));
		ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_contentSizeFlag, 	Bools[FCompressionOptions.ContentSizeFlag]));
		ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_checksumFlag, 		Bools[FCompressionOptions.ChecksumFlag]));
		ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_dictIDFlag, 			Bools[FCompressionOptions.DictIDFlag]));
		ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_nbWorkers, 			FCompressionOptions.Workers));
		ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_jobSize, 				FCompressionOptions.JobSize));
		ZSTDCheck(sZSTD_CCtx_setParameter, ZSTD_CCtx_setParameter(FStream, ZSTD_c_overlapLog, 			FCompressionOptions.OverlapLog));
	end;

	input.src := @ABuffer;
	input.size := ACount;
	input.pos := 0;

	while input.pos < input.size do
	begin
		output.dst := FStreamOutBuffer;
		output.size := FStreamOutBufferSize;
		output.pos := 0;
		ZSTDCheck(sZSTD_compressStream2, ZSTD_compressStream2(FStream, output, input, ZSTD_e_continue));
		if output.pos > 0 then
			FDest.WriteBuffer(FStreamOutBuffer^, Output.pos);
	end;
end;

//**************************************************************************************************
// TZSTDDecompressStream
//**************************************************************************************************

constructor TZSTDDecompressStream.Create(ASource: TStream);
const
	Default_TZSTDDecompressOptions: TZSTDDecompressOptions = ();
begin
	inherited Create;
	FOptions := Default_TZSTDDecompressOptions;

	FSource := ASource;
end;

constructor TZSTDDecompressStream.Create(ASource: TStream; const AOption: TZSTDDecompressOptions);
begin
	inherited Create;
	FSource := ASource;
	FOptions := AOption;
end;

destructor TZSTDDecompressStream.Destroy;
begin
	if Assigned(FStream) then
		ZSTD_freeDStream(FStream);
	if Assigned(FStreamInBuffer) then
		FreeMem(FStreamInBuffer);
	if Assigned(FStreamOutBuffer) then
		FreeMem(FStreamOutBuffer);
	if Assigned(FSeekBuffer) then
		FreeMem(FSeekBuffer);

	inherited Destroy;
end;

function TZSTDDecompressStream.Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64;
const
	SEEK_BUFFER_SIZE = 65536;
var
	seekSize: Integer;
	seekSizeTotal: Int64;
begin
	if AOrigin <> soCurrent then
		raise Exception.Create('[TZSTDecompressStream.Seek] SeekOrigin must be soCurrent');
	if AOffset < 0 then
		raise Exception.Create('[TZSTDecompressStream.Seek] Seek offset must be positive');

	if AOffset = 0 then
	begin
		//They're asking for the current position
		Result := FPosition;
		Exit;
	end;

	if not Assigned(FSeekBuffer) then
		GetMem(FSeekBuffer, SEEK_BUFFER_SIZE);
	seekSizeTotal := AOffset;
	while seekSizeTotal > 0 do
	begin
		if seekSizeTotal >= SEEK_BUFFER_SIZE then
			seekSize := SEEK_BUFFER_SIZE
		else
			seekSize := seekSizeTotal;
		seekSize := Self.Read(FSeekBuffer^, seekSize);
		if seekSize = 0 then
			Break;
		Dec(seekSizeTotal, seekSize);
	end;
	Result := FPosition;
end;

function TZSTDDecompressStream.Read(var ABuffer; ACount: Longint): Longint;
var
	availableCount: size_t;
	buffer: PByte;
	source: PByte;
begin
	Result := 0;
	if ACount = 0 then
		Exit;

	if not Assigned(FStreamInBuffer) then
	begin
		FStreamInBufferSize := ZSTD_DStreamInSize;
		GetMem(FStreamInBuffer, FStreamInBufferSize);
		FInput.src := FStreamInBuffer;
		FInput.size := FStreamInBufferSize;
		FInput.pos := FStreamInBufferSize;
	end;

	if not Assigned(FStreamOutBuffer) then
	begin
		FStreamOutBufferSize := ZSTD_DStreamOutSize;
		GetMem(FStreamOutBuffer, FStreamOutBufferSize);
		FOutput.dst := FStreamOutBuffer;
		FOutput.size := FStreamOutBufferSize;
		FStreamOutBufferSizePos := 0;
	end;

	if not Assigned(FStream) then
	begin
		FStream := ZSTD_createDStream;
		if not Assigned(FStream) then
			raise EOutOfMemory.Create('');
		ZSTDCheck(sZSTD_initDStream, ZSTD_initDStream(FStream));
		ZSTDCheck(sZSTD_DCtx_setParameter, ZSTD_DCtx_setParameter(FStream, ZSTD_d_windowLogMax, FOptions.WindowLog));

		LoadEmbeddedDictionary(FSource, FStream);
	end;

	buffer := @ABuffer;
	while ACount > 0 do
	begin
		availableCount := FOutput.pos - FStreamOutBufferSizePos;
		if Integer(availableCount) > ACount then
			availableCount := ACount;
		if availableCount > 0 then
		begin
			Source := FStreamOutBuffer;
			Inc(Source, FStreamOutBufferSizePos);
			CopyMemory(buffer, source, availableCount);
			Inc(FStreamOutBufferSizePos, availableCount);
			Inc(buffer, availableCount);
			Dec(ACount, availableCount);
			Inc(Result, availableCount);
			Inc(FPosition, availableCount);
			if ACount = 0 then
				Break;
		end;

		FOutput.pos := 0;
		FStreamOutBufferSizePos := 0;

		if (FInput.pos = FInput.size) and (FInput.size > 0) then
		begin
			FInput.size := FSource.Read(FStreamInBuffer^, FInput.size);
			FInput.pos := 0;
		end;

		ZSTDCheck(sZSTD_decompressStream, ZSTD_decompressStream(FStream, FOutput, FInput));
		if (FOutput.pos = 0) and (FInput.size = 0) then
			Break;
	end;
end;

function TZSTDDecompressStream.Write(const ABuffer; ACount: Longint): Longint;
begin
	//You cannot write to a *decompression* stream.
	raise Exception.Create('[TZSTDDecompressStream.Write] Cannot write to a decompression stream');
end;

procedure TZSTDDecompressStream.LoadEmbeddedDictionary(ASourceStream: TStream; DecompressionStream: ZSTD_DStream);
var
	dwMagic: DWORD;
	dwDictSize: DWORD;
	dwDictionaryMagic: DWORD;
	d: AnsiString; //i.e. RawByteString
	res: size_t;
const
	ZSTD_MAGIC					= $184D2A5D;					// 5D 2A 4D 18
	COMPRESSED_DICT_MAGIC	= ZSTD_MAGICNUMBER;			// 28 B5 2F FD
	UNCOMPRESSED_DICT_MAGIC	= ZSTD_MAGIC_DICTIONARY;	// 37 A4 30 EC
begin
	if ASourceStream = nil then
		Exit;

	ASourceStream.ReadBuffer(dwMagic, 4);
	if dwMagic <> ZSTD_MAGIC then
		Exit; //not a valid .zst

	ASourceStream.ReadBuffer(dwDictSize, 4);
	if dwDictSize < 4 then
		Exit; //dictionary too small
	if dwDictSize > (100 * 1024*1024) then
		Exit; //dictionary too large (104,857,600 bytes)

	SetLength(d, dwDictSize);
	ASourceStream.ReadBuffer(d[1], dwDictSize);

	dwDictionaryMagic := PDWORD(@d[1])^;
	if (dwDictionaryMagic <> COMPRESSED_DICT_MAGIC) and (dwDictionaryMagic <> UNCOMPRESSED_DICT_MAGIC) then
		Exit; //not a valid dictionary

	if dwDictionaryMagic = COMPRESSED_DICT_MAGIC then
	begin
		d := ZSTDDecompressBytes(d);
	end;

	res := ZSTD_DCtx_loadDictionary(DecompressionStream, PAnsiChar(d), Length(d));
	ZSTDCheck(SZSTD_DCtx_loadDictionary, res);
end;

procedure TZSTDCompressStream.ValidateCompressionOptions(const Options: TZSTDCompressOptions);

	procedure CheckBounds(Value: Integer; Parameter: ZSTD_cParameter);
	var
		bounds: ZSTD_bounds;
	begin
		if Value = 0 then //ZSTD is very careful that all options are such that 0 (zero) is a default.
			Exit;

		bounds := ZSTD_cParam_getBounds(Parameter);
		if (Value < bounds.lowerBound) or (Value > bounds.upperBound) then
			raise EInvalidArgument.Create('Compression option %d out of bounds');
	end;

begin
	CheckBounds(Options.CompressionLevel,	ZSTD_c_compressionLevel); //special value zero means default
	CheckBounds(Options.WindowLog,			ZSTD_c_windowLog);
	CheckBounds(Options.HashLog,				ZSTD_c_hashLog);
	CheckBounds(Options.ChainLog,				ZSTD_c_chainLog);
	CheckBounds(Options.SearchLog,			ZSTD_c_searchLog);
	CheckBounds(Options.MinMatch,				ZSTD_c_minMatch);
	CheckBounds(Options.TargetLength,		ZSTD_c_targetLength);
	CheckBounds(Options.Strategy,				ZSTD_c_strategy);
	CheckBounds(Options.LdmHashLog,			ZSTD_c_ldmHashLog);
	CheckBounds(Options.LdmMinMatch,			ZSTD_c_ldmMinMatch);
	CheckBounds(Options.LdmBucketSizeLog,	ZSTD_c_ldmBucketSizeLog);
	CheckBounds(Options.LdmHashRateLog,		ZSTD_c_ldmHashRateLog);
	CheckBounds(Options.Workers,				ZSTD_c_nbWorkers);
	CheckBounds(Options.JobSize,				ZSTD_c_jobSize);
	CheckBounds(Options.OverlapLog,			ZSTD_c_overlapLog);
end;

end.

