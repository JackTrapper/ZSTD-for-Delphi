ZSTD
====

**Zstandard**, or `zstd`` for version, is a fast lossless compression algorithm, targeting real-time compression scenarios at zlib-level and better compression ratios. It's backed by a very fast entropy stage, provided by [Huff0 and FSE library](https://github.com/Cyan4973/FiniteStateEntropy).

ZStandard only provides the ability to compress one file. The resulting compressed file does not contain any metadata; it is only the compressed version of the original file's contents.

Zstandard's format is stable and documented in [RFC8878](https://datatracker.ietf.org/doc/html/rfc8878).

ZSTD for Delphi
===============

Wrapper around libzstd.dll
--------------------------

This code does not implement Zstandard from scratch, but instead is a wrapper around the [`libzstd.dll` reference library](https://github.com/facebook/zstd/releases) which are available in x86 and x64 builds. This code provides API header translations, as well as static and dynamic linking to the `libzstd.dll` library.

You can find the API documentation in the [zstd Manual](https://facebook.github.io/zstd/zstd_manual.html).

Compression and Decompression Streams
-------------------------------------

This code also supplies **TZSTDCompressStream** and **TZSTDDecompressionStream** classes that descend from [**TStream**](https://docwiki.embarcadero.com/Libraries/Alexandria/en/System.Classes.TStream). This allows you to use the common paradigm when dealing with compression to read the compressed file out of a **TStream**, where the contents are decompressed as you read.

Sample Usage
============

1. Read bytes from sourceStream, and compress them into destinationStream

       type
          sourceStream: TStream;
          destinationStream: TStream;

       // Read data from source, and compress it to dest
       ZSTDCompressStream(sourceStream, destinationStream);

2. Read compressed bytes from a sourceStream, and uncompress them into destionationStream

       // Read from compressed stream and write it out to the dest uncompressed
       ZSTDDecompressStream(sourceStream, destinationStream);

3. Decompress a compressed .zst file

       // decompress .zst compressed file
       ZSTDDecompressFile(
             'E:\ArchiveTeam\imgur_20230427191011_0c1de22b.1682559222.megawarc.warc.zst', // source filename
             'E:\ArchiveTeam\imgur_20230427191011_0c1de22b.1682559222.megawarc.warc'      // decompressed filename
       );

4. Decompress a buffer in memory

       var
          compressedBytes: RawByteString;
          decompressedBytes: RawByteString;

       // decompress the buffer in memory
       decompressedBytes := ZSTDDecompressBytes(compressedBytes);

WARC Embedded Dictionary
========================

The `.zst` file format is a collection of frames, each identified by a 4-byte prefix. Some standard prefixes are:

- `0xFD2FB528` - Standard Zstandard frame, indicating Zstandard compressed data.
- `0xEC30A437` - Zstandard dictionary, used for files containing a compression or decompression dictionary.
- `0x184D2A50` to `0x184D2A5F` - Range for skippable frames, allowing the inclusion of non-compressed data or metadata in a Zstandard stream.

Frames with a magic number between `0x184D2A50` to `0x184D2A5F` will be skipped by any standards-compliant decoder that doesn't recognize the magic number. These frames allow the inclusion of future non-compressed or other metata. 

WARC makes use of this [Skippable frames](...todo add url.) feature. The WARC standard defines the ability to optionally prefix a `.zst` file with a skippable frame `0x184D2A5D`. This frame contains the *dictionary* that is to be supplied to the Zstandard decompressor. The dictionary can either be compressed or uncompressed:

- `ZSTD_MAGIC_DICTIONARY = $EC30A437;    // Uncompressed dictionary`
- `ZSTD_MAGICNUMBER      = $FD2FB528;    // Compressed dictionary`

This dictionary is then supposed to be supplied to the Zstandard decoder through the [**ZSTD_DCtx_loadDictionary API**](https://facebook.github.io/zstd/zstd_manual.html#Chapter13) function. If the dictionary is compressed, it must first be decompressed using Zstandard, before supplying it to **ZSTD_DCtx_loadDictionary**.

ZSTD-for-Delphi honors this WARC extension of the standard, and will look for the dictionary frame, and pass it to Zstandard as the dictionary to use. [Here](https://github.com/facebook/zstd/pull/2349) is the pull request to include this behavior in the Zstandard specification.

