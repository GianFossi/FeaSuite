namespace FeaSuite.Storage

open System
open System.IO
open FeaSuite.Core

// ---------------------------------------------------------------------------
// PagedMatrixStore – file-backed sparse matrix (COO format with paging)
//
// Stores triplets (row, col, value) on disk.
// Provides:
//   - Add(row, col, val)  → append a triplet
//   - Get(row, col)       → scan and sum matching triplets (assembled value)
//   - ToDense()           → materialise the full dense matrix (for small problems)
//   - Flush()             → flush the write buffer to disk
//
// File format:
//   Header (24 bytes): int32 rows, int32 cols, int64 entryCount, 8 bytes padding
//   Data: sequence of (int32 row, int32 col, float64 value) = 16 bytes each
// ---------------------------------------------------------------------------

[<Struct>]
type private TripletEntry = { Row: int32; Col: int32; Value: float }

/// File-backed sparse matrix using COO storage with page-buffered writes.
type PagedMatrixStore(filePath: string, rows: int, cols: int, ?writeBufferSize: int) =

    let bufSize      = defaultArg writeBufferSize 256
    let entryBytes   = 16  // 4 + 4 + 8
    let headerBytes  = 24

    let mutable entryCount : int64 = 0L
    let writeBuffer          = Array.zeroCreate<TripletEntry> bufSize
    let mutable bufPos       = 0

    let stream =
        let fs = new FileStream(filePath, FileMode.Create, FileAccess.ReadWrite, FileShare.None)
        // Write placeholder header
        let hdr = Array.zeroCreate<byte> headerBytes
        fs.Write(hdr)
        fs

    let writeHeader () =
        stream.Seek(0L, SeekOrigin.Begin) |> ignore
        let hdr = Array.zeroCreate<byte> headerBytes
        BitConverter.TryWriteBytes(System.Span<byte>(hdr, 0, 4),  rows)       |> ignore
        BitConverter.TryWriteBytes(System.Span<byte>(hdr, 4, 4),  cols)       |> ignore
        BitConverter.TryWriteBytes(System.Span<byte>(hdr, 8, 8),  entryCount) |> ignore
        stream.Write(hdr)

    let flushBuffer () =
        if bufPos > 0 then
            stream.Seek(0L, SeekOrigin.End) |> ignore
            let chunk = Array.zeroCreate<byte> (bufPos * entryBytes)
            for i in 0 .. bufPos - 1 do
                let e   = writeBuffer.[i]
                let off = i * entryBytes
                BitConverter.TryWriteBytes(System.Span<byte>(chunk, off,     4), e.Row)   |> ignore
                BitConverter.TryWriteBytes(System.Span<byte>(chunk, off + 4, 4), e.Col)   |> ignore
                BitConverter.TryWriteBytes(System.Span<byte>(chunk, off + 8, 8), e.Value) |> ignore
            stream.Write(chunk)
            entryCount <- entryCount + int64 bufPos
            bufPos <- 0
            writeHeader ()

    member _.Rows = rows
    member _.Cols = cols
    member _.EntryCount = entryCount

    /// Append a triplet (row, col, value).
    member _.Add(row: int, col: int, value: float) =
        writeBuffer.[bufPos] <- { Row = row; Col = col; Value = value }
        bufPos <- bufPos + 1
        if bufPos = bufSize then flushBuffer ()

    /// Return the assembled (summed) value at (row, col).
    member self.Get(row: int, col: int) : float =
        self.Flush()  // ensure everything is on disk
        stream.Seek(int64 headerBytes, SeekOrigin.Begin) |> ignore
        let mutable sum = 0.0
        let entryBuf    = Array.zeroCreate<byte> entryBytes
        for _ in 1L .. entryCount do
            stream.Read(entryBuf) |> ignore
            let r = BitConverter.ToInt32(entryBuf, 0)
            let c = BitConverter.ToInt32(entryBuf, 4)
            if r = row && c = col then
                sum <- sum + BitConverter.ToDouble(entryBuf, 8)
        sum

    /// Materialise as a dense float[,] (only practical for small matrices).
    member self.ToDense() : float[,] =
        self.Flush()
        stream.Seek(int64 headerBytes, SeekOrigin.Begin) |> ignore
        let A = Array2D.zeroCreate<float> rows cols
        let entryBuf = Array.zeroCreate<byte> entryBytes
        for _ in 1L .. entryCount do
            stream.Read(entryBuf) |> ignore
            let r = BitConverter.ToInt32(entryBuf, 0)
            let c = BitConverter.ToInt32(entryBuf, 4)
            let v = BitConverter.ToDouble(entryBuf, 8)
            if r >= 0 && r < rows && c >= 0 && c < cols then
                A.[r, c] <- A.[r, c] + v
        A

    /// Flush write buffer to disk and update file header.
    member _.Flush() = flushBuffer ()

    interface IPagedMatrixStore with
        member self.Rows         = self.Rows
        member self.Cols         = self.Cols
        member self.Add(r, c, v) = self.Add(r, c, v)
        member self.Get(r, c)    = self.Get(r, c)
        member self.ToDense()    = self.ToDense()
        member self.Flush()      = self.Flush()

    interface System.IDisposable with
        member self.Dispose() =
            self.Flush()
            stream.Dispose()

    /// Open an existing PagedMatrixStore file (read-only scan).
    static member OpenReadOnly(filePath: string) : (int * int * int64 * (int * int * float) seq) =
        use stream = new FileStream(filePath, FileMode.Open, FileAccess.Read, FileShare.Read)
        let hdr = Array.zeroCreate<byte> 24
        stream.Read(hdr) |> ignore
        let rows  = BitConverter.ToInt32(hdr, 0)
        let cols  = BitConverter.ToInt32(hdr, 4)
        let count = BitConverter.ToInt64(hdr, 8)
        let entryBuf = Array.zeroCreate<byte> 16
        let entries = seq {
            for _ in 1L .. count do
                stream.Read(entryBuf) |> ignore
                let r = BitConverter.ToInt32(entryBuf, 0)
                let c = BitConverter.ToInt32(entryBuf, 4)
                let v = BitConverter.ToDouble(entryBuf, 8)
                yield r, c, v
        }
        rows, cols, count, entries
