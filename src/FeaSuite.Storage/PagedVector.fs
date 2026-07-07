namespace FeaSuite.Storage

open System
open System.IO
open FeaSuite.Core

// ---------------------------------------------------------------------------
// PagedVector<float> – file-backed float vector with an in-memory page cache.
//
// File layout:
//   - 16-byte header: int64 length, int32 pageSize, 4 bytes padding
//   - Data pages: each page stores `pageSize` float64 values (8 bytes each)
//
// Up to `maxCachePages` pages are kept in memory. When a new page is needed
// and the cache is full, the least-recently-used dirty page is flushed to disk.
// ---------------------------------------------------------------------------

/// A cached page of float[] with dirty tracking.
[<AllowNullLiteral>]
type private FloatPage(index: int, data: float[]) =
    let mutable dirty = false
    member _.Index   = index
    member _.Data    = data
    member _.IsDirty = dirty
    member _.MarkDirty() = dirty <- true
    member _.MarkClean() = dirty <- false

/// File-backed paged vector of float values.
type PagedVector private (filePath: string, length: int, pageSize: int, stream: BinaryWriter * BinaryReader * FileStream) =

    let (writer, reader, fs) = stream
    let headerBytes = 16
    let maxCachePages = 8

    let cache       = System.Collections.Generic.Dictionary<int, FloatPage>()
    let accessOrder = System.Collections.Generic.LinkedList<int>()

    let pageByteOffset (pageIdx: int) = int64 headerBytes + int64 pageIdx * int64 pageSize * 8L

    let readPageFromDisk (pageIdx: int) : float[] =
        let offset = pageByteOffset pageIdx
        let count  = min pageSize (length - pageIdx * pageSize)
        let data   = Array.zeroCreate<float> pageSize
        if offset < fs.Length then
            fs.Seek(offset, SeekOrigin.Begin) |> ignore
            for i in 0 .. count - 1 do
                data.[i] <- reader.ReadDouble()
        data

    let writePageToDisk (page: FloatPage) =
        let offset = pageByteOffset page.Index
        let count  = min pageSize (length - page.Index * pageSize)
        fs.Seek(offset, SeekOrigin.Begin) |> ignore
        for i in 0 .. count - 1 do
            writer.Write(page.Data.[i])
        page.MarkClean()

    let getOrLoadPage (pageIdx: int) : FloatPage =
        match cache.TryGetValue pageIdx with
        | true, page ->
            accessOrder.Remove pageIdx |> ignore
            accessOrder.AddLast pageIdx |> ignore
            page
        | false, _ ->
            // Evict LRU page if cache is full
            if cache.Count >= maxCachePages then
                let firstNode = accessOrder.First
                if not (obj.ReferenceEquals(firstNode, null)) then
                    let oldest = firstNode.Value
                    accessOrder.RemoveFirst()
                    let evicted = cache.[oldest]
                    cache.Remove oldest |> ignore
                    if evicted.IsDirty then writePageToDisk evicted

            let data = readPageFromDisk pageIdx
            let page = FloatPage(pageIdx, data)
            cache.[pageIdx] <- page
            accessOrder.AddLast pageIdx |> ignore
            page

    let flushAll () =
        for kvp in cache do
            if kvp.Value.IsDirty then writePageToDisk kvp.Value
        writer.Flush()

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    member _.Length = length

    member _.Item
        with get (index: int) : float =
            if index < 0 || index >= length then raise (ArgumentOutOfRangeException "index")
            let page = getOrLoadPage (index / pageSize)
            page.Data.[index % pageSize]
        and set (index: int) (value: float) =
            if index < 0 || index >= length then raise (ArgumentOutOfRangeException "index")
            let page = getOrLoadPage (index / pageSize)
            page.Data.[index % pageSize] <- value
            page.MarkDirty()

    member _.Flush() = flushAll ()

    member self.ToArray() =
        [| for i in 0 .. length - 1 -> self.[i] |]

    interface IPagedVector<float> with
        member self.Length          = self.Length
        member self.GetValue i      = self.[i]
        member self.SetValue(i, v)  = self.[i] <- v
        member self.Flush()         = self.Flush()

    interface System.IDisposable with
        member _.Dispose() =
            flushAll ()
            writer.Dispose()
            reader.Dispose()
            fs.Dispose()

    // -----------------------------------------------------------------------
    // Factory
    // -----------------------------------------------------------------------

    static member private HeaderBytes = 16

    static member Create(filePath: string, length: int, ?pageSize: int) : PagedVector =
        let ps = defaultArg pageSize 1024
        let fs = new FileStream(filePath, FileMode.Create, FileAccess.ReadWrite, FileShare.None, 4096, false)
        let totalPages = int (Math.Ceiling(float length / float ps))
        let totalSize  = int64 PagedVector.HeaderBytes + int64 totalPages * int64 ps * 8L
        fs.SetLength totalSize
        let bw = new BinaryWriter(fs, Text.Encoding.Default, true)
        let br = new BinaryReader(fs, Text.Encoding.Default, true)
        fs.Seek(0L, SeekOrigin.Begin) |> ignore
        bw.Write(int64 length)
        bw.Write(int32 ps)
        bw.Write(int32 0)  // padding
        new PagedVector(filePath, length, ps, (bw, br, fs))

    static member Open(filePath: string) : PagedVector =
        let fs = new FileStream(filePath, FileMode.Open, FileAccess.ReadWrite, FileShare.None, 4096, false)
        let br = new BinaryReader(fs, Text.Encoding.Default, true)
        let bw = new BinaryWriter(fs, Text.Encoding.Default, true)
        fs.Seek(0L, SeekOrigin.Begin) |> ignore
        let length   = int (br.ReadInt64())
        let pageSize = br.ReadInt32()
        new PagedVector(filePath, length, pageSize, (bw, br, fs))
