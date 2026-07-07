module FeaSuite.Tests.PagedStorageTests

open System.IO
open Xunit
open FeaSuite.Core
open FeaSuite.Storage

// ---------------------------------------------------------------------------
// Paged storage tests
// ---------------------------------------------------------------------------

/// Get a unique temp file path (cleaned up after each test).
let private tmpFile () = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())

[<Fact>]
let ``PagedVector: create, write, read back all elements`` () =
    let path = tmpFile ()
    try
        use vec = PagedVector.Create(path, 100, pageSize = 16)
        for i in 0 .. 99 do
            vec.[i] <- float i * 1.5
        vec.Flush()
        for i in 0 .. 99 do
            let expected = float i * 1.5
            let actual   = vec.[i]
            Assert.Equal(expected, actual)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``PagedVector: persists data across open/close`` () =
    let path = tmpFile ()
    try
        // Write phase – scoped so the file is closed before re-opening
        let expected = Array.init 50 float
        (use vec = PagedVector.Create(path, 50, pageSize = 8)
         for i in 0 .. 49 do vec.[i] <- expected.[i]
         vec.Flush())

        // Re-open and verify
        use vec2 = PagedVector.Open(path)
        Assert.Equal(50, vec2.Length)
        for i in 0 .. 49 do
            Assert.Equal(expected.[i], vec2.[i])
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``PagedVector: index out of range throws`` () =
    let path = tmpFile ()
    try
        use vec = PagedVector.Create(path, 10)
        Assert.Throws<System.ArgumentOutOfRangeException>(fun () -> vec.[-1] |> ignore) |> ignore
        Assert.Throws<System.ArgumentOutOfRangeException>(fun () -> vec.[10] |> ignore) |> ignore
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``PagedMatrixStore: add and read back single entry`` () =
    let path = tmpFile ()
    try
        use store = new PagedMatrixStore(path, 4, 4)
        store.Add(1, 2, 3.14)
        store.Flush()
        let v = store.Get(1, 2)
        Assert.Equal(3.14, v)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``PagedMatrixStore: entries accumulate (duplicate (i,j) summed)`` () =
    let path = tmpFile ()
    try
        use store = new PagedMatrixStore(path, 3, 3)
        store.Add(0, 0, 1.0)
        store.Add(0, 0, 2.0)
        store.Add(0, 0, 3.0)
        store.Flush()
        let v = store.Get(0, 0)
        Assert.Equal(6.0, v)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``PagedMatrixStore: ToDense returns correct 2x2 matrix`` () =
    let path = tmpFile ()
    try
        use store = new PagedMatrixStore(path, 2, 2)
        store.Add(0, 0,  2.0)
        store.Add(0, 1, -1.0)
        store.Add(1, 0, -1.0)
        store.Add(1, 1,  2.0)
        let A = store.ToDense()
        Assert.Equal( 2.0, A.[0, 0])
        Assert.Equal(-1.0, A.[0, 1])
        Assert.Equal(-1.0, A.[1, 0])
        Assert.Equal( 2.0, A.[1, 1])
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``PagedVector: large vector with small page size pages correctly`` () =
    let path = tmpFile ()
    try
        let n = 5000
        use vec = PagedVector.Create(path, n, pageSize = 64)
        for i in 0 .. n - 1 do vec.[i] <- float i
        vec.Flush()
        // Verify every element
        for i in 0 .. n - 1 do
            Assert.Equal(float i, vec.[i])
    finally
        if File.Exists path then File.Delete path
