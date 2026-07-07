namespace FeaSuite.Core

// ---------------------------------------------------------------------------
// Geometry adapter types
// Compatible with GianFossi/Geometry (https://github.com/GianFossi/Geometry).
// When the Geometry package is published to NuGet, replace these definitions
// with a package reference and open the Geometry namespace.
// ---------------------------------------------------------------------------

/// Immutable 3-D point.
type Point3D = { X: float; Y: float; Z: float }

/// Immutable 3-D vector.
type Vector3D = { Dx: float; Dy: float; Dz: float }

module Point3D =
    let origin : Point3D = { X = 0.0; Y = 0.0; Z = 0.0 }

    /// Euclidean distance between two points.
    let distanceTo (a: Point3D) (b: Point3D) : float =
        let dx = b.X - a.X
        let dy = b.Y - a.Y
        let dz = b.Z - a.Z
        sqrt (dx * dx + dy * dy + dz * dz)

    /// Create from (x, y) setting z = 0.
    let ofXY x y : Point3D = { X = x; Y = y; Z = 0.0 }

    /// Create from (x, y, z).
    let ofXYZ x y z : Point3D = { X = x; Y = y; Z = z }

    /// Vector from a to b.
    let vectorTo (a: Point3D) (b: Point3D) : Vector3D =
        { Dx = b.X - a.X; Dy = b.Y - a.Y; Dz = b.Z - a.Z }

module Vector3D =
    let magnitude (v: Vector3D) : float =
        sqrt (v.Dx * v.Dx + v.Dy * v.Dy + v.Dz * v.Dz)

    let normalize (v: Vector3D) : Vector3D =
        let m = magnitude v
        if m < 1e-15 then { Dx = 1.0; Dy = 0.0; Dz = 0.0 }
        else { Dx = v.Dx / m; Dy = v.Dy / m; Dz = v.Dz / m }
