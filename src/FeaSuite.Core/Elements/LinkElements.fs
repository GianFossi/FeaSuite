namespace FeaSuite.Core

// ---------------------------------------------------------------------------
// Link element sub-types
// ---------------------------------------------------------------------------

/// Link element sub-types.
type LinkElement =
    | Link11  /// ANSYS LINK11  – Structural 3D Linear Actuator; 2 nodes; DOF: UX, UY, UZ
    | Link31  /// ANSYS LINK31  – Radiation Link; 2 nodes; DOF: TEMP
    | Link33  /// ANSYS LINK33  – Thermal 3D Conduction Bar; 2 or 3 nodes; DOF: TEMP
    | Link34  /// ANSYS LINK34  – Convection Link; 2 nodes; DOF: TEMP
    | Link68  /// ANSYS LINK68  – Coupled Thermal-Electric Line; 2 nodes; DOF: TEMP, VOLT
    | Link180 /// ANSYS LINK180 – Structural 3D Spar (or Truss); 2 nodes; DOF: UX, UY, UZ
    | Link228 /// ANSYS LINK228 – 3D Coupled-Field Link; 2 or 3 nodes; DOF: UX, UY, UZ, TEMP, VOLT
