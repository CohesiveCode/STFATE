# Subroutines in dmf.f

This document outlines the subroutines found in the `dmf.f` file, their purpose, and the other subroutines they call.

### `PROGRAM DMF`
*   **Purpose**: This is the main program entry point. It appears to be a dummy program primarily used for setting up memory pointers for blank common storage blocks, a common practice in older Fortran for dynamic memory allocation.
*   **Calls**: `MAIN`, `EXIT`. (Also non-standard `MEMGET`, `XRFL` which seem specific to a CDC environment).

### `SUBROUTINE MAIN`
*   **Purpose**: This is the primary driver subroutine that controls the simulation flow. It reads execution parameters, defines the simulation time, and calls the main phases of the simulation: convective descent (`DUMP`), dynamic collapse (`COLAPS`), and long-term diffusion (`MAD`). It also handles the overall simulation loop and printing of final results.
*   **Calls**: `SECOND`, `ESTGEO`, `AMBC`, `UW`, `DUMP`, `COLAPS`, `BOOKS`, `MAD`, `PRINTC`, `EXIT`.

### `SUBROUTINE ESTGEO`
*   **Purpose**: Defines the estuary geometry. It reads grid spacing and depth data, and generates a coded array (`ICODE`) that identifies land, water, and boundary points.
*   **Calls**: `EXIT`.

### `SUBROUTINE AMBC`
*   **Purpose**: Reads and sets up the ambient environmental conditions. This includes the barge dump location, the ambient density profile of the water, and parameters for the velocity profiles.
*   **Calls**: `DINT`.

### `SUBROUTINE DUMP`
*   **Purpose**: Simulates the initial convective descent phase of the dredged material disposal. It models the three-dimensional, axi-symmetric instantaneous release from the barge.
*   **Calls**: `RUNGS` (with `DERIVD`), `DRAW`.

### `SUBROUTINE DERIVD`
*   **Purpose**: Calculates the derivatives of the governing equations for the convective descent phase (`DUMP`). It is passed as an argument to the `RUNGS` solver.
*   **Calls**: `VEL`, `EXIT`.

### `SUBROUTINE COLAPS`
*   **Purpose**: Simulates the dynamic collapse phase of the cloud after the initial descent. This occurs as the dense cloud interacts with the ambient stratification or the bottom.
*   **Calls**: `RUNGS` (with `DERIVC`), `BOTTOM`, `DRAW`.

### `SUBROUTINE DERIVC`
*   **Purpose**: Calculates the derivatives of the governing equations for the dynamic collapse phase (`COLAPS`). It is passed as an argument to the `RUNGS` solver.
*   **Calls**: `VEL`.

### `SUBROUTINE BOTTOM`
*   **Purpose**: Computes the dynamics of the cloud when it interacts with and collapses on the bottom of the estuary.
*   **Calls**: `RUNGS` (with `DERIVB`).

### `SUBROUTINE DERIVB`
*   **Purpose**: Calculates the derivatives of the governing equations for the bottom interaction phase (`BOTTOM`). It is passed as an argument to the `RUNGS` solver.
*   **Calls**: `VEL`, `EXIT`.

### `SUBROUTINE RUNGS`
*   **Purpose**: A fourth-order Runge-Kutta numerical integration routine. It is a general-purpose solver for a system of ordinary differential equations.
*   **Calls**: `DERIVE` (This is a formal parameter, which is `DERIVD`, `DERIVC`, or `DERIVB` in practice).

### `SUBROUTINE UW`
*   **Purpose**: Reads and provides the horizontal velocity field (U and W components) for the long-term simulation from an external file (unit 7).
*   **Calls**: None.

### `SUBROUTINE VEL`
*   **Purpose**: Interpolates the velocity field to find the horizontal velocities (UA, WA) at a specific point (XA, YA, ZA) in the domain. It can handle single or multi-layer velocity profiles.
*   **Calls**: `DINT`.

### `SUBROUTINE DRAW`
*   **Purpose**: A graphics routine to generate character-based plots of simulation results.
*   **Calls**: `NORM`, `SPLOT`.

### `SUBROUTINE SPLOT`
*   **Purpose**: The core printer plot routine that generates the character-based graph on the output.
*   **Calls**: `RANGE`, `PFIX`.

### `SUBROUTINE RANGE`
*   **Purpose**: A utility routine that finds the maximum and minimum values in an array.
*   **Calls**: None.

### `SUBROUTINE NORM`
*   **Purpose**: A utility routine that normalizes the values in an array to a specified range (e.g., for plotting).
*   **Calls**: `RANGE`.

### `SUBROUTINE PFIX`
*   **Purpose**: A utility routine to place an alphanumeric character into the correct position in the print buffer for plotting.
*   **Calls**: None (uses non-standard `DECODE`/`ENCODE`).

### `SUBROUTINE BOOKS`
*   **Purpose**: A "bookkeeping" routine that handles the mass transfer from the short-term model (convective descent and collapse) to the long-term diffusion model. It creates small clouds of material that have settled out of the main cloud.
*   **Calls**: `DINT`, `VEL`, `VDIFF`.

### `SUBROUTINE INJECT`
*   **Purpose**: This routine manages the small clouds created by `BOOKS`. It convects and diffuses them analytically. If a cloud grows large enough, this routine injects its mass into the main long-term grid.
*   **Calls**: `DINT`, `VEL`, `VDIFF`.

### `SUBROUTINE MAD`
*   **Purpose**: This routine "Moves And Diffuses" the material in the long-term grid. It calls routines to handle the transport of both the main grid and the small analytical clouds.
*   **Calls**: `ACAD` (likely `INJECT`), `TRNSPT`, `AVESPT`, `DINT`, `VDIFF`, `PRINTC`.

### `SUBROUTINE TRNSPT`
*   **Purpose**: Calculates the transport of a particle/grid point over one time step based on the local velocity.
*   **Calls**: `VEL`.

### `SUBROUTINE AVESPT`
*   **Purpose**: Averages concentrations in a 5-point star pattern for the diffusion calculation, with a diffusion limiting scheme.
*   **Calls**: None.

### `SUBROUTINE PRINTC`
*   **Purpose**: A formatted printing routine to output the contents of a 2D array (like concentration, thickness, etc.) to the screen in a gridded format.
*   **Calls**: None.
