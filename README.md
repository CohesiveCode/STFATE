# STFATE

Implementation of the Short-Term Fate (STFATE) model for simulating dredged material transport and fate in estuarine / coastal aquatic environments.

```
                                                 z , w
                                             ╱
                                         ╱
                                     ╱
    ────────────────●──────────────────→ x , u
                                    │
                    g       │
                    ↓       │
                                    │_   ┌ b(t) ┐
                                    │|   \  cloud  /  | U(t)
                         a(t) │|    \_______/    ↓
                                    │-
                                    ↓ y , v
```

## Overview
STFATE models the near-field evolution of a discharged dredged material cloud immediately after release. It tracks cloud geometry, particle concentrations, tracer mixing, and interaction with ambient flow fields. This repository is an early-stage modern Fortran reimplementation emphasizing clarity, modular design, and testability using fpm (Fortran Package Manager).

## Key Features (Current / Emerging)
- Cloud representation: `t_dump_des_cloud` type encapsulating geometry, kinematics, particle inventory, tracer concentration.
- Domain bounds management: `t_bounds` type for spatial extents (x,y,z) with initialization utilities.
- Model coefficients: `t_model_coeffs` with presets (e.g. `set_tetra_tech()` placeholder) for empirical relationships.
- Physical constants: Gravity, π, viscosity, and limits in `mod_constants`.
- Planned physics: Entrainment, apparent mass, drag, buoyancy, vorticity, particle settling, multi-cloud interactions.
- Early integration with `stdlib` for kinds, constants (path-based dependency during development).

## Project Status
This is a work in progress. Many algorithms (interpolation, full time stepping, ambient field coupling, multi-cloud handling, boundary interaction) are still TODOs noted in `app/main.f90`. Contributions focused on incremental physical process implementation and validation are welcome.

## Repository Structure
```
app/              Main program (`program STFATE`) – prototype driver
src/              Core modules (bounds, cloud, particles, model_coeffs, search, utils, etc.)
example/          Demo or experimental programs
old/              Legacy / original Fortran 77 & early F90 sources for reference
dmf-f77/          Notes & older DMF subroutines (historical context)
test/             Test-drive based test harness (WIP)
Notes/            Design and derivation notes
fortran_conversion_plan.md  Migration / refactor planning
fpm.toml          fpm package manifest
LICENSE           MIT License
```

## Building
Requires a modern Fortran compiler (e.g. `gfortran >= 10`) and `fpm`.

Install `fpm` if needed:
```bash
curl -fsSL https://raw.githubusercontent.com/fortran-lang/fpm/master/install.sh | bash
```

Clone stdlib (development path dependency) if not already available:
```bash
git clone https://github.com/fortran-lang/stdlib.git ../stdlib
```
Adjust the relative path in `fpm.toml` if your layout differs.

Build the project:
```bash
fpm build
```

## Running
Execute the prototype main program:
```bash
fpm run
```
Current output prints initialized cloud, particle subset, and model coefficients. As physics is added, runtime arguments (e.g., configuration files for ambient fields or discharge parameters) may be introduced.

## Testing
Test infrastructure uses `test-drive` (declared under `[dev-dependencies]`). Add test sources in `test/` and run:
```bash
fpm test
```
At present tests are minimal or placeholder – contributions adding unit tests for each module (initialization, bounds checking, coefficient selection) are encouraged.

## Module Snapshot
- `mod_bounds`: Domain extents, initialization, (future) spatial queries.
- `mod_cloud`: Cloud state (geometry, velocity, particles, tracer). Expansion planned for time-stepping & conservation checks.
- `mod_model_coeffs`: Empirical / calibration constants (drag, entrainment, settling factors).
- `mod_constants`: Physical constants and limits.
- `mod_particles`: Particle population properties & concentration tracking.
- `mod_search`: Utilities (e.g. binary search) to support interpolation and indexing.
- `mod_utils`: General helpers (I/O, safety checks, printing).
- Additional modules (`dump_*`, interpolation helpers) will integrate into the main evolution loop.

## Roadmap (Indicative)
1. Implement instantaneous descent / initial cloud formation physics.
2. Add ambient scalar & vector field interpolation (Fitpack / custom routines).
3. Introduce time-stepping loop with adaptive Δt and event hooks (bottom contact, surface proximity).
4. Multi-cloud interactions & merging / splitting criteria.
5. Output formats: structured NetCDF / CSV summaries and diagnostic logs.
6. Validation against legacy Fortran 77 implementations & published benchmark cases.

## Contributing
Contributions are welcome. Suggested workflow:
1. Fork & create feature branch.
2. Add or update tests in `test/` when modifying physics or data structures.
3. Ensure `fpm build` & `fpm test` pass locally.
4. Submit a concise PR describing motivation and scientific basis (if new model terms).

Please keep code modular, avoid implicit typing (enforced in `fpm.toml`), and prefer descriptive variable names. Early clarity outweighs premature micro-optimizations.

## License
MIT – see `LICENSE`.

## Acknowledgments
Foundational theory from: "Development of Models for Prediction of Short-term Fate of Dredged Material Discharged in the Estuarine Environment" and related technical documents. Parameter sets (e.g., Tetra Tech coefficients) referenced for backward compatibility.

## Disclaimer
This reimplementation is under active development; numerical outputs should not yet be used for regulatory or engineering decisions without independent verification.

## Development Notes / Original Plan (Preserved)
Historical planning notes retained in `fortran_conversion_plan.md` & legacy sources under `old/` for reference. Original high-level guidance: start simple (single cloud, no ambient flow), layer complexity iteratively.

