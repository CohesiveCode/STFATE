# Fortran 77 to Modern Fortran Conversion Plan

## Project Overview
Converting `jet_discharge.f` (3,305 lines) from Fortran 77 to modern Fortran while maintaining the subroutine/function structure without introducing classes.

## Current Code Analysis

### File Structure
- **Main Program**: `DMFJ` (lines 7-70)
- **Primary Subroutine**: `MAIN` (lines 76-362)
- **Total Subroutines/Functions**: 25 units
- **File Size**: 3,305 lines

### Key F77 Characteristics to Convert
- **COMMON blocks**: ~165 occurrences
- **GOTO statements**: ~263 occurrences  
- **FORMAT statements**: ~189 numbered statements
- **Fixed-form source**: Traditional column-based format
- **No IMPLICIT NONE**: Relies on implicit typing
- **F77 continuation**: Uses column 6 for continuation

## Conversion Steps

### 1. Source Format Conversion
- Convert from fixed-form (`.f`) to free-form (`.f90`)
- Remove column restrictions (1-72 character limit)
- Update file extension to `.f90`

### 2. Add Explicit Typing
- Add `IMPLICIT NONE` to all program units
- Declare all variables explicitly with appropriate types
- Review and correct any implicit typing assumptions

### 3. Modernize COMMON Blocks
- Create modules for shared data instead of COMMON blocks
- Group related variables into logical modules
- Use `USE` statements to import module data
- Maintain global accessibility where needed

### 4. Update Format Statements
- Replace numbered FORMAT statements with inline formatting
- Convert to modern FORMAT syntax where beneficial
- Maintain readability of output formatting

### 5. Eliminate GOTO Statements
- Replace GOTO with structured constructs:
  - `DO` loops for iteration
  - `IF-THEN-ELSE` for conditional logic
  - `SELECT CASE` for multi-way branching
  - `EXIT` and `CYCLE` for loop control

### 6. Modernize Array Declarations
- Update to modern array syntax
- Use assumed-shape arrays where appropriate
- Maintain existing array bounds and indexing

### 7. Update Continuation Lines
- Replace column 6 continuation with `&` symbol
- Improve line breaking for readability
- Maintain logical code grouping

### 8. Modernize I/O Statements
- Update READ/WRITE statements to modern form
- Use unit numbers consistently
- Maintain existing file handling logic

### 9. Code Structure Preservation
- Keep existing subroutine/function organization
- Maintain current algorithm implementations
- Preserve computational logic and numerical methods
- Keep similar parameter passing patterns

### 10. Testing and Validation
- Compile with modern Fortran compiler
- Verify numerical results match original
- Test all execution paths
- Validate input/output consistency

## Implementation Notes

### What Will NOT Change
- Overall program structure and flow
- Computational algorithms and methods
- Subroutine/function interfaces (where possible)
- Input/output file formats
- Numerical precision and accuracy

### What Will Change
- Source code format and readability
- Variable declaration style
- Control flow structures
- Data sharing mechanisms
- Compiler compatibility

## Expected Benefits
- Improved code readability and maintainability
- Better compiler optimization opportunities
- Enhanced error checking and debugging
- Future extensibility and modification ease
- Compliance with modern Fortran standards

## Timeline Estimate
- **Phase 1**: Basic format conversion (source format, IMPLICIT NONE)
- **Phase 2**: Data structure modernization (COMMON to modules)
- **Phase 3**: Control flow updates (GOTO elimination)
- **Phase 4**: Final cleanup and testing

## Success Criteria
- Code compiles successfully with modern Fortran compiler
- All test cases produce identical numerical results
- Code maintains original functionality and performance
- Improved code readability and structure