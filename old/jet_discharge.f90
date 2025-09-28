!=======================================================================
!   FORTRAN CODE CONVERTED FROM F77 TO MODERN FORTRAN
!   SOURCE: APPENDIX D, Listing of Fortran Program for Modeling Fixed or
!           Moving Jet Discharge of Dredged Material
!=======================================================================

! Module for dimensions and array sizing
module dimensions_mod
    implicit none
    integer :: NS, NSP1, NVL
end module dimensions_mod

! Module for bay/domain geometry parameters
module bay_geometry_mod
    implicit none
    real :: DX, DTL, XBARGE, ZBARGE, DXH, DXR, AREA
end module bay_geometry_mod

! Module for ambient conditions
module ambient_conditions_mod
    implicit none
    integer :: NROA, IY
    real :: Y(8), ROA(8), H
end module ambient_conditions_mod

! Module for simulation control and guidance parameters
module simulation_control_mod
    implicit none
    real :: TJET, TSTOP
    integer :: ISTEP, IPLUNG, NUTRL, NTRIAL, ILEAVE, KEY1, KEY2, KEY3
end module simulation_control_mod

! Module for additional guidance parameters
module guidance_mod
    implicit none
    integer :: NIND
    integer :: NLINE(150), MF(150), ML(150)
end module guidance_mod

! Module for printing control
module print_control_mod
    implicit none
    logical :: PRT
end module print_control_mod

! Module for cloud/particle tracking arrays
module cloud_tracking_mod
    implicit none
    real :: T(600), CX(600), CY(600), CZ(600), CU(600), DENDIF(600)
    real :: BC(600), AA(600), FC(600), VF
end module cloud_tracking_mod

! Module for jet properties
module jet_properties_mod
    implicit none
    real :: DL(600), THETA2(600), S(600)
    real :: UB, UBX, WBZ, SAI, V2, DLDT, DJET, ROI, ROO, VDOT
end module jet_properties_mod

! Module for particle/sediment properties
module particle_properties_mod
    implicit none
    real :: PARAM(13), RDAS(13), CS(13), VFALL(13), VOIDS(13), BVOID
    real :: ROAS(13)  ! Alternative name used in some places
end module particle_properties_mod

! Module for graphics/plotting parameters
module graphics_mod
    implicit none
    integer :: IGCN, IGCL, IGLT, IPCN, IPCL, IPLT
end module graphics_mod

! Module for tracking lost material
module lost_material_mod
    implicit none
    real :: GONE
end module lost_material_mod

! Module for bottom position
module bottom_position_mod
    implicit none
    real :: XBO, ZBO
end module bottom_position_mod

! Module for switches and flags
module switches_mod
    implicit none
    integer :: ITF
end module switches_mod

! Module for discretization passes
module discretization_mod
    implicit none
    integer :: NPASS, MPASS
end module discretization_mod

! Module for tracer and concentration tracking
module tracer_mod
    implicit none
    integer :: ITD
    real :: TD(6), DC(6), TRACER, CINIT, CBACK
end module tracer_mod

! Module for coefficients - short term
module short_term_coeff_mod
    implicit none
    real :: ALPHA, ALPHAC, ALPHAO, ALPHA1, ALPHA2, ALPHA3, ALPHA4
    real :: BETA1, BETA2, BETA3, BETAC, BETAR, GAMMA
end module short_term_coeff_mod

! Module for coefficients - long term
module long_term_coeff_mod
    implicit none
    real :: ALAMDA, DIF, AKYO
end module long_term_coeff_mod

! Module for mathematical constants
module constants_mod
    implicit none
    real :: G, PI
end module constants_mod

! Module for velocity specifications
module velocity_specs_mod
    implicit none
    integer :: IFORM
    real :: DU1, DU2, UU1, UU2, DW1, DW2, WW1, WW2, DL1, DL2
end module velocity_specs_mod

! Module for initialization data
module initialization_mod
    implicit none
    real :: SAVE(22), CF
end module initialization_mod

! Module for profile data
module profile_mod
    implicit none
    integer :: NPROF
    real :: DTROA
    real :: YROA(8,50), RHOA(8,50)
end module profile_mod

! Module for cloud storage counters
module cloud_storage_mod
    implicit none
    integer :: NTCLD(13)
end module cloud_storage_mod

! Module for computational arrays
module computational_arrays_mod
    implicit none
    real :: E(22), EP(22)
end module computational_arrays_mod

! Module for entrainment control
module entrainment_mod
    implicit none
    integer :: NOTRN
end module entrainment_mod

! Module for collapse parameters
module collapse_mod
    implicit none
    real :: AO, FBED
    integer :: IBED
end module collapse_mod

! Module for step size control
module step_control_mod
    implicit none
    real :: DS, DLF
end module step_control_mod

! Module for point tracking
module point_tracking_mod
    implicit none
    integer :: MST, NST
end module point_tracking_mod

! Module for numerical counters
module numerical_counters_mod
    implicit none
    integer :: NCT
end module numerical_counters_mod

! Module for correlation parameters
module correlation_mod
    implicit none
    real :: CM, CMAX
end module correlation_mod

! Module for old jet properties
module old_jet_mod
    implicit none
    real :: ODTC, OCSIDE, ODBX, ODBZ, OCTOP, OT
    real :: OCTHK(13), OTBF(13)
end module old_jet_mod

program dmfj
    use dimensions_mod
    implicit none
    
    ! Local variables
    integer :: NMAX, MMAX, NSC, NVL_LOCAL
    integer :: LDIM, N1, N2, N3, N4, N5, N6, N7, N8, N9, N10
    integer :: N11, N12, N13, N14, N15, N16, N17, N18, N19, N20, NEED
    real, allocatable :: A(:)
    
    ! THIS IS DUMMY MAIN PROGRAM TO SET BLANK COMMON STORAGE POINTERS
    ! FOR LONG TERM ARRAYS, SOLIDS ARRAY AND SMALL CLOUDS ARRAYS
    
    rewind(7)
    read(5, '(16I5)') NMAX, MMAX, NS, NVL_LOCAL, NSC
    
    ! NMAX-LONG TERM ARRAY DIMENSION IN Z-DIRECTION
    ! MMAX-LONG TERM ARRAY DIMENSION IN X-DIRECTION
    ! NS-NUMBER OF SOLID FRACTIONS
    ! NVL-NUMBER OF VELOCITY PLANES
    ! NSC-MAXIMUM NUMBER OF SMALL CLOUDS OF ONE COMPONENT ALLOWED
    
    ! SET ARRAY POINTERS
    NSP1 = NS + 1
    NVL = NVL_LOCAL
    LDIM = NMAX * MMAX
    N1 = 1                    ! X
    N2 = N1 + LDIM           ! Z
    N3 = N2 + LDIM           ! DEPTH
    N4 = N3 + LDIM           ! ICODE
    N5 = N4 + LDIM           ! CP
    N6 = N5 + LDIM           ! THICKP
    N7 = N6 + LDIM           ! TOPP
    N8 = N7 + LDIM           ! C
    N9 = N8 + LDIM*NSP1      ! THICK
    N10 = N9 + LDIM*NSP1     ! TOP
    N11 = N10 + LDIM*NSP1    ! ACCUM
    N12 = N11 + LDIM*NS      ! U
    N13 = N12 + LDIM*NVL     ! W
    N14 = N13 + LDIM*NVL     ! SS
    N15 = N14 + 600*NS       ! TSIDE
    N16 = N15 + NSC*NSP1     ! TTHK
    N17 = N16 + NSC*NSP1     ! TTOP
    N18 = N17 + NSC*NSP1     ! TMASS
    N19 = N18 + NSC*NSP1     ! TX
    N20 = N19 + NSC*NSP1     ! TZ
    NEED = N20 + NSC*NSP1
    
    ! Allocate the main array
    allocate(A(NEED))
    
    ! FIND PRESENT FIELD LENGTH, ADD LENGTH OF ARRAYS (NEED) AND
    ! REQUEST NEW FIELD LENGTH
    ! CDC ONLY - commented out as not needed for gfortran
    ! LENF = MEMGET(65B)+1
    ! NEWLEN = LENF + NEED
    
    write(6, '(/////10X,A/10X,A,1X,A,3X,A,2X,A,2X,A,4X,A/10X,I3,2X,I3,4X,I2,1X,I3,2X,I3,2X,I6)') &
        'STORAGE ALLOCATION PARAMETERS FOLLOW...', &
        'NMAX', 'MMAX', 'NS', 'NVL', 'NSC', 'NEED', &
        NMAX, MMAX, NS, NVL, NSC, NEED
        
    ! CDC ONLY - not needed for gfortran
    ! CALL XRFL (NEWLEN)
    
    ! NOTE: There are typos in the following CALL statement as printed in the source.
    !       A(NS) should probably be A(N5). A(NB) should probably be A(N8).
    !       The argument list does not match the SUBROUTINE MAIN definition.
    call main_sub(A(N1), A(N2), A(N3), A(N4), A(N5), A(N6), A(N7), A(N8), &
                  A(N9), A(N10), A(N11), A(N12), A(N13), A(N14), A(N15), A(N16), &
                  A(N17), A(N18), A(N19), A(N20), NMAX, MMAX, NSC)
    
    deallocate(A)
    
end program dmfj

!=======================================================================
!     JET MODEL
!=======================================================================

subroutine main_sub(X, Z, DEPTH, ICODE, CP, COUT, THICKP, TOPP, C, THICK, &
                    TOP, ACCUM, U, W, SS, TSIDE, TTHK, TTOP, TMASS, TX, TZ, &
                    NMAX, MMAX, NSC)
    use dimensions_mod
    use bay_geometry_mod
    use ambient_conditions_mod
    use simulation_control_mod
    use print_control_mod
    use cloud_tracking_mod
    use jet_properties_mod
    use particle_properties_mod
    use graphics_mod
    use lost_material_mod
    use bottom_position_mod
    use switches_mod
    use discretization_mod
    use tracer_mod
    
    implicit none
    
    ! Arguments
    integer, intent(in) :: NMAX, MMAX, NSC
    real, intent(inout) :: X(NMAX, MMAX), Z(NMAX, MMAX), DEPTH(NMAX, MMAX)
    integer, intent(inout) :: ICODE(NMAX, MMAX)
    real, intent(inout) :: CP(NMAX, *), THICKP(NMAX, *), TOPP(NMAX, *)
    real, intent(inout) :: COUT(NMAX, MMAX)
    real, intent(inout) :: C(NMAX, MMAX, *), THICK(NMAX, MMAX, *), TOP(NMAX, MMAX, *)
    real, intent(inout) :: ACCUM(NMAX, MMAX, *)
    real, intent(inout) :: TSIDE(NSC, *), TTHK(NSC, *), TTOP(NSC, *)
    real, intent(inout) :: TMASS(NSC, *), TX(NSC, *), TZ(NSC, *)
    real, intent(inout) :: U(*), W(*), SS(*)
    
    ! Local variables - need to identify types from usage
    character(8) :: ID(8)
    real :: TPRT(12)
    ! ... more variables to be added as we convert
    
    ! This is a stub - the actual conversion will continue in the next steps
    
end subroutine main_sub
