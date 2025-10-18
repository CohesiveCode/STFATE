! =============================================================================
! module:      model_parameters_mod
! purpose:     manages a set of model coefficients.
! description: this module defines the type for the coefficients and provides
!              a "set-once" mechanism. once initialized, the master copy of
!              the coefficients inside this module cannot be modified.
! =============================================================================
module model_parameters_mod
   use stdlib_kinds, only: dp
   use stdlib_io

   implicit none(type, external)


   ! everything is private unless explicitly declared public
   private
   ! the derived type is made public so programs can declare variables of this type.
   public :: t_model_coeffs
   public :: initialize_coeffs
   public :: get_coeffs

   ! definition of the derived type for the coefficients
   type :: t_model_coeffs
      real(kind=dp) :: dincr1, dincr2, alphao, beta, cm, cd, cdrag, gama
      real(kind=dp) :: cfric, cd3, cd4, alphac, frictn, f1, alamda, akyo
   end type t_model_coeffs

   ! --- private module data ---
   ! this is the single, master instance of the coefficients. it is private
   ! and cannot be accessed directly from outside the module.
   type(t_model_coeffs) :: locked_coeffs

   ! this logical flag acts as the lock. once true, values cannot be changed.
   logical :: are_coeffs_locked = .false.

contains

   ! =============================================================================
   ! initializes the master coefficients with defaults, allows for overrides,
   ! and then locks them from further changes.
   ! =============================================================================

   subroutine read_coeffs_from_file(filename)
      character(len=*), intent(in) :: filename

      ! Local variables
      type(t_model_coeffs) :: loc_coeffs
      integer :: iunit, ios
      character(len=100) :: line
      character(len=20) :: param_name
      real(kind=dp) :: param_value

      ! Open the file for reading
      open(newunit=iunit, file=filename, status='old', action='read', iostat=ios)
      if (ios /= 0) then
         write(*,*) 'Error opening file: ', filename
         stop
      end if

      ! Read each line and parse parameter name and value
      do
         read(iunit, '(A)', iostat=ios) line
         if (ios /= 0) exit  ! Exit on end of file or error

         read(line, '(A20, F10.5)', iostat=ios) param_name, param_value
         if (ios /= 0) cycle  ! Skip lines that don't match the format

         select case (trim(adjustl(param_name)))
          case ('dincr1')
            loc_coeffs%dincr1 = param_value
          case ('dincr2')
            loc_coeffs%dincr2 = param_value
          case ('alphao')
            loc_coeffs%alphao = param_value
          case ('beta')
            loc_coeffs%beta = param_value
          case ('cm')
            loc_coeffs%cm = param_value
          case ('cd')
            loc_coeffs%cd = param_value
          case ('cdrag')
            loc_coeffs%cdrag = param_value
          case ('gama')
            loc_coeffs%gama = param_value
          case ('cfric')
            loc_coeffs%cfric = param_value
          case ('cd3')
            loc_coeffs%cd3 = param_value
          case ('cd4')
            loc_coeffs%cd4 = param_value
          case ('alphac')
            loc_coeffs%alphac = param_value
          case ('frictn')
            loc_coeffs%frictn = param_value
          case ('f1')
            loc_coeffs%f1 = param_value
          case ('alamda')
            loc_coeffs%alamda = param_value
          case ('akyo')
            loc_coeffs%akyo = param_value
          case default
            write(*,*) 'Warning: Unknown parameter name: ', trim(param_name)
         end select
        
         ! Stor the coefficients and lock them
         call initialize_coeffs(loc_coeffs)
      end do
   end subroutine read_coeffs_from_file

   subroutine initialize_coeffs(model_coeffs)
      ! optional arguments allow the user to provide specific values at startup.
      ! add more optional arguments as needed for other coefficients.
      type(t_model_coeffs), intent(in), optional :: model_coeffs

      ! Local variables
      type(local_model_coeffs) :: local_coeffs


      if (present(model_coeffs)) then
         locked_coeffs = model_coeffs
      else
         ! set the default values
         locked_coeffs%dincr1  = 1.0_dp
         locked_coeffs%dincr2  = 1.0_dp
         locked_coeffs%alphao  = 0.235_dp
         locked_coeffs%beta    = 0.0_dp
         locked_coeffs%cm      = 1.0_dp
         locked_coeffs%cd      = 0.5_dp
         locked_coeffs%cdrag   = 1.0_dp
         locked_coeffs%gama    = 0.25_dp
         locked_coeffs%cfric   = 0.01_dp
         locked_coeffs%cd3     = 0.1_dp
         locked_coeffs%cd4     = 1.0_dp
         locked_coeffs%alphac  = 0.001_dp
         locked_coeffs%frictn  = 0.01_dp
         locked_coeffs%f1      = 0.1_dp
         locked_coeffs%alamda  = 0.005_dp
         locked_coeffs%akyo    = 0.05_dp
      end if
      ! now, set the lock. this is the crucial step.
      are_coeffs_locked = .true.

   end subroutine initialize_coeffs

   subroutine get_coeffs(coeffs_out)
      !! Returns a safe copy of the locked coefficients.
      type(t_model_coeffs), intent(out) :: coeffs_out

      ! ensure coefficients have been initialized before trying to retrieve them.
      if (.not. are_coeffs_locked) then
         error stop "ERROR: Attempted to get coefficients before they were initialized."
      end if

      ! assign the private, master copy to the output variable.
      coeffs_out = locked_coeffs

   end subroutine get_coeffs

end module model_parameters_mod
