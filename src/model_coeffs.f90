module mod_model_coeffs
   use stdlib_kinds, only: dp
   use mod_set_default, only: set_default
   use stdlib_strings, only: join
   use stdlib_string_type
   use stdlib_strings

   implicit none(type, external)
   private

   type, public :: t_model_coeffs
      type(string_type) :: name   ! Name of the model coefficients
      real(dp) :: dincr1          ! Factor used for estimating time step in convective phase
      real(dp) :: dincr2          ! Factor used for estimating time step in dynamic collapse
      real(dp) :: alpha0          ! Entrainment coefficient for turbulent thermal
      real(dp) :: beta            ! Settling coeff
      real(dp) :: mass_coeff      ! Apparent mass coefficient
      real(dp) :: drag_sphere     ! Drag coefficient for a sphere
      real(dp) :: gama            ! Density gradient in the cloud
      real(dp) :: drag_oblate     ! Drag coefficient for a collapsing spheriod
      real(dp) :: fric_oblate     ! Skin friction coefficient for the quadrant of a collapsing spheriod
      real(dp) :: drag_ellip      ! Drag coefficient for an ellipsoidal wedge
      real(dp) :: drag_plate      ! Drag coefficient for a plate
      real(dp) :: alphac          ! Entrainment coefficient for collapse
      real(dp) :: frictn          ! Friction coefficient between cloud and estuary bottom
      ! Mod. factor used on computing the resistance of the friction force to the collapse of a quadrant of an oblate spheriod
      real(dp) :: f1
      real(dp) :: alambda         ! Diss. factor used in computing horiz. diffusion
      real(dp) :: akyo            ! Max. value of vertical diffusion coeff.
      !   real(dp), private :: nu              ! Viscosity coefficient
   contains
      procedure, public, pass :: print => print_params
      procedure, public, pass :: set_tetra_tech => set_tetra_tech_coeffs
      procedure, public, pass :: set => set_coeffs
   end type

contains


   subroutine set_tetra_tech_coeffs(self)
      !! Set the coefficients for the tetra tech coefficients
      class(t_model_coeffs), intent(inout) :: self
      type(string_type) :: name
      name = "Tetra Tech"
      call self%set(               &
         name           = name,    &
         dincr1         = 1.0_dp,  &
         dincr2         = 1.0_dp,  &
         alpha0         = 0.235_dp,&
         beta           = 0.0_dp,  &
         mass_coeff     = 1.0_dp,  &
         drag_sphere    = 0.5_dp,  &
         gama           = 1.0_dp,  &
         drag_oblate    = 0.25_dp, &
         fric_oblate    = 0.01_dp, &
         drag_ellip     = 0.1_dp,  &
         drag_plate     = 1.0_dp,  &
         alphac         = 1e-3_dp, &
         frictn         = 1e-2_dp, &
         f1             = 0.1_dp,  &
         alambda        = 5e-3_dp, &
         akyo           = 0.05_dp  &
         )

   end subroutine set_tetra_tech_coeffs

   subroutine print_params(self)
      ! This updated version provides better alignment and more robust formatting.
      class(t_model_coeffs), intent(in) :: self

      ! Local variables
      type(string_type) :: header, footer

      character(len=*), parameter :: FMT = '(A16, " : ", T19, F12.5)'

      header = "--- Model Coefficients: "//trim(self%name) // " ---"

      footer = repeat("-", len_trim(header))

      ! Add blank lines for better separation from other program output
      print *, ""
      print *, header

      ! 3. Print all parameters using the new, consistent format
      print FMT, "dincr1",            self%dincr1
      print FMT, "dincr2",            self%dincr2
      print FMT, "alpha0",            self%alpha0
      print FMT, "beta",              self%beta
      print FMT, "Mass Coeff.",       self%mass_coeff
      print FMT, "Drag Coeff-Sphere", self%drag_sphere
      print FMT, "Density Grad.",     self%gama
      print FMT, "Drag Coeff-Oblate", self%drag_oblate
      print FMT, "Fric Oblate",       self%fric_oblate
      print FMT, "Drag Coeff-Ellip",  self%drag_ellip
      print FMT, "Drag Coeff-Plate",  self%drag_plate
      print FMT, "alphac",            self%alphac
      print FMT, "frictn",            self%frictn
      print FMT, "f1",                self%f1
      print FMT, "alambda",           self%alambda
      print FMT, "akyo",              self%akyo

      print *, footer
      print *, ""

   end subroutine print_params


   subroutine set_coeffs(self, &
      name, &
      dincr1, &
      dincr2  , &
      alpha0  , &
      beta  , &
      mass_coeff  ,   &
      drag_sphere  , &
      gama  , &
      drag_oblate  , &
      fric_oblate  , &
      drag_ellip  , &
      drag_plate  , &
      alphac  , &
      frictn  , &
      f1  , &
      alambda  , &
      akyo &
      )
      
      class(t_model_coeffs), intent(inout) :: self
      type(string_type), intent(in) :: name            ! Name of the model coefficients
      real(dp), intent(in):: dincr1       ! Not sure what this is
      real(dp), intent(in):: dincr2       ! Not sure what this is
      real(dp), intent(in):: alpha0       ! Forgot what this is
      real(dp), intent(in):: beta         !
      real(dp), intent(in):: mass_coeff   ! Apparent mass coefficient
      real(dp), intent(in):: drag_sphere           ! Not sure what this is
      real(dp), intent(in):: gama   ! Check - Drag coefficient
      real(dp), intent(in):: drag_oblate         !
      real(dp), intent(in):: fric_oblate   ! Friction coeffient
      real(dp), intent(in):: drag_ellip          !
      real(dp), intent(in):: drag_plate          !
      real(dp), intent(in):: alphac       !
      real(dp), intent(in):: frictn       !
      real(dp), intent(in):: f1           !
      real(dp), intent(in):: alambda      !
      real(dp), intent(in):: akyo         !

      self%name         = name
      self%dincr1       = dincr1
      self%dincr2       = dincr2
      self%alpha0       = alpha0
      self%beta         = beta
      self%mass_coeff   = mass_coeff
      self%drag_sphere  = drag_sphere
      self%gama         = gama
      self%drag_oblate  = drag_oblate
      self%fric_oblate  = fric_oblate
      self%drag_ellip   = drag_ellip
      self%drag_plate   = drag_plate
      self%alphac       = alphac
      self%frictn       = frictn
      self%f1           = f1
      self%alambda      = alambda
      self%akyo         = akyo

   end subroutine set_coeffs

end module mod_model_coeffs

