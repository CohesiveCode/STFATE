module mod_geometry
   !! Module contains procedures for calculating simple geometric properties
   !! TODO: Move volume functions here
   use stdlib_kinds , only: dp
   use mod_constants, only: pi

   implicit none(type, external)
   private
   public:: calc_y_centroid_hemisphere, calc_y_centroid_spheriod, calc_oblate_spheriod_vol, calc_half_ellipsoid_vol
contains

   pure function calc_y_centroid_hemisphere(vert_radius) result(centroid)
      !! Calc the y-centroid of referenced to the the top of the hemisphere or half ellipsoid
      !! (Yep they are the same)
      real(dp), intent(in) :: vert_radius !! Vertical radius of the cloud
      real(dp) :: centroid                !! Centroid of the hemisphere or half ellipsoid

      ! Calculate the centroid of a hemisphere of radius vert_radius
      centroid = 3.0_dp/ 8.0_dp * vert_radius
   end function calc_y_centroid_hemisphere

   pure function calc_y_centroid_spheriod(vert_radius) result(centroid)
      !! Calc the y-centroid, referenced to the top of the spheriod
      real(dp), intent(in) :: vert_radius !! Vertical radius of the cloud
      real(dp)             :: centroid    !! Centroid of the spheriod
      centroid = vert_radius / 2.0_dp
   end function calc_y_centroid_spheriod

   pure function calc_oblate_spheriod_vol(vert_radius, horiz_radius) result(vol)
      !! Ref(1) - Eqn(3.22:Sub) - Calc the volume of an oblate spheriod
      real(dp), intent(in) :: vert_radius    !! Vertical radius
      real(dp), intent(in) :: horiz_radius   !! Horizontal radius
      real(dp)             :: vol            !! Volume of the cloud 

      vol = 4.0_dp / 3.0_dp * pi * vert_radius * horiz_radius**2
   end function calc_oblate_spheriod_vol

   elemental pure function calc_half_ellipsoid_vol(vert_radius, horiz_radius) result(vol)
      !! Ref(1) - Eqn(3.50:Sub) - Calc the volme of a half ellipsoid
      real(dp), intent(in) :: vert_radius  !! Vertical radius of the cloud
      real(dp), intent(in) :: horiz_radius !! Horizontal radius of the cloud
      real(dp)             :: vol          !! Volume of the cloud

      vol = 0.5_dp * calc_oblate_spheriod_vol(vert_radius, horiz_radius)
   end function calc_half_ellipsoid_vol

end module mod_geometry
