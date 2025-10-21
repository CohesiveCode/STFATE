module mod_enum
    
    implicit none(type, external)
    private
    public :: cloud_phase_enum

    type, private :: t_cloud_phase_enum
        !! Type for storing the phase the cloud is current 
        integer :: DESCENT         = 1
        integer :: WATER_COLLAPSE  = 2
        integer :: BOTTOM_COLLAPSE = 3
    end type t_cloud_phase_enum

    

    
contains

end module mod_enum