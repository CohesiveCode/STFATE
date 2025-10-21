module mod_enumerations
   use iso_c_binding
   implicit none

   enum, bind(c)
      enumerator :: red = 4
      enumerator :: blue = 9
      enumerator yellow
   end enum

contains

end module mod_enumerations
