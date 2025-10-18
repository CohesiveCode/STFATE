module mod_utils
   use stdlib_kinds, only: dp
   use stdlib_string_type
   use stdlib_strings

   implicit none(type, external)
   private
   public :: alloc_message

   interface alloc_message
      module procedure alloc_char_message_dp
      module procedure alloc_string_message_dp
   end interface alloc_message

contains

   pure function alloc_char_message_dp(dp_array, ext_message) result(stat)
      real(dp), dimension(..), allocatable, intent(in):: dp_array
      character(len=*), intent(in) :: ext_message

      logical :: stat
      type(string_type) :: message
      message = ext_message//":Array is not allocated"

      if (allocated(dp_array)) then
         stat = .true.
      else
         error stop char(message)
      end if

   end function alloc_char_message_dp

   pure function alloc_string_message_dp(dp_array, ext_message) result(stat)
      real(dp), dimension(..), allocatable, intent(in):: dp_array
      type(string_type), intent(in) :: ext_message

      logical :: stat
      type(string_type) :: message
      message = ext_message//":Array is not allocated"

      if (allocated(dp_array)) then
         stat = .true.
      else
         error stop char(message)
      end if

   end function alloc_string_message_dp
end module mod_utils
