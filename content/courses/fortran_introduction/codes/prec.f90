module precisions
use ISO_FORTRAN_ENV

! Single precision
integer, parameter   :: sp = selected_real_kind(real32)

! Double precision
integer, parameter   :: dp = selected_real_kind(real64)

end module 
