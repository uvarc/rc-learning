! unicode.f90
!
! From https://cyber.dabamos.de/programming/modernfortran/unicode.html
!
! Modern Fortran language standards have no intrinsic support for Unicode I/O.
! To bypass this limitation, the Universal Coded Character Set defined in ISO 10646
! can be used instead, which is mostly identical to UTF-32.
! All code points of the Basic Multilingual Plane can be accessed,
! while the Supplementary Multilingual Plane is not supported.
!
! $ sudo apt-get install gfortran
! $ f95 -fbackslash -o unicode unicode.f90 && ./unicode

program unicode
    use, intrinsic :: iso_fortran_env, only: output_unit
    implicit none
    integer, parameter :: u = selected_char_kind('ISO_10646')
    character(kind=u, len=:), allocatable :: string

    ! 𝌆 = U+1D306
    ! UTF-8:
    string = u_'\uf09d\u8c86'

    open (output_unit, encoding='utf-8')
    print '(a)',string
    print*,'len is',LEN(string)
end program unicode
