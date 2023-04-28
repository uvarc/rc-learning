module file_utils
  implicit none
!*******************************************************************************
  !
  ! This module manages variables related to files and their paths.
  !
  ! Author:  Katherine A. Holcomb
  ! It is Unix-oriented and could be improved by employing variable-length 
  ! strings.
!*******************************************************************************

  integer, parameter      :: MAX_PATH=256
  integer, parameter      :: MAX_LINE=256
  character(len=1)        :: separator='/'

  !Names of directories for input/output
  character(len=MAX_PATH) :: fqcwd

  integer, parameter      :: base_unit=10
  integer, parameter      :: invalid=0

contains

  subroutine get_cwd(fqcwd)
    character(len=*), intent(out) :: fqcwd
    ! May vary by compiler
    call getcwd(fqcwd)
  end subroutine get_cwd

  function unit_number()
     integer          :: unit_number
     integer          :: base_unit=10
     integer          :: iunit
     logical          :: first=.true.
     save                 

     if ( first ) then
        iunit=base_unit
        first=.false.
     else
        iunit=iunit+1
     endif
     unit_number=iunit

  end function unit_number

  function open_file(filename,mode)
  ! Opens the file filename if it can, returns a unit number for it.
     integer                                 :: open_file
     character(len=*), intent(in)            :: filename
     character(len=*), intent(in), optional  :: mode
     character(len=9)              :: fmode
     logical                       :: file_exists
     character(len=MAX_PATH)       :: fname
     integer                       :: i, ios, iunit
     integer, dimension(MAX_PATH) :: farray

     if ( present(mode)) then
        if ( mode == 'r' .or. mode == 'R' ) then
           fmode='read'
        else if ( mode == 'w' .or. mode == 'W' ) then
           fmode='write'
        else if ( mode == 'rw' .or. mode == 'RW'                               &
             .or. mode == 'wr' .or. mode == 'WR' ) then
           fmode='readwrite'
        else
           fmode='readwrite'
        endif
     else
        fmode='readwrite'
     endif

     fname=trim(adjustl(filename))

     if ( fmode == 'read' .or. fmode == 'readwrite') then
     ! File needs to exist
     !This is the best I could figure out to handle uninitialized filename
     !errors (which probably contain garbage characters so they aren't blank)
        farray=0
        do i=1,len_trim(fname)
           farray(i)=ichar(fname(i:i))
        enddo
        if (any(farray > 127)) then
           iunit=invalid
        endif
     endif
     
     inquire(file=fname,exist=file_exists)

     if ( file_exists .and. fmode == 'write' ) then
        open_file=invalid
     else if ( .not. file_exists .and. fmode == 'read' ) then
        iunit=invalid
     else
        iunit=unit_number()
        open(iunit,file=fname,action=fmode,iostat=ios)
        if ( ios .ne. 0 ) then
           iunit=invalid
        endif
     endif

     open_file=iunit

  end function open_file

  
  function count_records(funit,nheaders)
  ! Counts the number of lines, not including the first nheaders lines.
    integer                       :: count_records
    integer, intent(in)           :: funit
    integer, intent(in)           :: nheaders

    character(len=MAX_LINE)       :: line
    integer                       :: ios, lpcount

    lpcount = 0

    do
      read(funit,*,end=10,iostat=ios) line
         if (ios==0) then
         ! success, increment counter
            lpcount = lpcount + 1
         else
            stop "Unable to read file"
         endif
    end do
10  continue

    count_records = lpcount-nheaders

    rewind(funit)

  end function count_records

end module file_utils
