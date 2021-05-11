program nfc
implicit none

   logical  :: packers_win, seahawks_win, saints_win
   logical  :: packers_lose, seahawks_lose, saints_lose
   logical  :: packers_advance=.false.
   logical  :: seahawks_advance=.false.
   logical  :: saints_advance=.false.

   packers_win=.false.; packers_lose=.true.
   seahawks_win=.true.; seahawks_lose=.false.
   saints_win=.false.; saints_lose=.false.

   if ( packers_win .or. ( .not. seahawks_win ) ) then
      packers_advance=.true.
   endif

   if ( seahawks_win ) then
       if ( packers_lose .and. .not. saints_win ) then
          seahawks_advance=.true.
       endif
    endif

    if ( saints_win ) then
       if ( packers_lose .and. seahawks_win ) then
          saints_advance=.true.
       endif
    endif

    if ( packers_advance ) then
        print *, "Packers advance!"
    else if ( seahawks_advance ) then
        print *, "Seahawks advance!"
    else if ( saints_advance ) then
        print *, "Saints advance!"
    else
        print *, "Error: no match to conditions."
    endif

end program nfc

