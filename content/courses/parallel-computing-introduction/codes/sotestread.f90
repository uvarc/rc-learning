program testread

implicit none

integer :: ngrid
integer :: jd, kd, ld, nq, nqc

integer :: refmach, alpha, rey, time, gaminf
integer :: beta, tinf

integer :: j, k, l, n

integer, allocatable, dimension(:,:,:,:) :: q
character(len=64) :: filename

if (command_argument_count() < 1) then
    print *,'Usage: read [filename]'
else 
    call get_command_argument(1, filename)
    open(2, file=trim(filename), form='unformatted', convert='little_endian')
    read(2) NGRID
    read(2) JD, KD, LD, NQ, NQC
    read(2) REFMACH,ALPHA,REY,TIME,GAMINF,BETA,TINF

    allocate(q(jd, kd, ld, nq))
    read(2) ((((Q(J,K,L,N),J=1,JD),K=1,KD),L=1,LD),N=1,NQ)
    close(2)

    print *, 'Ngrid = ', ngrid
    print *, 'jd, kd, ld, nq, nqc = ', jd, kd, ld, nq, nqc

    print *, 'q: min/mean/max = ', minval(q), sum(q)/size(q), maxval(q)

    !write(*,*) ((((Q(J,K,L,N),J=1,JD),K=1,KD),L=1,LD),N=1,NQ)

    deallocate(q)
endif

end program testread

