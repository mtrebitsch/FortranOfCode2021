program diagnostic2
  ! Puzzle 3:
  ! Find the life support rating, as "O2gen * CO2scrub"
  ! Where O2gen and CO2scrub are two binary numbers taken from the "log"
  ! For O2gen, it is the number where the 1st bit is the most common 1st bit,
  !   then the 2nd bit is the most common 2nd bit of these ones, etc.
  ! For CO2scrub, it is the opposite

  implicit none

  character(len=80) :: input_file
  character(len=80)  :: line
  integer(kind=4)   :: ilun, io, dummy
  logical           :: exists
  integer(kind=4)   :: i, n, nbits
  integer(kind=4), allocatable :: logfile(:,:), count_ones(:)
  logical, allocatable :: onemax(:)
  integer(kind=4)   :: o2gen, co2scrub

  input_file = "input_03.dat"
  ! Check that the file exists and count the lines
  call count_lines(input_file, n, nbits)

  allocate(logfile(1:n,1:nbits), count_ones(1:nbits), onemax(1:nbits))
  logfile(:,:) = 0

  ! Read the file first to count the number of lines
  open(newunit=ilun, file=input_file, status="old", action="read", iostat=io)
  do i=1,n
     read(ilun, *, iostat=io) line
     if (io /= 0) exit
     read(line,'(<nbits>I1)') logfile(i,:)
  end do
  close(ilun)

  call find_life(n, nbits, logfile, .true., o2gen)
  call find_life(n, nbits, logfile, .false., co2scrub)

  write(*,'(A40, I8)') 'O2 generator rating:', o2gen
  write(*,'(A40, I8)') 'CO2 scrubber rating:', co2scrub
  write(*,'(A40, I8)') 'Life rating:', o2gen*co2scrub

  


contains
  
  subroutine count_lines(fname, nlines, nbits)
    character(len=80), intent(in)               :: fname
    integer(kind=4), intent(out)                :: nlines, nbits
    integer(kind=4)   :: ilun, io
    character(len=80) :: dummy
    logical           :: exists

    ! Check that the file exists
    inquire(file=fname, exist=exists)
    if (.not. exists) then
       write(*,*) "File "//trim(fname)//" does not exist"
       stop
    end if
    
    ! Read the file first to count the number of lines
    nlines = 0
    open(newunit=ilun, file=fname, status="old", action="read", iostat=io)
    do
       read(ilun, *, iostat=io) dummy
       if (io /= 0) exit
       nlines = nlines + 1
    end do
    close(ilun)

    nbits = len(trim(dummy))
  end subroutine count_lines

  subroutine find_mostcommon(array, n, val)
    integer(kind=4), intent(in)  :: n
    integer(kind=4), intent(in)  :: array(1:n)
    integer(kind=4), intent(out) :: val

    val = 0
    if (count(array .eq. 1) .ge. 0.5*n) val = 1    
  end subroutine find_mostcommon
  
  subroutine convert_array_to_integer(n, array, val)
    integer(kind=4), intent(in)  :: n
    integer(kind=4), intent(in)  :: array(1:n)
    integer(kind=4), intent(out) :: val
    character(len=n) :: line

    write(line,'(<n>I1)') array
    read(line,'(B<n>)') val
    
  end subroutine convert_array_to_integer

  subroutine find_life(n, nbits, array, most, val)
    integer(kind=4), intent(in)  :: n, nbits
    integer(kind=4), intent(in)  :: array(1:n, 1:nbits)
    logical,         intent(in)  :: most
    integer(kind=4), intent(out) :: val

    logical, allocatable :: mask1D(:), mask2D(:,:)
    integer(kind=4) :: bit, i, j, nok
    integer(kind=4), allocatable :: valsearch(:,:), valtmp(:,:)
    
    allocate(valsearch(1:n, 1:nbits))
    valsearch = array
    allocate(mask1D(1:n), mask2D(1:n, 1:nbits))
    nok = n
    
    do i=1, nbits
       call find_mostcommon(valsearch(:,i), nok, bit)
       if (.not. most) bit = modulo(bit-1, 2)
       
       mask1D = valsearch(:,i).eq.bit
       mask2D = spread(mask1D, 2, nbits)
       
       nok = count(mask1D)
       if(allocated(valtmp)) deallocate(valtmp)
       allocate(valtmp(1:nok, 1:nbits))

       ! Mask the array
       valtmp = reshape(pack(valsearch, mask2D), (/nok, nbits/))

       ! Array sie management
       deallocate(valsearch, mask2D, mask1D)
       allocate(mask1D(1:nok), valsearch(1:nok, 1:nbits), mask2D(1:nok, 1:nbits))
       valsearch=valtmp

       ! Break if we reach only 1 number
       if (nok .eq. 1) exit
    end do

    call convert_array_to_integer(nbits, valsearch, val)
    
    
  end subroutine find_life


end program diagnostic2
