program diagnostic
  ! Puzzle 3:
  ! Find the power consumption, as "gamma*epsilon"
  ! Where gamma and epsilon are two binary numbers taken from the "log"
  ! - Each bit of gamma is given by the most common value of each bit in the log
  ! - Each bit of epsilon is the least common value of each bit
  ! Note that gamma = NOT epsilon

  implicit none

  character(len=80) :: input_file
  character(len=80)  :: line
  integer(kind=4)   :: ilun, io, dummy
  logical           :: exists
  integer(kind=4)   :: i, n, nbits
  integer(kind=4), allocatable :: logfile(:,:), count_ones(:)
  logical, allocatable :: onemax(:)
  integer(kind=4)   :: gamma, epsilon

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

  count_ones = count(logfile(:,:) .eq. 1, 1)
  ! this assumes that 0 has priority over 1 in case of equality
  onemax = count_ones .gt. n*0.5 
  ! convert the array of bools into a binary number
  ! TODO: this is a bit ugly
  write(line,'(<nbits>I1)') merge(1, 0, onemax)
  read(line,'(B<nbits>)') gamma
  write(line,'(<nbits>I1)') merge(0, 1, onemax)
  read(line,'(B<nbits>)') epsilon

  write(*,'(A20, I8)') 'Gamma rate:', gamma
  write(*,'(A20, I8)') 'Epsilon rate:', epsilon
  write(*,'(A20, I8)') 'Consumption:', gamma*epsilon


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


end program diagnostic
