program sonar_sweep2
  ! Puzzle 1:
  ! How many measurements are larger than the previous measurement?
  !
  ! The input is expected to be formatted as a list of depths
  ! The output should be the number of times there is an increase
  implicit none

  character(len=80) :: input_file

  integer(kind=4)                 :: i, n
  integer(kind=4)                 :: num_increase
  integer(kind=4), allocatable    :: depth(:), gradient(:), window(:)

  input_file = "input_01.dat"
  
  call read_depth(input_file, depth, n)

  write(*,*) 'Input file '//trim(input_file)//' has ', n,' lines'

  allocate(window(1:n-2))
  call sliding_window(depth, window, n)

  ! Create the gradient array
  allocate(gradient(1:size(window)))
  gradient(1) = 0
  gradient(2:n) = window(2:n)-window(1:n-1)

  ! Count how many times the gradient is positive (i.e. depth increases)
  num_increase = count(gradient .gt. 0)

  write(*,*) 'There are ', num_increase, ' steps where the depth increases'



contains

  subroutine read_depth(fname, array, n)
    character(len=80), intent(in)               :: fname
    integer(kind=4), allocatable, intent(inout) :: array(:)
    integer(kind=4), intent(out)                :: n

    integer(kind=4) :: ilun, io, dummy
    logical         :: exists

    ! Check that the file exists
    inquire(file=fname, exist=exists)
    if (.not. exists) then
       write(*,*) "File "//trim(fname)//" does not exist"
       stop
    end if
    
    ! Read the file first to count the number of lines
    n = 0
    open(newunit=ilun, file=fname, status="old", action="read", iostat=io)
    do
       read(ilun, *, iostat=io) dummy
       if (io /= 0) exit
       n = n + 1
    end do
    close(ilun)

    ! Now allocate the array and read it
    allocate(array(1:n))
    open(newunit=ilun, file=fname, status="old", action="read", iostat=io)
    do i=1, n
       read(ilun, *, iostat=io) array(i)
    end do
    close(ilun)
  end subroutine read_depth


  subroutine sliding_window(depth, window, n)
    integer(kind=4), intent(in) :: n
    integer(kind=4), intent(in) :: depth(n)
    integer(kind=4), intent(out):: window(n-2)
    integer(kind=4) :: i

    window(:) = 0
    window(:) = depth(1:n-2) + depth(2:n-1) + depth(3:n)

  end subroutine sliding_window

end program sonar_sweep2
