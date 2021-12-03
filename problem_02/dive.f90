program dive
  ! Puzzle 2:
  ! How far are you going, counting depth*distance
  !
  ! The input is expected to be formatted as a list of "COMMAND N"
  !   with COMMAND in [forward, down, up] and N a number
  ! The output should be depth*distance
  implicit none

  character(len=80) :: input_file
  character(len=8)  :: instruction
  integer(kind=4)   :: i, n
  integer(kind=4)   :: depth, distance
  integer(kind=4)   :: ilun, io, dummy
  logical           :: exists

  input_file = "input_02.dat"
  ! Check that the file exists
  inquire(file=input_file, exist=exists)
  if (.not. exists) then
     write(*,*) "File "//trim(input_file)//" does not exist"
     stop
  end if

  ! Initialize the distance and depth indicators
  depth = 0
  distance = 0

  ! Read the file first to count the number of lines
  open(newunit=ilun, file=input_file, status="old", action="read", iostat=io)
  do
     read(ilun, *, iostat=io) instruction, n
     if (io /= 0) exit

     if(trim(instruction) .eq. 'forward') distance = distance + n
     if(trim(instruction) .eq. 'down') depth = depth + n
     if(trim(instruction) .eq. 'up') depth = depth - n
     
  end do
  close(ilun)

  write(*,'(A20,I4,A1,I4,A1)') 'New position is (',distance,',',depth,')'
  write(*,*) 'The product is then ',distance*depth
end program dive
