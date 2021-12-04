program bingo
  ! Puzzle 4: Bingo!
  ! Given a series of digits and a set of 5x5 boards, find the final score.
  ! It is given by sum(unmarked)*sum(marked) on the winning board
  ! A winning board has a winning column or row
  ! In Bingo, numbers are assumed to below 100

  implicit none

  character(len=80) :: input_file
  character(len=80)  :: line
  integer(kind=4)   :: ilun, io, dummy
  integer(kind=4)   :: i,j, d, ndigits, nsets, pos(2), winpos(1), win
  integer(kind=4)   :: uncalled, lastnum, score
  integer(kind=4), allocatable :: numbers(:), sets(:,:,:)
  logical, allocatable :: marks(:,:,:), ok(:)

  input_file = "input_04.dat"

  call get_digits(input_file, numbers, ndigits)

  call read_sets(input_file, sets)

  ! Now, main logic
  ! First, keep an array of "marks"
  nsets = size(sets,1)
  allocate(marks(nsets, 5, 5), ok(1:nsets))
  marks = .false.

  ! Now, for each digit, mark it the mark array
  do d=1,ndigits
     do i=1, nsets
        pos = findloc(sets(i,:,:), numbers(d))
        if (pos(1) /= 0 .and. pos(2) /= 0) then
           marks(i,pos(1),pos(2)) = .true.
        end if
        ok(i) = is_bingo(marks(i,:,:))
     end do
     if(any(ok)) then
        winpos = findloc(ok, .true.)
        exit
     end if
  end do

  win = winpos(1)
  write(*,'(A4,I2,A8)') 'Set', win, 'has won'

  uncalled = sum(sets(win,:,:), mask=.not.marks(win,:,:))
  lastnum = numbers(d)
  score = uncalled*lastnum
  write(*,'(A30,I6)'), 'Sum of uncalled numbers:', uncalled
  write(*,'(A30,I6)'), 'Last called digit:', lastnum
  write(*,'(A30,I6)'), 'Final score:', score

contains

  subroutine get_digits(fname, numbers, n)
    character(len=80), intent(in) :: fname
    integer(kind=4), allocatable, intent(out)  :: numbers(:)
    integer(kind=4), intent(out)  :: n
    character(len=500) :: line  ! 100 2-digits numbers at most + 99 commas < 500
    integer(kind=4)    :: ilun
    integer(kind=4)    :: i
    logical            :: exists
    
    ! Check that the file exists
    inquire(file=fname, exist=exists)
    if (.not. exists) then
       write(*,*) "File "//trim(fname)//" does not exist"
       stop
    end if

    ! We assume that the first line is the set of digits
    open(newunit=ilun, file=fname, status="old", action="read", iostat=io)
    read(ilun, '(a)') line
    close(ilun)

    ! Count number of comma to know how many digits
    n=1
    do i=1,len(trim(line))
       if (line(i:i) .eq. ',') n = n+1
    end do

    ! Split the line and read the digits
    allocate(numbers(1:n))
    read(line, '(<n>I)') numbers

    
  end subroutine get_digits
  
  subroutine read_sets(fname, sets)
    character(len=80), intent(in) :: fname
    integer(kind=4), allocatable, intent(out)  :: sets(:,:,:)
    integer(kind=4)   :: ilun, io, i, j, nsets
    integer(kind=4)   :: set(5,5), set1, set2, set3, set4, set5
    character(len=80) :: dummy
    logical           :: exists
    
    ! Read the file first to count the number of sets
    open(newunit=ilun, file=fname, status="old", action="read", iostat=io)
    read(ilun, '(a)', iostat=io)  ! First line
    read(ilun, '(a)')
    nsets = 0
    do
       nsets = nsets + 1
       call read_single_set(ilun, set, io)
       read(ilun, '(a)', iostat=io)
       if (io /= 0) exit
    end do
    close(ilun)
    write(*,'(A16,I3)') 'Number of sets:', nsets

    ! Allocate the set array
    allocate(sets(nsets, 5,5))

    ! Repeat the reading
    open(newunit=ilun, file=fname, status="old", action="read", iostat=io)
    read(ilun, '(a)', iostat=io)  ! First line
    read(ilun, '(a)')
    do i=1,nsets
       call read_single_set(ilun, sets(i,:,:), io)
       read(ilun, '(a)', iostat=io)
    end do
    close(ilun)
    write(*,*) 'Read all sets'

  end subroutine read_sets

  subroutine read_single_set(ilun, set, io)
    integer(kind=4), intent(in) :: ilun
    integer(kind=4), intent(out):: set(5,5), io
    integer(kind=4) :: i

    do i=1,5
       read(ilun, '(5(I2,X))', iostat=io) set(i,:)
    end do

  end subroutine read_single_set

  function is_bingo(array)
    logical :: is_bingo, array(5,5)

    is_bingo = .false.
    if (any(all(array, 1)) .or. any(all(array,2))) is_bingo = .true.
    
  end function is_bingo

end program bingo
