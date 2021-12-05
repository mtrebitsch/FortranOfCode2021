program vent2
  ! Puzzle 5: Hydrothermal vents
  ! We have a series of lines given by coordinates "x1,y1 -> x2,y2"
  ! Each line contains the end-points
  ! We want to know how many points have at least 2 lines overlapping
  ! There us probably a math-savvy approach, but…
  ! we can also just draw the lines.
  ! For part 2, we care about diagonal lines too

  implicit none

  character(len=80) :: input_file
  character(len=80)  :: line
  integer(kind=4)   :: ilun, io, dummy
  integer(kind=4)   :: i,j, nx, ny, danger
  integer(kind=4), allocatable :: map(:,:)

  input_file = "input_05.dat"

  call init_map(input_file, map)

  call read_map(input_file, map)

  ! write(*,'(A30)'), 'The filled map looks like this:'
  ! call print_map(map)

  danger = count(map .ge. 2)
  write(*,'(A30,I6)'), 'Number of danger zones:', danger



contains

  subroutine init_map(fname, map)
    character(len=80), intent(in) :: fname
    integer(kind=4), allocatable, intent(out)  :: map(:,:)
    character(len=80)  :: line
    integer(kind=4)    :: ilun, io
    integer(kind=4)    :: i, xmax, ymax, x1, x2, y1, y2
    logical            :: exists
    
    ! Check that the file exists
    inquire(file=fname, exist=exists)
    if (.not. exists) then
       write(*,*) "File "//trim(fname)//" does not exist"
       stop
    end if

    xmax = 0
    ymax = 0
    open(newunit=ilun, file=fname, status="old", action="read", iostat=io)
    do
       read(ilun, '(A)', iostat=io) line
       call parse_line(line, x1, x2, y1, y2)
       if (x2 .gt. xmax) xmax = x2
       if (y2 .gt. ymax) ymax = y2
       if (io /= 0) exit    
    end do
    close(ilun)

    allocate(map(0:xmax,0:ymax))
    map = 0
  end subroutine init_map

  subroutine read_map(fname, map)
    character(len=80), intent(in)  :: fname
    integer(kind=4), intent(inout) :: map(0:,0:)
    character(len=80)  :: line
    integer(kind=4)    :: ilun, io
    integer(kind=4)    :: i, j, xmax, ymax, x1, x2, y1, y2
    integer(kind=4)    :: xstart, xend, ystart, yend, dx, dy
    logical            :: exists
    
    open(newunit=ilun, file=fname, status="old", action="read", iostat=io)
    do
       read(ilun, '(A)', iostat=io) line
       if (io /= 0) exit    
       call parse_line(line, x1, y1, x2, y2)
       
       ! Deal with horizontal/vertical lines
       if ((x1 .eq. x2) .or. (y1 .eq. y2)) then
          xstart = min(x1, x2)
          ystart = min(y1, y2)
          xend = max(x1, x2)
          yend = max(y1, y2)
          map(xstart:xend,ystart:yend) = map(xstart:xend,ystart:yend) + 1
       end if
       
       ! Deal with diagonal lines
       if (abs(x2-x1) .eq. abs(y2-y1)) then
          xstart = x1
          ystart = y1
          xend = x2
          yend = y2
          if (x1.gt.x2) then
             xstart = x2
             ystart = y2
             xend = x1
             yend = y1
          end if

          dy = 1
          if (yend.lt.ystart) dy=-1
          
          do i=0, xend-xstart
             map(xstart+i,ystart+dy*i) = map(xstart+i,ystart+dy*i) + 1
          end do
       end if

    end do
    close(ilun)
  end subroutine read_map

  subroutine print_map(map)
    integer(kind=4), intent(in) :: map(0:,0:)
    integer(kind=4) :: i, j, nx, ny

    nx = size(map, 1)
    ny = size(map, 2)
    do j=0,ny-1
       do i=0,nx-1
          if (map(i,j) .gt. 0) then
             write(*,'(I1)', advance="no") map(i,j)
          else
             write(*,'(A)', advance="no") '.'
          end if
       end do
       write(*,*) ''
    end do

end subroutine print_map

  
  subroutine parse_line(line, a, b, c, d)
    character(len=80), intent(in) :: line
    integer(kind=4), intent(out)  :: a, b, c, d
    integer(kind=4) :: sep
    sep = index(line, ' -> ')
    read(line(1:sep), '(2I)') a, b
    read(line(sep+4:),'(2I)') c, d
    
  end subroutine parse_line
  

end program vent2
