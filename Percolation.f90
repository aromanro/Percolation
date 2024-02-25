!  Percolation.f
!
!  FUNCTIONS:
!  Percolation - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Percolation
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program Percolation
    use, intrinsic :: iso_fortran_env, only: dp=>real64
    use Stats
    implicit none

    ! Variables
    integer :: i, j, cnt
    real(dp) :: p, pr, p2, pg, errorBar
    type(statistics) :: percolationStatistics
    character(len=20) :: filename
    integer :: unit

    ! Body of Percolation
    call RANDOM_INIT(.false.,.true.)
    call RANDOM_SEED()
    
    percolationStatistics%grid_size = 2
    percolationStatistics%points = 100
    !percolationStatistics%grid_size = 10
    !percolationStatistics%lower = 0.58
    !percolationStatistics%upper = 0.60
    
    call percolationStatistics%init_stats
    
    call percolationStatistics%calculate
    
    filename = "percolation.dat"
    OPEN(NEWUNIT=unit, FILE=filename, STATUS="UNKNOWN", ACTION="WRITE")
    
    do i = 1, percolationStatistics%points
        p = percolationStatistics%grids(i)%probability    
        pr = percolationStatistics%values(i)
        
        ! percolation probability for a grid of size 2
        p2 = p * p
        p2 = p2 * (2.0 - p2)
        
        errorBar = percolationStatistics%approx_error(pr)
        !print *, "Probability: ", p, " Percolation probability: ", pr, " expected: ", p2, " approx error bar: ", errorBar, " diff: ", pr - p2
        write(unit,'(F,F,F,F)') p, pr, p2, errorBar !something like this for gnuplot
    end do

    end program Percolation

