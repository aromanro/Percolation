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
    real(dp) :: p, pr, p2, pg
    type(statistics) :: percolationStatistics

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
    
    do i = 1, percolationStatistics%points
        p = percolationStatistics%grids(i)%probability    
        pr = percolationStatistics%values(i)
        
        p2 = p * p
        p2 = p2 * (2.0 - p2)
        print *, "Probability: ", p, " Percolation probability: ", pr, " expected: ", p2, " approx error bar: ", percolationStatistics%approx_error(pr), " diff: ", pr - p2
    end do

    end program Percolation

