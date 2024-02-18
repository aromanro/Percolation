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
    use Stats
    implicit none

    ! Variables
    integer :: i, j, cnt
    real :: p, pr, p2, pg
    type(statistics) :: percolationStatistics

    ! Body of Percolation
    call RANDOM_INIT(.false.,.true.)
    call RANDOM_SEED()
    
    percolationStatistics%grid_size = 2
    percolationStatistics%points = 20
    !percolationStatistics%grid_size = 10
    !percolationStatistics%lower = 0.55
    !percolationStatistics%upper = 0.65
    
    call percolationStatistics%init_stats
    
    call percolationStatistics%calculate
    
    do i = 1, percolationStatistics%points
        p = percolationStatistics%grids(i)%probability    
        pr = percolationStatistics%values(i)
        
        p2 = p * p
        p2 = p2 * (2.0 - p2)
        print *, p, " ", pr, " expected: ", p2
    end do

    end program Percolation

