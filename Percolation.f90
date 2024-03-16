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
    use Calc
    implicit none    

    ! Variables
    integer :: i, j, cnt
    real(dp) :: p, pr, p2, pg, errorBar
    type(statistics) :: percolationStatistics2
    character(len=20) :: filename
    integer :: unit

    ! Body of Percolation
    call RANDOM_INIT(.false.,.true.)
    call RANDOM_SEED()
    
    percolationStatistics2%grid_size = 2
    percolationStatistics2%points = 100
    percolationStatistics2%simulations = 5000
    
    call percolationStatistics2%init_stats
    call percolationStatistics2%calculate
    
    filename = "data2.dat"
    OPEN(NEWUNIT=unit, FILE=filename, STATUS="UNKNOWN", ACTION="WRITE")
    
    do i = 1, percolationStatistics2%points
        p = percolationStatistics2%grids(i)%probability    
        pr = percolationStatistics2%values(i)
        
        ! percolation probability for a grid of size 2
        p2 = p * p
        p2 = p2 * (2.0 - p2)
        
        errorBar = percolationStatistics2%approx_error(pr)
        !print *, "Probability: ", p, " Percolation probability: ", pr, " expected: ", p2, " approx error bar: ", errorBar, " diff: ", pr - p2
        write(unit,'(F,F,F,F)') p, pr, p2, 2.0_dp * errorBar !something like this for gnuplot
    end do
    CLOSE(unit)
    
    print *, "Computing percolation for grid size 10..."
    call calculate_stats("percolation.dat", 10, 100, 3000, 0.55_dp, 0.65_dp) 
    print *, "Computing percolation for grid size 40..."
    call calculate_stats("percolation.dat", 40, 100, 3000, 0.55_dp, 0.65_dp) 
    print *, "Computing percolation for grid size 160..."
    call calculate_stats("percolation.dat", 160, 100, 3000, 0.55_dp, 0.65_dp)
    
    print *, "Computing percolation for grid size 640..."
    call calculate_stats("percolation.dat", 640, 100, 3000, 0.55_dp, 0.65_dp)

    end program Percolation

