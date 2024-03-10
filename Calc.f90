    module Calc
    use, intrinsic :: iso_fortran_env, only: dp=>real64
    use Stats
    implicit none

    private
    public calculate_stats
    
    contains
    
    subroutine calculate_stats(filename, grid_size, points, simulations, lower, upper)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: grid_size, points, simulations
        real(dp), intent(in) :: lower, upper
        type(statistics) :: percolationStatistics
        integer :: unit, i
        real(dp) :: p, pr, errorBar
        logical :: exists
    
        percolationStatistics%grid_size = grid_size
        percolationStatistics%points = points
        percolationStatistics%simulations = simulations
        percolationStatistics%lower = lower
        percolationStatistics%upper = upper
        
        call percolationStatistics%init_stats
        call percolationStatistics%calculate
        
        inquire(FILE=filename, EXIST=exists)
        if (exists) then
            OPEN(NEWUNIT=unit, FILE=filename, STATUS="OLD", POSITION="APPEND", ACTION="WRITE")
        else
            OPEN(NEWUNIT=unit, FILE=filename, STATUS="NEW", ACTION="WRITE")
        end if
    
        do i = 1, percolationStatistics%points
            p = percolationStatistics%grids(i)%probability    
            pr = percolationStatistics%values(i)
        
            errorBar = percolationStatistics%approx_error(pr)
            write(unit,'(F,F,F,F)') p, pr, 2.0_dp * errorBar !something like this for gnuplot
        end do    

        write(unit,*) ""
        write(unit,*) ""
        CLOSE(unit)
    end subroutine calculate_stats
    
    end module Calc