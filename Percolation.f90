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
    use Grid
    implicit none

    ! Variables
    integer :: i, j, cnt
    real :: p, pr, p2
    type(PercolationGrid2D) :: percolationGrid

    ! Body of Percolation
    call RANDOM_INIT(.false.,.true.)
    call RANDOM_SEED()
    
    percolationGrid%size = 2
    call percolationGrid%init
    
    do p = 0.0, 1.0, 0.05
        
        cnt = 0
        percolationGrid%probability = p
        
        do i = 1, 100000
            call percolationGrid%fill
            if (percolationGrid%percolates()) then
                cnt = cnt + 1
            end if
        end do
        
        p2 = p * p
        p2 = p2 * (2.0 - p2)
        pr = real(cnt) / 100000.0
        
        print *, p, " ", pr, " expected: ", p2
    end do

    end program Percolation

