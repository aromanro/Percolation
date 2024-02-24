    module Stats
    use, intrinsic :: iso_fortran_env, only: dp=>real64
    use Grid    
    implicit none
    
    type :: statistics
        integer, public :: points = 10
        integer, public :: simulations = 100000
        integer, public :: grid_size = 2
        real(dp), public :: lower = 0.0_dp
        real(dp), public :: upper = 1.0_dp
        real(dp), dimension(:), allocatable, public :: values
        class (PercolationGrid2d), dimension(:), allocatable, public :: grids
    contains
        procedure, public :: init_stats
        procedure, public :: calculate
    end type statistics    
        
    contains   
    
    subroutine init_stats(self)
        class (statistics), intent(inout) :: self
        integer i

        allocate(self%values(self%points))
        
        allocate(self%grids(self%points))
        
        do i = 1, self%points
            self%values(i) = 0.0_dp
            self%grids(i)%size = self%grid_size
            self%grids(i)%probability = self%lower + real(i-1, dp) * (self%upper - self%lower) / real(self%points-1, dp)
            call self%grids(i)%init
        end do
    end subroutine init_stats    
    
    
    subroutine calculate(self)
        class (statistics), intent(inout) :: self
        integer i
        
        !!$omp parallel do
        do i = 1, self%points
            block
            integer cnt, j
            
            cnt = 0
            do j = 1, self%simulations
                call self%grids(i)%fill
                if (self%grids(i)%percolates()) then
                    cnt = cnt + 1
                end if
            end do
          
            self%values(i) = real(cnt, dp) / real(self%simulations, dp)
            end block
        end do
        !!$omp end parallel do
        
    end subroutine calculate
    
    end module Stats    