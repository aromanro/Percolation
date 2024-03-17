    module Stats    
    use, intrinsic :: iso_fortran_env, only: dp=>real64
    !use omp_lib
    use Grid    
    implicit none    
    
    private    ! hide the type-bound procedure implementation procedures
    public statistics    ! make the type public
    
    type :: statistics
        integer, public :: points = 10
        integer, public :: simulations = 10000
        integer, public :: grid_size = 2
        real(dp), public :: lower = 0.0_dp
        real(dp), public :: upper = 1.0_dp
        real(dp), dimension(:), allocatable, public :: values
        class (PercolationGrid2d), dimension(:), allocatable, public :: grids
    contains
        procedure, public :: init
        final :: destructor
        procedure, public :: calculate
        procedure, public :: approx_error
    end type statistics    
        
    contains   
    
    subroutine init(self)
        class (statistics), intent(inout) :: self
        integer i

        if (allocated(self%values)) deallocate(self%values)
        allocate(self%values(self%points))
        
        if (allocated(self%grids)) deallocate(self%grids)
        allocate(self%grids(self%points))
        
        do i = 1, self%points
            self%values(i) = 0.0_dp
            self%grids(i)%size = self%grid_size
            self%grids(i)%probability = self%lower + real(i-1, dp) * (self%upper - self%lower) / real(self%points-1, dp)
            !call self%grids(i)%init
        end do
    end subroutine init   
    
    subroutine destructor(self)
        type (statistics), intent(inout) :: self
        
        if (allocated(self%values)) deallocate(self%values)
        if (allocated(self%grids)) deallocate(self%grids)
    end subroutine destructor
    
    
    subroutine calculate(self)
        class (statistics), intent(inout) :: self
        integer i
        
        !!$omp parallel do private(i) shared(self) schedule(static)
        do i = 1, self%points
            block
            integer cnt, j
            
            call self%grids(i)%init
            cnt = 0
            do j = 1, self%simulations
                call self%grids(i)%fill
                if (self%grids(i)%percolates()) then
                    cnt = cnt + 1
                end if
            end do
            deallocate(self%grids(i)%grid)
          
            self%values(i) = real(cnt, dp) / real(self%simulations, dp)
            end block
        end do
        !!$omp end parallel do
        
    end subroutine calculate
    
    function approx_error(self, val) result(err)
        class (statistics), intent(in) :: self
        real(dp), intent(in) :: val
        real(dp) :: err
        
        err = sqrt(val * (1.0_dp - val) / real(self%simulations, dp))
    end function approx_error
    
    end module Stats    