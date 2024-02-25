    module Grid
    use, intrinsic :: iso_fortran_env, only: dp=>real64
    implicit none

    type :: PercolationGrid2d
        integer, public :: size = 0
        real(dp), public :: probability = 0.5_dp
        logical, dimension(:,:), allocatable, public :: grid
    contains
        procedure, public :: init
        procedure, public :: fill
        procedure, public :: percolates
        procedure, private :: cluster_percolates
        procedure, private :: find_root
        procedure, private :: clusters_union
    end type PercolationGrid2d

    contains

    subroutine init(self)
        class (PercolationGrid2d), intent(inout) :: self

        allocate(self%grid(self%size, self%size))
    end subroutine init
    
    
    
    subroutine fill(self)
        class (PercolationGrid2d), intent(inout) :: self
        integer :: i, j
        real(dp) :: r
        ! fill grid with random values with probability of self%probability
        do i = 1, self%size
            do j = 1, self%size
                call RANDOM_NUMBER(r)
                self%grid(i,j) = (r < self%probability)
            end do
        end do
    end subroutine fill

    
    
    function percolates(self) result(res)
        class (PercolationGrid2d), intent(in) :: self
        logical :: res
        integer, dimension(self%size,self%size) :: clusters
        integer, dimension(self%size*self%size) :: parents
        integer, dimension(self%size*self%size) :: ranks
        integer :: i, j, i2, j2, c

        clusters = 0
        parents = 0
        ranks = 0
        c = 1

        do i = 1, self%size
            do j = 1, self%size
                if (self%grid(i,j)) then
                    i2 = i + 1
                    j2 = j
                    call self%clusters_union(parents, clusters, ranks, i, j, i2, j2, c)

                    i2 = i
                    j2 = j + 1
                    call self%clusters_union(parents, clusters, ranks, i, j, i2, j2, c)
                end if
            end do
        end do

        do i = 1, self%size
            do j = 1, self%size
                if (clusters(i,j) /= 0) then
                    clusters(i,j) = self%find_root(parents, clusters(i,j))
                end if
            end do
        end do

        res = self%cluster_percolates(clusters)
    end function percolates


    function cluster_percolates(self, clusters) result(res)
        class (PercolationGrid2d), intent(in) :: self
        integer, dimension(self%size,self%size), intent(in) :: clusters
        logical :: res
        integer :: i, j

        res = .false.
        outer: do i = 1, self%size
            if (clusters(1,i) /= 0) then
                do j = 1, self%size
                    if (clusters(1,i) == clusters(self%size,j)) then
                        res = .true.
                        exit outer
                    end if
                end do
            endif
        end do outer
        end function cluster_percolates


    function find_root(self, parents, c) result(res)
        class (PercolationGrid2d), intent(in) :: self
        integer, dimension(self%size*self%size), intent(inout) :: parents
        integer :: c, res

        do while (parents(c) /= c)
            parents(c) = parents(parents(c))
            c = parents(c)
        end do

        res = c
    end function find_root


    subroutine clusters_union(self, parents, clusters, ranks, i1, j1, i2, j2, c)
        class (PercolationGrid2d), intent(in) :: self
        integer, dimension(self%size*self%size), intent(inout) :: parents
        integer, dimension(self%size,self%size), intent(inout) :: clusters
        integer, dimension(self%size*self%size), intent(inout) :: ranks
        integer, intent(in) :: i1, j1, i2, j2
        integer, intent(inout) :: c
        integer :: c1, c2

        if (i2 < 1 .or. i2 > self%size .or. j2 < 1 .or. j2 > self%size) then
            return
        else if (.not. self%grid(i1,j1) .or. .not. self%grid(i2,j2)) then
            return
        end if

        c1 = clusters(i1,j1)
        c2 = clusters(i2,j2)
        if (c1 == 0 .and. c2 == 0) then
            clusters(i1,j1) = c
            clusters(i2,j2) = c
            parents(c) = c
            c = c + 1
        else if (c1 == 0 .and. c2 /= 0) then
            clusters(i1,j1) = c2
        else if (c1 /= 0 .and. c2 == 0) then
            clusters(i2,j2) = c1
        else
            c1 = self%find_root(parents, c1)
            c2 = self%find_root(parents, c2)
            if (c1 /= c2) then
                if (ranks(c1) < ranks(c2)) then
                    block
                    integer :: tmp
                    tmp = c1
                    c1 = c2
                    c2 = tmp
                    end block
                end if
                parents(c2) = c1
                if (ranks(c1) == ranks(c2)) then
                    ranks(c1) = ranks(c1) + 1
                end if
            end if
        end if
    end subroutine clusters_union

    end module Grid
