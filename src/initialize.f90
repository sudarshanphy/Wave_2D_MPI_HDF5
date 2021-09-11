module initialize
! this module was created so that we could pass allocatable
! arrays that haven't been allocated yet.
! just using subroutines inside this file was giving
! memory error during run time.

contains 

subroutine init_grid(array1, n, L)
        ! this subroutine initializes the axis values
        ! takes a dynamical array array1, n and L as input
        ! returns the grid points for array1

        use variable, only: delta, i

        implicit none
        integer, intent(in) :: n
        double precision, intent(in) :: L 
        double precision, allocatable, dimension(:), intent(inout) :: array1
        
        ! allocate the dynamical array to store values
        allocate(array1(n))

        ! need to this to as we added 1 to nx earlier
        delta = L/(n - 1)

        ! loop over the array elemenst and add delta 
        do i = 1, n

                array1(i) = (i - 1) * delta      

        end do

end subroutine

subroutine zeros(nx_local, un_local, unm1_local, unp1_local, sn_local)
        ! this subroutine zeros all the elemts of a 2D array
        ! with dimesnion (nx_local, ny)

        use variable, only: ny !, un_local, unm1_local, unp1_local, sn_local
        implicit none

        integer, intent(in) :: nx_local
        double precision, dimension(nx_local, ny), intent(inout) :: un_local, unm1_local, unp1_local, sn_local

        un_local(1:nx_local, :) = 0.0
        unm1_local(1:nx_local, :) = 0.0
        unp1_local(1:nx_local, :) = 0.0
        sn_local(1:nx_local, :) = 0.0

end subroutine

end module initialize
