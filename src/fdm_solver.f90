subroutine fdm_solver(dt, my_rank, nx_local, un_local, unm1_local, unp1_local, sn_local)
    ! finite difference method solver
    use variable, only: cfl, ny, nprocs
    
    implicit none
    double precision, intent(in) :: dt
    integer, intent(in) :: nx_local, my_rank
    double precision, dimension(:,:), intent(in) :: sn_local(nx_local, ny), un_local(nx_local, ny), &
                                                    unm1_local(nx_local, ny)
    double precision, dimension(:,:), intent(inout) :: unp1_local(nx_local, ny)
    integer :: j, i, ilow, ihigh

    ! calculate the values in the inner grid points at t(n+1)
    ilow = 2
    ihigh = nx_local - 1
    if (my_rank == 0) ilow = 3
    if (my_rank == nprocs - 1) ihigh = nx_local - 2
    
    do j = 2, ny-1
        do i = ilow, ihigh
        
            unp1_local(i,j) = 2 * un_local(i, j) - unm1_local(i,j) +&
            & ((cfl**2) * (un_local(i+1,j) + un_local(i-1,j) -&
            & (4 * un_local(i, j)) + un_local(i, j+1) + un_local(i, j-1))) +&
            & dt**2 * sn_local(i,j) 
           
            ! if (abs(unp1_local(i,j)) <= 1e-30) unp1_local(i,j) = 0.0
        end do
    end do

end subroutine fdm_solver

