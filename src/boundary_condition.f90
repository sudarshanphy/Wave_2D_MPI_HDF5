module boundary_condition
    ! if done in serial
    ! reflecting boundary condition, as boundary values are always 0
    !un(:,1) = 0
    !un(:, ny) = 0
    !un(1, :) = 0
    !un(nx, :) = 0
    
    ! absorbing boundary condition
    !unp1(1, :) = un(2, :) + ((cfl - 1)/(cfl + 1)) * (unp1(2, :) - un(1, :))
    !unp1(nx, :) = un(nx - 1, :) + ((cfl - 1)/(cfl + 1)) * (unp1(nx - 1, :) - un(nx, :))
    !unp1(:, 1) = un(:, 2) + ((cfl - 1)/(cfl + 1)) * (unp1(:, 2) - un(:, 1))
    !unp1(:, ny) = un(:, ny - 1) + ((cfl - 1)/(cfl + 1)) * (unp1(:, ny - 1) - un(:, ny))
    
    ! here we have modified it for local arrays of un and unp1
    ! this is how the boundaries are defined, and domain is decomposed for n processors:
    !    
    !                            xr_boundary
    !                ---------------------------------
    !                |       |       |      |        |
    !                | rank  | rank  |      |  rank  |
    !                |  0    |  1    | ...  |   n-1  |
    !                |       |       |      |        |
    !  yl_boundary   |       |       |      |        | yr_boundary
    !                |       |       |      |        |
    !                |       |       |      |        |
    !                |       |       |      |        |
    !                |       |       |      |        |
    !                |       |       |      |        |
    !                |       |       |      |        |
    !                ---------------------------------
    !                            xl_boundary
    !

contains

subroutine apply_boundary(my_rank, nx_local, un_local, unp1_local)

    use variable, only: ny, cfl, nprocs
    use variable, only: xl_boundary, xr_boundary, &
                        yl_boundary, yr_boundary 
    
    implicit none
    integer, intent(in) :: my_rank, nx_local
    double precision, dimension(nx_local, ny), intent(inout) :: un_local, unp1_local

    ! apply the boundary condition
    if (my_rank == 0) then
         ! left boundary for y axis is only in rank 0
         if (yl_boundary == 0) un_local(2, 1:ny) = 0.0   ! relect  
         if (yl_boundary == 1) unp1_local(2, 1:ny) = un_local(3, 1:ny) + (((cfl - 1)/(cfl + 1)) &
                                        * (unp1_local(3, 1:ny) - un_local(2, 1:ny))) ! absorb

    end if
    
    if (my_rank == nprocs - 1) then
        if (yr_boundary == 0) un_local(nx_local - 1, 1:ny) = 0.0
        if (yr_boundary == 1) unp1_local(nx_local - 1, 1:ny) = un_local(nx_local-2, 1:ny) +&
                                (((cfl - 1)/(cfl + 1)) * (unp1_local(nx_local - 2, 1:ny) - &
                                un_local(nx_local - 1, 1:ny)))
    end if
    
    if (xl_boundary == 0) un_local(2:nx_local-1, 1) = 0.0 ! reflect in xleft
    if (xl_boundary == 1) then
        unp1_local(2:nx_local-1, 1) = un_local(2:nx_local-1, 2) + ((cfl - 1)/(cfl + 1)) &
                                   * (unp1_local(2:nx_local-1, 2) - un_local(2:nx_local-1, 1)) ! absorb in xleft
     end if
    
    if (xr_boundary == 0) un_local(2:nx_local - 1, ny) = 0.0
    if (xr_boundary == 1) then
         unp1_local(2:nx_local - 1, ny) = un_local(2:nx_local-1, ny-1) +&
                                      ((cfl - 1)/(cfl + 1)) * (unp1_local(2:nx_local-1, ny-1) -&
                                      un_local(2:nx_local - 1, ny))
    end if

end subroutine
 
end module boundary_condition
