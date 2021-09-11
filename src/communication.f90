! this subroutine carries out all teh mpi communications 

subroutine communicate_all(my_rank, nx_local, un_local, unm1_local, unp1_local, sn_local)
    use variable, only: ny!, un_local, unm1_local, unp1_local, sn_local
    integer, intent(in) :: my_rank, nx_local
    double precision, dimension(nx_local, ny), intent(inout) :: un_local, unm1_local, unp1_local, sn_local
    
    call communicate(my_rank, nx_local, un_local)
    call communicate(my_rank, nx_local, unm1_local)
    call communicate(my_rank, nx_local, unp1_local)
    call communicate(my_rank, nx_local, sn_local)
        
    ! update the values
    unm1_local(:, :) = un_local(:, :)
    un_local(:, :) = unp1_local(:, :)
        
end subroutine

subroutine communicate(my_rank, nx_local, array_local)
    use variable, only: nprocs, ierr, comm, ny
    use mpi
    implicit none
    
    integer, intent(in) :: my_rank, nx_local
    double precision, dimension(nx_local, ny), intent(inout) :: array_local
    integer, dimension(MPI_STATUS_SIZE) :: status1
    
    ! do right communication
    if (my_rank < nprocs - 1) then
        call MPI_SEND(array_local(nx_local-1, 1:ny), ny, MPI_DOUBLE_PRECISION, &
                      my_rank + 1, 0, comm, ierr)
        call MPI_RECV(array_local(nx_local, 1:ny), ny, MPI_DOUBLE_PRECISION, &
                      my_rank + 1, 1, comm, status1, ierr)
    end if
    
    ! do left communication
    if (0 < my_rank) then
        call MPI_SEND(array_local(2, 1:ny), ny, MPI_DOUBLE_PRECISION, &
                      my_rank - 1 , 1, comm, ierr)
        call MPI_RECV(array_local(1, 1:ny), ny, MPI_DOUBLE_PRECISION, &
                      my_rank - 1, 0, comm, status1, ierr)
    end if

    ! call MPI_BARRIER(comm, ierr)

end subroutine

subroutine gather_at_zero(nx_local, unp1_local)
    ! gather from all the processors to processor 0
    use variable, only: nprocs, ierr, comm, nx_per_core, unp1_global, ny
    use mpi
    implicit none

    integer :: source, a, b
    integer, intent(in) :: nx_local
    double precision, dimension(nx_local, ny), intent(in) :: unp1_local
    integer, dimension(MPI_STATUS_SIZE) :: status1

    unp1_global(1:nx_per_core(1),:) = unp1_local(2:nx_local - 1, :)
    if (nprocs > 1) then
        do source = 1, nprocs - 1
            a = sum(nx_per_core(1:(source+1))) - nx_per_core(source+1) + 1 
            b = sum(nx_per_core(1:(source+1)))
            call MPI_RECV(unp1_global(a:b, :), (b - a + 1)*ny, MPI_DOUBLE_PRECISION, &
                          source, 2, comm, status1, ierr)
        end do
    end if

end subroutine

subroutine send_to_zero(nx_local, unp1_local)
    ! send from different processors to processor 0
    use variable, only: ierr, comm, ny
    use mpi
    implicit none

    integer, intent(in) :: nx_local
    integer, parameter :: dest = 0
    double precision, dimension(nx_local, ny), intent(in):: unp1_local

    call MPI_SEND(unp1_local(2:nx_local - 1, :), (nx_local-2)*ny, &
                  MPI_DOUBLE_PRECISION, dest, 2, comm, ierr)

end subroutine
